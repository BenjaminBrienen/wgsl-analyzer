import { spawn } from "node:child_process";
import process from "node:process";
import { text } from "node:stream/consumers";
import * as vscode from "vscode";
import type * as lc from "vscode-languageclient/node";
import { bootstrap } from "./bootstrap.ts";
import { createClient } from "./client.ts";
import { Config, prepareVsCodeConfig } from "./config.ts";
import type { ServerStatusParameters } from "./lsp_ext.ts";
import * as wa from "./lsp_ext.ts";
import type { WgslAnalyzerExtensionApi } from "./main.ts";
import { PersistentState } from "./persistent_state.ts";
import { type SyntaxElement, SyntaxTreeProvider } from "./syntax_tree_provider.ts";
import {
	isWeslDocument,
	isWeslEditor,
	isWeslTomlEditor,
	LazyOutputChannel,
	log,
	type WeslEditor,
} from "./utilities.ts";

// We only support local folders, not eg. Live Share (`vlsl:` scheme), so do not activate if
// only those are in use. We use "Empty" to represent these scenarios.
// (w-a still somewhat works with Live Share, because commands are tunneled to the host)

export type Workspace =
	| { kind: "Empty" }
	| { kind: "Workspace Folder" }
	| { kind: "Detached Files"; files: vscode.TextDocument[] };

export function fetchWorkspace(): Workspace {
	const folders = (vscode.workspace.workspaceFolders || []).filter(
		(folder) => folder.uri.scheme === "file",
	);
	const weslDocuments = vscode.workspace.textDocuments.filter((document) =>
		isWeslDocument(document),
	);
	if (folders.length > 0) {
		return { kind: "Workspace Folder" };
	}
	if (weslDocuments.length > 0) {
		return { kind: "Detached Files", files: weslDocuments };
	}
	return { kind: "Empty" };
}

export type CommandFactory = {
	enabled: (context: InitializedContext) => Cmd;
	disabled?: (context: Context) => Cmd;
};

export type InitializedContext = Context & {
	readonly client: lc.LanguageClient;
};

export class Context implements WgslAnalyzerExtensionApi {
	public readonly statusBar: vscode.StatusBarItem;
	public readonly config: Config;
	public readonly workspace: Workspace;
	public readonly version: string;

	private _client: lc.LanguageClient | undefined;
	private _serverPath: string | undefined;
	private traceOutputChannel: vscode.LogOutputChannel | undefined;
	private readonly testController: vscode.TestController | undefined;
	private outputChannel: vscode.LogOutputChannel | undefined;
	private clientSubscriptions: Disposable[];
	private readonly state: PersistentState;
	private readonly commandFactories: Record<string, CommandFactory>;
	private commandDisposables: Disposable[];
	private readonly unlinkedFiles: vscode.Uri[];
	private _syntaxTreeProvider: SyntaxTreeProvider | undefined;
	private _syntaxTreeView: vscode.TreeView<SyntaxElement> | undefined;
	private lastStatus: ServerStatusParameters | { health: "stopped" } = {
		health: "stopped",
	};
	private _serverVersion: string;
	private readonly statusBarActiveEditorListener: Disposable;

	public get serverPath(): string | undefined {
		return this._serverPath;
	}

	public get serverVersion(): string | undefined {
		return this._serverVersion;
	}

	public get client() {
		return this._client;
	}

	public get syntaxTreeView() {
		return this._syntaxTreeView;
	}

	public get syntaxTreeProvider() {
		return this._syntaxTreeProvider;
	}

	public constructor(
		// biome-ignore lint/style/noParameterProperties: TODO
		public readonly extCtx: vscode.ExtensionContext,
		commandFactories: Record<string, CommandFactory>,
		workspace: Workspace,
	) {
		extCtx.subscriptions.push(this);
		this.version = extCtx.extension.packageJSON.version ?? "<unknown>";
		this._serverVersion = "<not running>";
		this.config = new Config(extCtx.subscriptions);
		this.statusBar = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left);
		this.updateStatusBarVisibility(vscode.window.activeTextEditor);
		this.statusBarActiveEditorListener = vscode.window.onDidChangeActiveTextEditor((editor) => {
			this.updateStatusBarVisibility(editor);
		});
		this.workspace = workspace;
		this.clientSubscriptions = [];
		this.commandDisposables = [];
		this.commandFactories = commandFactories;
		this.unlinkedFiles = [];
		this.state = new PersistentState(extCtx.globalState);

		this.updateCommands("disable");
		this.setServerStatus({
			health: "stopped",
		});
	}

	public dispose() {
		this.config.dispose();
		this.statusBar.dispose();
		this.statusBarActiveEditorListener.dispose();
		this.testController?.dispose();
		void this.disposeClient();
		for (const disposable of this.commandDisposables) {
			disposable.dispose();
		}
	}

	public async onWorkspaceFolderChanges() {
		const workspace = fetchWorkspace();
		if (
			workspace.kind === "Detached Files"
			&& this.workspace.kind === "Detached Files"
			&& workspace.files !== this.workspace.files
		) {
			if (this.client?.isRunning()) {
				// Ideally we would not need to tear down the server here, but currently detached files
				// are only specified at server start
				await this.stopAndDispose();
				await this.start();
			}
			return;
		}
		if (workspace.kind === "Workspace Folder" && this.workspace.kind === "Workspace Folder") {
			return;
		}
		if (workspace.kind === "Empty") {
			await this.stopAndDispose();
			return;
		}
		if (this.client?.isRunning()) {
			await this.restart();
		}
	}

	private async getOrCreateClient() {
		if (this.workspace.kind === "Empty") {
			return;
		}

		if (!this._client) {
			this._serverPath = await this.bootstrap();
			text(spawn(this._serverPath, ["--version"]).stdout.setEncoding("utf-8")).then(
				(data) => {
					const prefix = "wgsl-analyzer ";
					this._serverVersion = data.slice(data.startsWith(prefix) ? prefix.length : 0).trim();
					this.refreshServerStatus();
				},
				(exception: unknown) => {
					log.error("Failed to get language server version", exception);
					this._serverVersion = "<unknown>";
					this.refreshServerStatus();
				},
			);
			const newEnv = { ...process.env, ...this.config.serverExtraEnv };
			const run: lc.Executable = {
				command: this._serverPath,
				options: { env: newEnv },
			};
			const serverOptions = {
				run,
				debug: run,
			};

			let rawInitializationOptions = vscode.workspace.getConfiguration("wgsl-analyzer");

			if (this.workspace.kind === "Detached Files") {
				rawInitializationOptions = {
					detachedFiles: this.workspace.files.map((file) => file.uri.fsPath),
					...rawInitializationOptions,
				};
			}

			const initializationOptions = prepareVsCodeConfig(rawInitializationOptions);

			this._client = createClient(
				this.getTraceOutputChannel(),
				this.getOutputChannel(),
				initializationOptions,
				serverOptions,
				this.config,
				this.unlinkedFiles,
			);
			this.pushClientCleanup(
				this._client.onNotification(wa.serverStatus, (parameters) => {
					this.setServerStatus(parameters);
				}),
			);
			this.pushClientCleanup(
				this._client.onNotification(wa.openServerLogs, () => {
					this.getOutputChannel().show();
				}),
			);
		}
		return this._client;
	}

	private getOutputChannel(): vscode.LogOutputChannel {
		if (!this.outputChannel) {
			this.outputChannel = vscode.window.createOutputChannel("wgsl-analyzer Language Server", {
				log: true,
			});
			this.pushExtCleanup(this.outputChannel);
		}
		return this.outputChannel;
	}

	private getTraceOutputChannel(): vscode.LogOutputChannel {
		if (!this.traceOutputChannel) {
			this.traceOutputChannel = new LazyOutputChannel("wgsl-analyzer Language Server Trace");
			this.pushExtCleanup(this.traceOutputChannel);
		}
		return this.traceOutputChannel;
	}

	private bootstrap(): Promise<string> {
		return bootstrap(this.extCtx, this.config, this.state).catch((exception: unknown) => {
			let message = "bootstrap error. ";

			message += 'See the logs in "OUTPUT > wgsl-analyzer Client" (should open automatically). ';
			message +=
				'To enable verbose logs, click the gear icon in the "OUTPUT" tab and select "Debug".';

			log.error("Bootstrap error", exception);
			throw new Error(message);
		});
	}

	public async start() {
		log.info("Starting language client");
		const client = await this.getOrCreateClient();
		if (!client) {
			return;
		}
		await client.start();
		this.updateCommands();
		if (this.config.showSyntaxTree) {
			this.prepareSyntaxTreeView(client);
		}
	}

	private prepareSyntaxTreeView(client: lc.LanguageClient) {
		const ctxInit: InitializedContext = { ...this, client };
		this._syntaxTreeProvider = new SyntaxTreeProvider(ctxInit);
		this._syntaxTreeView = vscode.window.createTreeView("weslSyntaxTree", {
			treeDataProvider: this._syntaxTreeProvider,
			showCollapseAll: true,
		});

		this.pushExtCleanup(this._syntaxTreeView);

		vscode.window.onDidChangeActiveTextEditor(async () => {
			if (this.syntaxTreeView?.visible) {
				await this.syntaxTreeProvider?.refresh();
			}
		});

		vscode.workspace.onDidChangeTextDocument(async (event) => {
			if (
				vscode.window.activeTextEditor?.document !== event.document
				|| event.contentChanges.length === 0
			) {
				return;
			}

			if (this.syntaxTreeView?.visible) {
				await this.syntaxTreeProvider?.refresh();
			}
		});

		vscode.window.onDidChangeTextEditorSelection(async (event) => {
			if (!(this.syntaxTreeView?.visible && isWeslEditor(event.textEditor))) {
				return;
			}

			const selection = event.selections[0];
			if (selection === undefined) {
				return;
			}

			const result = this.syntaxTreeProvider?.getElementByRange(selection);
			if (result !== undefined) {
				await this.syntaxTreeView.reveal(result);
			}
		});

		this._syntaxTreeView.onDidChangeVisibility(async (event) => {
			if (event.visible) {
				await this.syntaxTreeProvider?.refresh();
			}
		});
	}

	public async restart() {
		// FIXME: We should re-use the client, that is context.deactivate() if none of the configs have changed
		await this.stopAndDispose();
		await this.start();
	}

	public async stop() {
		if (!this._client) {
			return;
		}
		log.info("Stopping language client");
		this.updateCommands("disable");
		await this._client.stop();
	}

	public async stopAndDispose() {
		if (!this._client) {
			return;
		}
		log.info("Disposing language client");
		this.updateCommands("disable");
		// we give the server 100ms to stop gracefully
		const timeoutMilliseconds = 100;
		await this.client?.stop(timeoutMilliseconds).catch((_: unknown) => {
			// failing to stop is not worth handling
		});
		await this.disposeClient();
	}

	private async disposeClient() {
		for (const disposable of this.clientSubscriptions) {
			disposable.dispose();
		}
		this.clientSubscriptions = [];
		await this._client?.dispose();
		this._serverPath = undefined;
		this._client = undefined;
	}

	public get activeWeslEditor(): WeslEditor | undefined {
		const editor = vscode.window.activeTextEditor;
		return editor && isWeslEditor(editor) ? editor : undefined;
	}

	public get activeWeslTomlEditor(): vscode.TextEditor | undefined {
		const editor = vscode.window.activeTextEditor;
		return editor && isWeslTomlEditor(editor) ? editor : undefined;
	}

	public get extensionPath(): string {
		return this.extCtx.extensionPath;
	}

	public get subscriptions(): Disposable[] {
		return this.extCtx.subscriptions;
	}

	private updateCommands(forceDisable?: "disable") {
		for (const disposable of this.commandDisposables) {
			disposable.dispose();
		}
		this.commandDisposables = [];

		const clientRunning = (!forceDisable && this._client?.isRunning()) ?? false;
		const isClientRunning = (_ctx: Context): _ctx is InitializedContext => clientRunning;

		for (const [name, factory] of Object.entries(this.commandFactories)) {
			const fullName = `wgsl-analyzer.${name}`;
			// biome-ignore lint/suspicious/noExplicitAny: Signature comes from upstream
			let callback: any;
			if (isClientRunning(this)) {
				// we asserted that `client` is defined
				callback = factory.enabled(this);
			} else if (factory.disabled) {
				callback = factory.disabled(this);
			} else {
				callback = () =>
					vscode.window.showErrorMessage(
						`command ${fullName} failed: wgsl-analyzer server is not running`,
					);
			}

			this.commandDisposables.push(vscode.commands.registerCommand(fullName, callback));
		}
	}

	public setServerStatus(status: ServerStatusParameters | { health: "stopped" }) {
		this.lastStatus = status;
		this.updateStatusBarItem();
	}

	public refreshServerStatus() {
		this.updateStatusBarItem();
	}

	private updateStatusBarItem() {
		let icon = "";
		const status = this.lastStatus;
		const statusBar = this.statusBar;
		statusBar.tooltip = new vscode.MarkdownString("", true);
		statusBar.tooltip.isTrusted = true;
		switch (status.health) {
			case "ok":
				statusBar.color = undefined;
				statusBar.backgroundColor = undefined;
				if (this.config.statusBarClickAction === "stopServer") {
					statusBar.command = "wgsl-analyzer.stopServer";
				} else {
					statusBar.command = "wgsl-analyzer.openLogs";
				}
				void this.syntaxTreeProvider?.refresh();
				break;
			case "warning":
				statusBar.color = new vscode.ThemeColor("statusBarItem.warningForeground");
				statusBar.backgroundColor = new vscode.ThemeColor("statusBarItem.warningBackground");
				statusBar.command = "wgsl-analyzer.openLogs";
				icon = "$(warning) ";
				break;
			case "error":
				statusBar.color = new vscode.ThemeColor("statusBarItem.errorForeground");
				statusBar.backgroundColor = new vscode.ThemeColor("statusBarItem.errorBackground");
				statusBar.command = "wgsl-analyzer.openLogs";
				icon = "$(error) ";
				break;
			case "stopped":
				statusBar.tooltip.appendText("Server is stopped");
				statusBar.tooltip.appendMarkdown("\n\n[Start server](command:wgsl-analyzer.startServer)");
				statusBar.color = new vscode.ThemeColor("statusBarItem.warningForeground");
				statusBar.backgroundColor = new vscode.ThemeColor("statusBarItem.warningBackground");
				statusBar.command = "wgsl-analyzer.startServer";
				statusBar.text = "$(stop-circle) wgsl-analyzer";
				return;
		}
		if (status.message) {
			statusBar.tooltip.appendMarkdown(status.message);
		}
		if (statusBar.tooltip.value) {
			statusBar.tooltip.appendMarkdown("\n\n---\n\n");
		}

		const toggleCheckOnSave = this.config.checkOnSave ? "Disable" : "Enable";
		statusBar.tooltip.appendMarkdown(
			`[Extension Info](command:wgsl-analyzer.serverVersion "Show version and server binary info"): Version ${this.version}, Server Version ${this._serverVersion}`
				+ "\n\n---\n\n"
				+ '[$(terminal) Open Logs](command:wgsl-analyzer.openLogs "Open the server logs")'
				+ "\n\n"
				+ `[$(settings) ${toggleCheckOnSave} Check on Save](command:wgsl-analyzer.toggleCheckOnSave "Temporarily ${toggleCheckOnSave.toLowerCase()} check on save functionality")`
				+ "\n\n"
				+ '[$(stop-circle) Stop server](command:wgsl-analyzer.stopServer "Stop the server")'
				+ "\n\n"
				+ '[$(debug-restart) Restart server](command:wgsl-analyzer.restartServer "Restart the server")',
		);
		// biome-ignore lint/security/noSecrets: not a secret
		if (!status.quiescent) icon = "$(loading~spin) ";
		statusBar.text = `${icon}wgsl-analyzer`;
	}

	private updateStatusBarVisibility(editor: vscode.TextEditor | undefined) {
		const showStatusBar = this.config.statusBarShowStatusBar;
		if (showStatusBar === undefined || showStatusBar === "never") {
			this.statusBar.hide();
		} else if (showStatusBar === "always") {
			this.statusBar.show();
		} else {
			const documentSelector = showStatusBar.documentSelector;
			if (editor !== undefined && vscode.languages.match(documentSelector, editor.document) > 0) {
				this.statusBar.show();
			} else {
				this.statusBar.hide();
			}
		}
	}

	public pushExtCleanup(d: Disposable) {
		this.extCtx.subscriptions.push(d);
	}

	public pushClientCleanup(d: Disposable) {
		this.clientSubscriptions.push(d);
	}
}

// biome-ignore lint/style/useConsistentTypeDefinitions: behavior
export interface Disposable {
	dispose: () => void;
}

// biome-ignore lint/suspicious/noExplicitAny: Signature comes from upstream
export type Cmd = (...args: any[]) => unknown;
