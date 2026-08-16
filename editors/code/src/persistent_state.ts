import type * as vscode from "vscode";

import { log } from "./utilities.ts";

export class PersistentState {
	// biome-ignore lint/style/noParameterProperties: TODO
	public constructor(private readonly globalState: vscode.Memento) {
		const { serverVersion } = this;
		log.info("PersistentState:", { serverVersion });
	}

	/**
	 * Version of the extension that installed the server.
	 * Used to check if we need to run patchelf again on NixOS.
	 */
	public get serverVersion(): string | undefined {
		return this.globalState.get("serverVersion");
	}

	public async updateServerVersion(value: string | undefined) {
		await this.globalState.update("serverVersion", value);
	}
}
