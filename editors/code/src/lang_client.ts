import * as vscode from "vscode";
import * as lc from "vscode-languageclient/node";

export class WaLanguageClient extends lc.LanguageClient {
	public override handleFailedRequest<T>(
		type: lc.MessageSignature,
		token: vscode.CancellationToken | undefined,
		// biome-ignore lint/suspicious/noExplicitAny: Signature comes from upstream
		error: any,
		defaultValue: T,
		showNotification?: boolean,
	): T {
		const showError = vscode.workspace
			.getConfiguration("wgsl-analyzer")
			// biome-ignore lint/security/noSecrets: not a secret
			.get("showRequestFailedErrorNotification");
		if (
			!showError
			&& error instanceof lc.ResponseError
			&& error.code === lc.ErrorCodes.InternalError
		) {
			// Do not show notification for internal errors, these are emitted by w-a when a request fails.
			// biome-ignore lint/style/noParameterAssign: intentional
			showNotification = false;
		}

		return super.handleFailedRequest(type, token, error, defaultValue, showNotification);
	}
}
