import { createAgentSession, AgentSession, codingTools, readOnlyTools } from "@mariozechner/pi-coding-agent";
import { getModel, type Model } from "@mariozechner/pi-ai";
import { log } from "./log.js";
import type { SendEventFn, SendAndWaitFn } from "./daemon-session.js";
import { createPiExtension, type PiExtensionOptions } from "./pi-extension.js";

type AnyModel = Model<any>;

export interface PiSessionOptions {
  id: string;
  cwd: string;
  model?: string;
  permissionMode?: string;
  resume?: string;
  sendEvent: SendEventFn;
  sendAndWait: SendAndWaitFn;
  onSessionId?: (realId: string) => void;
  onDone?: (sessionId: string) => void;
}

export class PiSession {
  readonly tempId: string;
  private realSessionId: string | null = null;
  private sessionStartSent = false;
  private cwd: string;
  private agentSession: AgentSession | null = null;
  private sendEvent: SendEventFn;
  private sendAndWait: SendAndWaitFn;
  private onSessionId?: (realId: string) => void;
  private onDone?: (sessionId: string) => void;
  private running = false;
  private resolvePrompt: ((value: void) => void) | null = null;
  private pendingPrompts: string[] = [];

  constructor(opts: PiSessionOptions) {
    this.tempId = opts.id;
    this.cwd = opts.cwd;
    this.sendEvent = opts.sendEvent;
    this.sendAndWait = opts.sendAndWait;
    this.onSessionId = opts.onSessionId;
    this.onDone = opts.onDone;
  }

  get sessionId(): string {
    return this.realSessionId || this.tempId;
  }

  get isRunning(): boolean {
    return this.running;
  }

  async start(_opts: PiSessionOptions): Promise<void> {
    this.running = true;
    log(`[pi-session] Starting session ${this.tempId} in ${this.cwd}`, 'info');

    const extensionOptions: PiExtensionOptions = {
      sendEvent: this.sendEvent,
      sendAndWait: this.sendAndWait,
      getSessionId: () => this.sessionId,
      getCwd: () => this.cwd,
      pid: process.pid,
    };

    const piExtension = createPiExtension(extensionOptions);

    let model: AnyModel;
    if (_opts.model) {
      const modelParts = _opts.model.split("/");
      if (modelParts.length >= 2) {
        model = getModel(modelParts[0] as any, modelParts[1] as any);
      } else {
        model = getModel("anthropic", _opts.model as any);
      }
    } else {
      model = getModel("anthropic", "claude-sonnet-4-20250514");
    }

    try {
      const result = await createAgentSession({
        cwd: this.cwd,
        model,
        tools: codingTools,
        customTools: [],
        sessionManager: undefined,
      });

      this.agentSession = result.session;
      this.realSessionId = this.agentSession.sessionId;
      this.onSessionId?.(this.realSessionId);

      log(`[pi-session] Session initialized: ${this.realSessionId}`, 'info');

      if (!this.sessionStartSent) {
        this.sessionStartSent = true;
        await this.sendEvent("SessionStart", this.sessionId, this.cwd, process.pid, {
          session_id: this.sessionId,
          model: model.provider + "/" + model.id,
        });
      }

      await this.runPromptLoop();
    } catch (e: any) {
      log(`[pi-session] Session error for ${this.sessionId}: ${e.message}`, 'error');
      await this.sendEvent("DaemonError", this.sessionId, this.cwd, process.pid, {
        error: e.message,
        session_id: this.sessionId,
      });
    } finally {
      this.running = false;
      this.onDone?.(this.sessionId);
      log(`[pi-session] Session ${this.sessionId} ended`, 'info');
    }
  }

  private async runPromptLoop(): Promise<void> {
    if (!this.agentSession) return;

    while (this.running) {
      if (this.pendingPrompts.length > 0) {
        const text = this.pendingPrompts.shift()!;
        try {
          await this.agentSession.prompt(text);
        } catch (e: any) {
          log(`[pi-session] Prompt error: ${e.message}`, 'error');
        }
      } else {
        await new Promise<void>((resolve) => {
          this.resolvePrompt = resolve;
        });
      }
    }
  }

  sendPrompt(text: string): void {
    if (!this.running) {
      log(`[pi-session] Cannot send prompt — session ${this.sessionId} not running`, 'warn');
      return;
    }
    this.pendingPrompts.push(text);
    if (this.resolvePrompt) {
      this.resolvePrompt();
      this.resolvePrompt = null;
    }
  }

  async interrupt(): Promise<void> {
    if (this.agentSession) {
      try {
        await this.agentSession.abort();
      } catch (e: any) {
        log(`[pi-session] Interrupt error: ${e.message}`, 'warn');
      }
    }
  }

  async setModel(model: string): Promise<void> {
    if (this.agentSession) {
      try {
        const modelParts = model.split("/");
        let resolvedModel: AnyModel;
        if (modelParts.length >= 2) {
          resolvedModel = getModel(modelParts[0] as any, modelParts[1] as any);
        } else {
          resolvedModel = getModel("anthropic", model as any);
        }
        await this.agentSession.setModel(resolvedModel);
      } catch (e: any) {
        log(`[pi-session] setModel error: ${e.message}`, 'warn');
      }
    }
  }

  setPermissionMode(_mode: string): void {
    log(`[pi-session] Permission mode not implemented in pi-agent`, 'warn');
  }

  stop(): void {
    this.pendingPrompts = [];
    if (this.resolvePrompt) {
      this.resolvePrompt();
      this.resolvePrompt = null;
    }
    if (this.agentSession) {
      this.agentSession.dispose();
      this.agentSession = null;
    }
    this.running = false;
  }
}
