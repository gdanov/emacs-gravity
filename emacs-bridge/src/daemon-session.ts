// Per-session SDK wrapper for the daemon.
// Manages the prompt queue, SDK query lifecycle, and message forwarding.

import type { Query, SDKMessage, SDKUserMessage, SDKSystemMessage, SDKResultMessage, SDKPartialAssistantMessage, SDKAssistantMessage, Options, PermissionResult, HookEvent, HookCallbackMatcher } from "@anthropic-ai/claude-agent-sdk";
import { query } from "@anthropic-ai/claude-agent-sdk";
import { log } from "./log";

// ============================================================================
// Async prompt queue — feeds user messages to the SDK query
// ============================================================================

export class PromptQueue implements AsyncIterable<SDKUserMessage> {
  private pending: SDKUserMessage[] = [];
  private waiter: { resolve: (msg: SDKUserMessage) => void } | null = null;
  private closed = false;

  push(text: string, sessionId: string): void {
    const msg: SDKUserMessage = {
      type: "user",
      session_id: sessionId,
      message: { role: "user", content: [{ type: "text", text }] },
      parent_tool_use_id: null,
    };
    if (this.waiter) {
      this.waiter.resolve(msg);
      this.waiter = null;
    } else {
      this.pending.push(msg);
    }
  }

  close(): void {
    this.closed = true;
    if (this.waiter) {
      // Resolve with a sentinel that will cause the iterator to end
      // We use a special message that the iterator checks for
      this.waiter.resolve({
        type: "user",
        session_id: "__closed__",
        message: { role: "user", content: [] },
        parent_tool_use_id: null,
      });
      this.waiter = null;
    }
  }

  async *[Symbol.asyncIterator](): AsyncGenerator<SDKUserMessage, void, undefined> {
    while (!this.closed) {
      if (this.pending.length > 0) {
        yield this.pending.shift()!;
      } else {
        const msg = await new Promise<SDKUserMessage>((resolve) => {
          this.waiter = { resolve };
        });
        if (msg.session_id === "__closed__") return;
        yield msg;
      }
    }
  }
}

// ============================================================================
// Daemon session — wraps a single SDK query
// ============================================================================

export type SendEventFn = (eventName: string, sessionId: string, cwd: string, pid: number | null, payload: any) => Promise<void>;
export type SendAndWaitFn = (eventName: string, sessionId: string, cwd: string, pid: number | null, payload: any) => Promise<any>;

export interface DaemonSessionOptions {
  id: string;                     // Temp ID assigned by Emacs
  cwd: string;
  model?: string;
  permissionMode?: string;
  resume?: string;                // Session ID to resume
  sendEvent: SendEventFn;
  sendAndWait: SendAndWaitFn;
  hooks?: Partial<Record<HookEvent, HookCallbackMatcher[]>>;
  canUseTool?: Options["canUseTool"];
  onSessionId?: (realId: string) => void;  // Called when real session_id is known
  onDone?: (sessionId: string, result?: SDKResultMessage) => void;
}

export class DaemonSession {
  readonly tempId: string;
  private realSessionId: string | null = null;
  private sessionStartSent = false;
  private cwd: string;
  private promptQueue: PromptQueue;
  private queryInstance: Query | null = null;
  private abortController: AbortController;
  private sendEvent: SendEventFn;
  private sendAndWait: SendAndWaitFn;
  private onSessionId?: (realId: string) => void;
  private onDone?: (sessionId: string, result?: SDKResultMessage) => void;
  private running = false;

  constructor(opts: DaemonSessionOptions) {
    this.tempId = opts.id;
    this.cwd = opts.cwd;
    this.promptQueue = new PromptQueue();
    this.abortController = new AbortController();
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

  /** Start the SDK query and begin consuming messages. */
  async start(opts: DaemonSessionOptions): Promise<void> {
    this.running = true;
    log(`[daemon-session] Starting session ${this.tempId} in ${this.cwd}`, 'info');

    const queryOpts: Options = {
      cwd: this.cwd,
      abortController: this.abortController,
      settingSources: ["user", "project", "local"],
      systemPrompt: { type: "preset", preset: "claude_code" },
      tools: { type: "preset", preset: "claude_code" },
      includePartialMessages: true,
      // Do NOT load the emacs-bridge plugin — we handle hooks natively
    };

    if (opts.model) queryOpts.model = opts.model;
    if (opts.permissionMode) queryOpts.permissionMode = opts.permissionMode as any;
    if (opts.resume) queryOpts.resume = opts.resume;
    if (opts.hooks) queryOpts.hooks = opts.hooks;
    if (opts.canUseTool) queryOpts.canUseTool = opts.canUseTool;

    try {
      this.queryInstance = query({
        prompt: this.promptQueue,
        options: queryOpts,
      });

      // Consume the async generator
      for await (const msg of this.queryInstance) {
        await this.handleMessage(msg);
      }
    } catch (e: any) {
      log(`[daemon-session] Query error for ${this.sessionId}: ${e.message}`, 'error');
      // Send error event to Emacs
      await this.sendEvent("DaemonError", this.sessionId, this.cwd, null, {
        error: e.message,
        session_id: this.sessionId,
      });
    } finally {
      this.running = false;
      this.onDone?.(this.sessionId);
      log(`[daemon-session] Session ${this.sessionId} ended`, 'info');
    }
  }

  /** Send a prompt to the running session. */
  sendPrompt(text: string): void {
    if (!this.running) {
      log(`[daemon-session] Cannot send prompt — session ${this.sessionId} not running`, 'warn');
      return;
    }
    this.promptQueue.push(text, this.sessionId);
  }

  /** Interrupt the current query. */
  async interrupt(): Promise<void> {
    if (this.queryInstance) {
      try {
        await this.queryInstance.interrupt();
      } catch (e: any) {
        log(`[daemon-session] Interrupt error: ${e.message}`, 'warn');
      }
    }
  }

  /** Change the model for subsequent turns. */
  async setModel(model: string): Promise<void> {
    if (this.queryInstance) {
      try {
        await this.queryInstance.setModel(model);
      } catch (e: any) {
        log(`[daemon-session] setModel error: ${e.message}`, 'warn');
      }
    }
  }

  /** Change the permission mode. */
  async setPermissionMode(mode: string): Promise<void> {
    if (this.queryInstance) {
      try {
        await this.queryInstance.setPermissionMode(mode as any);
      } catch (e: any) {
        log(`[daemon-session] setPermissionMode error: ${e.message}`, 'warn');
      }
    }
  }

  /** Stop the session entirely. */
  stop(): void {
    this.promptQueue.close();
    if (this.queryInstance) {
      this.queryInstance.close();
    }
    this.abortController.abort();
    this.running = false;
  }

  // --------------------------------------------------------------------------
  // Message handling — maps SDK messages to gravity events
  // --------------------------------------------------------------------------

  private async handleMessage(msg: SDKMessage): Promise<void> {
    switch (msg.type) {
      case "system":
        await this.handleSystemMessage(msg as any);
        break;
      case "assistant":
        await this.handleAssistantMessage(msg as SDKAssistantMessage);
        break;
      case "stream_event":
        await this.handleStreamEvent(msg as SDKPartialAssistantMessage);
        break;
      case "result":
        await this.handleResult(msg as SDKResultMessage);
        break;
      case "tool_progress":
        // Forward tool progress as a lightweight event
        await this.sendEvent("ToolProgress", this.sessionId, this.cwd, null, {
          tool_use_id: (msg as any).tool_use_id,
          tool_name: (msg as any).tool_name,
          elapsed_time_seconds: (msg as any).elapsed_time_seconds,
          session_id: this.sessionId,
        });
        break;
      default:
        log(`[daemon-session] Unhandled message type: ${msg.type}`, 'debug');
    }
  }

  private async handleSystemMessage(msg: any): Promise<void> {
    if (msg.subtype === "init") {
      const initMsg = msg as SDKSystemMessage;
      const realId = initMsg.session_id;
      log(`[daemon-session] system/init: session_id=${realId}, model=${initMsg.model}`, 'info');

      this.realSessionId = realId;
      this.onSessionId?.(realId);

      // Only send SessionStart on the FIRST system/init.
      // Subsequent prompts also trigger system/init but the session
      // is already established — sending SessionStart again would
      // reset the session state in Emacs (wiping turns/tools).
      if (!this.sessionStartSent) {
        this.sessionStartSent = true;
        await this.sendEvent("SessionStart", realId, this.cwd, null, {
          session_id: realId,
          cwd: this.cwd,
          temp_id: this.tempId,
          source: "daemon",
          model: initMsg.model,
          permission_mode: initMsg.permissionMode,
          tools: initMsg.tools,
        });
      }
    } else if (msg.subtype === "status") {
      // Status updates (e.g., compacting)
      await this.sendEvent("StatusLine", this.sessionId, this.cwd, null, {
        status: msg.status,
        session_id: this.sessionId,
      });
    }
  }

  private async handleAssistantMessage(msg: SDKAssistantMessage): Promise<void> {
    // Complete assistant message — clear streaming text in Emacs
    await this.sendEvent("AssistantComplete", this.sessionId, this.cwd, null, {
      session_id: this.sessionId,
      uuid: msg.uuid,
    });
  }

  private async handleStreamEvent(_msg: SDKPartialAssistantMessage): Promise<void> {
    // StreamDelta events disabled — streaming text not needed for now.
    // Hook-based text extraction (PostToolUse/Stop) provides the content we display.
  }

  private async handleResult(msg: SDKResultMessage): Promise<void> {
    const isSuccess = msg.subtype === "success";
    await this.sendEvent("DaemonResult", this.sessionId, this.cwd, null, {
      session_id: this.sessionId,
      subtype: msg.subtype,
      is_error: msg.is_error,
      num_turns: msg.num_turns,
      total_cost_usd: msg.total_cost_usd,
      duration_ms: msg.duration_ms,
      usage: msg.usage,
      result: isSuccess ? (msg as any).result : undefined,
      errors: !isSuccess ? (msg as any).errors : undefined,
    });
  }
}
