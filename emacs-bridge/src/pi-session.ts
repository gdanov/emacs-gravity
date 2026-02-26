import { Agent } from "@mariozechner/pi-agent-core";
import { getModel, type Model } from "@mariozechner/pi-ai";
import { log } from "./log.js";
import type { SendEventFn, SendAndWaitFn } from "./daemon-session.js";

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
  private agent: Agent | null = null;
  private sendEvent: SendEventFn;
  private sendAndWait: SendAndWaitFn;
  private onSessionId?: (realId: string) => void;
  private onDone?: (sessionId: string) => void;
  private running = false;
  private resolvePrompt: ((value: void) => void) | null = null;
  private pendingPrompts: string[] = [];
  private assistantCompleteSent = false;
  private lastAssistantMessage: any = null;

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

  private sendToolEvent(eventName: string, data: any): void {
    this.sendEvent(eventName, this.sessionId, this.cwd, process.pid, data).catch((e: any) => {
      log(`[pi-session] sendToolEvent error: ${e.message}`, 'error');
    });
  }

  async start(_opts: PiSessionOptions): Promise<void> {
    this.running = true;
    log(`[pi-session] Starting session ${this.tempId} in ${this.cwd}`, 'info');

    let model: AnyModel;
    if (_opts.model) {
      const modelParts = _opts.model.split("/");
      if (modelParts.length >= 2) {
        model = getModel(modelParts[0] as any, modelParts[1] as any);
      } else {
        model = getModel("anthropic", _opts.model as any);
      }
    } else {
      model = getModel("minimax", "MiniMax-M2.5");
    }

    try {
      this.agent = new Agent({
        initialState: {
          model,
          systemPrompt: "You are a helpful coding assistant.",
          tools: [],
        },
      });

      this.realSessionId = this.agent.sessionId || this.tempId;
      this.onSessionId?.(this.realSessionId);

      log(`[pi-session] Session initialized: ${this.realSessionId}`, 'info');

      this.agent.subscribe((event) => {
        switch (event.type) {
          case "agent_start":
            // SessionStart is sent once in start() — do NOT send again per-prompt.
            // agent_start fires for every prompt() call, which would trigger
            // reset-session in Emacs and wipe turn data.
            log(`[pi-session] agent_start`, 'debug');
            break;
          case "agent_end":
            log(`[pi-session] agent_end`, 'debug');
            // Extract stop_text from the last assistant message
            let stopText: string | undefined;
            let stopThinking: string | undefined;
            if (this.lastAssistantMessage) {
              const content = this.lastAssistantMessage.content;
              if (typeof content === 'string') {
                stopText = content;
              } else if (content && typeof content === 'object') {
                stopText = content.text || content.content?.[0]?.text;
                stopThinking = content.thinking;
              }
            }
            // Send AssistantComplete if not already sent (e.g., response with no tools)
            if (!this.assistantCompleteSent) {
              this.sendToolEvent("AssistantComplete", { session_id: this.sessionId });
            }
            this.assistantCompleteSent = false; // Reset for next response cycle
            // Send Stop (not SessionEnd) — agent_end fires per-prompt, not per-session.
            // SessionEnd would mark the session as ended in Emacs.
            this.sendToolEvent("Stop", { 
              session_id: this.sessionId,
              stop_text: stopText,
              stop_thinking: stopThinking
            });
            this.lastAssistantMessage = null; // Reset for next turn
            break;
          case "tool_execution_start":
            log(`[pi-session] tool_execution_start: ${event.toolName}`, 'debug');
            // Assistant message is complete before tools run - send AssistantComplete
            if (!this.assistantCompleteSent) {
              this.sendToolEvent("AssistantComplete", { session_id: this.sessionId });
              this.assistantCompleteSent = true;
            }
            this.sendToolEvent("PreToolUse", {
              tool_name: event.toolName,
              tool_call_id: event.toolCallId,
            });
            break;
          case "tool_execution_end":
            log(`[pi-session] tool_execution_end: ${event.toolName}`, 'debug');
            this.sendToolEvent("PostToolUse", {
              tool_name: event.toolName,
              tool_call_id: event.toolCallId,
              result: event.result,
              is_error: event.isError,
            });
            break;
          case "turn_end":
            // Store the assistant message for extracting stop_text on agent_end
            this.lastAssistantMessage = event.message;
            break;
          case "message_update":
            if (event.assistantMessageEvent && "delta" in event.assistantMessageEvent) {
              const delta = (event.assistantMessageEvent as any).delta;
              if (delta) {
                this.sendToolEvent("StreamDelta", { text: delta });
              }
            }
            break;
        }
      });

      await this.sendEvent("SessionStart", this.sessionId, this.cwd, process.pid, {
        session_id: this.sessionId,
        temp_id: this.tempId,
        model: model.provider + "/" + model.id,
      });

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
    if (!this.agent) return;

    log(`[pi-session] runPromptLoop started for ${this.sessionId}`, 'info');
    while (this.running) {
      if (this.pendingPrompts.length > 0) {
        const text = this.pendingPrompts.shift()!;
        log(`[pi-session] Processing prompt: "${text.substring(0, 50)}..."`, 'info');
        try {
          await this.agent.prompt(text);
          log(`[pi-session] Prompt completed`, 'info');
        } catch (e: any) {
          log(`[pi-session] Prompt error: ${e.message}`, 'error');
        }
      } else {
        log(`[pi-session] runPromptLoop waiting for prompt...`, 'debug');
        await new Promise<void>((resolve) => {
          this.resolvePrompt = resolve;
        });
        log(`[pi-session] runPromptLoop woken up, queue length: ${this.pendingPrompts.length}`, 'debug');
      }
    }
    log(`[pi-session] runPromptLoop exited`, 'info');
  }

  sendPrompt(text: string): void {
    if (!this.running) {
      log(`[pi-session] Cannot send prompt — session ${this.sessionId} not running`, 'warn');
      return;
    }
    log(`[pi-session] sendPrompt: queueing "${text.substring(0, 50)}..."`, 'info');
    this.pendingPrompts.push(text);

    // Reset AssistantComplete flag for new response cycle
    this.assistantCompleteSent = false;

    // Emit UserPromptSubmit for Emacs to track turns
    this.sendToolEvent("UserPromptSubmit", {
      prompt: text,
      session_id: this.sessionId,
    });

    if (this.resolvePrompt) {
      log(`[pi-session] sendPrompt: waking prompt loop`, 'info');
      this.resolvePrompt();
      this.resolvePrompt = null;
    }
  }

  async interrupt(): Promise<void> {
    if (this.agent) {
      try {
        this.agent.abort();
      } catch (e: any) {
        log(`[pi-session] Interrupt error: ${e.message}`, 'warn');
      }
    }
  }

  async setModel(model: string): Promise<void> {
    if (this.agent) {
      try {
        const modelParts = model.split("/");
        let resolvedModel: AnyModel;
        if (modelParts.length >= 2) {
          resolvedModel = getModel(modelParts[0] as any, modelParts[1] as any);
        } else {
          resolvedModel = getModel("anthropic", model as any);
        }
        this.agent.setModel(resolvedModel);
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
    if (this.agent) {
      this.agent.abort();
      this.agent = null;
    }
    this.running = false;
  }
}
