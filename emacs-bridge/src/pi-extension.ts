import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import type { SendEventFn, SendAndWaitFn } from "./daemon-session.js";

interface ToolCallEvent {
  toolCallId: string;
  toolName: string;
}

interface ToolExecutionStartEvent {
  toolCallId: string;
  toolName: string;
  args: Record<string, unknown>;
}

interface ToolExecutionEndEvent {
  toolCallId: string;
  toolName: string;
  result: unknown;
  isError: boolean;
}

interface AgentStartEvent {}

interface AgentEndEvent {
  messages: unknown[];
}

interface MessageUpdateEvent {
  assistantMessageEvent: unknown;
}

export interface PiExtensionOptions {
  sendEvent: SendEventFn;
  sendAndWait: SendAndWaitFn;
  getSessionId: () => string;
  getCwd: () => string;
  pid: number;
}

function getToolArgs(event: ToolCallEvent): Record<string, unknown> {
  if ("input" in event && event.input) {
    return event.input as Record<string, unknown>;
  }
  return {};
}

export function createPiExtension(options: PiExtensionOptions): (pi: ExtensionAPI) => void {
  const { sendEvent, sendAndWait, getSessionId, getCwd, pid } = options;

  return (pi: ExtensionAPI) => {
    pi.on("tool_call", async (event: ToolCallEvent): Promise<{ block?: boolean; reason?: string }> => {
      const sessionId = getSessionId();
      const cwd = getCwd();

      const response = await sendAndWait("PermissionRequest", sessionId, cwd, pid, {
        tool_name: event.toolName,
        tool_call_id: event.toolCallId,
        parameters: getToolArgs(event),
      });

      if (response.blocked) {
        return { block: true, reason: response.reason };
      }
      return {};
    });

    pi.on("tool_execution_start", (event: ToolExecutionStartEvent) => {
      const sessionId = getSessionId();
      const cwd = getCwd();

      sendEvent("PreToolUse", sessionId, cwd, pid, {
        tool_name: event.toolName,
        tool_call_id: event.toolCallId,
        parameters: event.args,
      });
    });

    pi.on("tool_execution_end", (event: ToolExecutionEndEvent) => {
      const sessionId = getSessionId();
      const cwd = getCwd();

      sendEvent("PostToolUse", sessionId, cwd, pid, {
        tool_name: event.toolName,
        tool_call_id: event.toolCallId,
        result: event.result,
        is_error: event.isError,
      });
    });

    pi.on("agent_start", (_event: AgentStartEvent) => {
      const sessionId = getSessionId();
      const cwd = getCwd();

      sendEvent("SessionStart", sessionId, cwd, pid, {});
    });

    pi.on("agent_end", (event: AgentEndEvent) => {
      const sessionId = getSessionId();
      const cwd = getCwd();

      sendEvent("SessionEnd", sessionId, cwd, pid, {
        num_turns: event.messages.length,
      });
    });

    pi.on("message_update", (event: MessageUpdateEvent) => {
      const sessionId = getSessionId();
      const cwd = getCwd();

      if (event.assistantMessageEvent && typeof event.assistantMessageEvent === "object") {
        const assistantEvent = event.assistantMessageEvent as { type?: string; delta?: string; text?: string };
        if (assistantEvent.type === "content" && assistantEvent.delta) {
          sendEvent("StreamDelta", sessionId, cwd, pid, {
            text: assistantEvent.delta,
          });
        }
      }
    });
  };
}
