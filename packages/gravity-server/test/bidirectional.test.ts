// bidirectional.test.ts — Tests for bidirectional flows
//
// Tests PermissionRequest, AskUserQuestion, plan review, and the
// deny-as-approve workaround for ExitPlanMode (#15755).

import { describe, it, expect, beforeEach, vi } from "vitest";
import type { Socket } from "net";
import { InboxManager } from "../src/state/inbox.js";

function mockSocket(): Socket & {
  written: string[];
  ended: boolean;
} {
  const buf: string[] = [];
  const sock = {
    written: buf,
    ended: false,
    write(data: string) {
      buf.push(data);
      return true;
    },
    end() {
      sock.ended = true;
    },
  } as unknown as Socket & { written: string[]; ended: boolean };
  return sock;
}

describe("InboxManager", () => {
  let inbox: InboxManager;

  beforeEach(() => {
    inbox = new InboxManager();
  });

  describe("add", () => {
    it("creates an inbox item with incrementing ID", () => {
      const item1 = inbox.add("permission", "s1", "proj", "Read", "summary", {});
      const item2 = inbox.add("permission", "s2", "proj", "Edit", "summary2", {});
      expect(item1.id).toBe(1);
      expect(item2.id).toBe(2);
    });

    it("stores the item in the items list", () => {
      inbox.add("permission", "s1", "proj", "Read", "summary", {});
      expect(inbox.all()).toHaveLength(1);
    });

    it("items are added to the front (most recent first)", () => {
      inbox.add("permission", "s1", "proj", "First", "s1", {});
      inbox.add("permission", "s2", "proj", "Second", "s2", {});
      expect(inbox.all()[0].label).toBe("Second");
      expect(inbox.all()[1].label).toBe("First");
    });

    it("registers pending response when hookSocket provided", () => {
      const sock = mockSocket();
      const item = inbox.add("permission", "s1", "proj", "Read", "summary", {}, sock);
      expect(inbox.getPending(item.id)).toBeDefined();
      expect(inbox.getPending(item.id)!.hookSocket).toBe(sock);
    });

    it("does not register pending response without hookSocket", () => {
      const item = inbox.add("permission", "s1", "proj", "Read", "summary", {});
      expect(inbox.getPending(item.id)).toBeUndefined();
    });
  });

  describe("remove", () => {
    it("removes an item by ID", () => {
      const item = inbox.add("permission", "s1", "proj", "Read", "s", {});
      const removed = inbox.remove(item.id);
      expect(removed).toBeDefined();
      expect(removed!.id).toBe(item.id);
      expect(inbox.all()).toHaveLength(0);
    });

    it("returns undefined for non-existent ID", () => {
      expect(inbox.remove(999)).toBeUndefined();
    });

    it("cleans up pending response", () => {
      const sock = mockSocket();
      const item = inbox.add("permission", "s1", "proj", "Read", "s", {}, sock);
      inbox.remove(item.id);
      expect(inbox.getPending(item.id)).toBeUndefined();
    });
  });

  describe("removeForSession", () => {
    it("removes all items for a session", () => {
      inbox.add("permission", "s1", "proj", "A", "s", {});
      inbox.add("permission", "s1", "proj", "B", "s", {});
      inbox.add("permission", "s2", "proj", "C", "s", {});
      const removed = inbox.removeForSession("s1");
      expect(removed).toHaveLength(2);
      expect(inbox.all()).toHaveLength(1);
      expect(inbox.all()[0].label).toBe("C");
    });

    it("filters by type when provided", () => {
      inbox.add("permission", "s1", "proj", "Perm", "s", {});
      inbox.add("question", "s1", "proj", "Q", "s", {});
      const removed = inbox.removeForSession("s1", "permission");
      expect(removed).toHaveLength(1);
      expect(removed[0].label).toBe("Perm");
      expect(inbox.all()).toHaveLength(1);
      expect(inbox.all()[0].label).toBe("Q");
    });

    it("returns empty array for unknown session", () => {
      const removed = inbox.removeForSession("unknown");
      expect(removed).toHaveLength(0);
    });

    it("cleans up pending responses for removed items", () => {
      const sock = mockSocket();
      const item = inbox.add("permission", "s1", "proj", "A", "s", {}, sock);
      inbox.removeForSession("s1");
      expect(inbox.getPending(item.id)).toBeUndefined();
    });
  });

  describe("removeStaleForSession", () => {
    it("removes items without pending responses", () => {
      inbox.add("permission", "s1", "proj", "A", "s", {});
      inbox.add("permission", "s1", "proj", "B", "s", {});
      const removed = inbox.removeStaleForSession("s1");
      expect(removed).toHaveLength(2);
      expect(inbox.all()).toHaveLength(0);
    });

    it("preserves items with active pending responses", () => {
      const sock = mockSocket();
      inbox.add("permission", "s1", "proj", "A", "s", {}, sock);
      const removed = inbox.removeStaleForSession("s1");
      expect(removed).toHaveLength(0);
      expect(inbox.all()).toHaveLength(1);
      expect(inbox.all()[0].label).toBe("A");
    });

    it("removes stale items but keeps pending ones", () => {
      const sock = mockSocket();
      inbox.add("permission", "s1", "proj", "Pending", "s", {}, sock);
      inbox.add("permission", "s1", "proj", "Stale", "s", {});
      const removed = inbox.removeStaleForSession("s1");
      expect(removed).toHaveLength(1);
      expect(removed[0].label).toBe("Stale");
      expect(inbox.all()).toHaveLength(1);
      expect(inbox.all()[0].label).toBe("Pending");
    });

    it("filters by type when provided", () => {
      inbox.add("permission", "s1", "proj", "Perm", "s", {});
      inbox.add("question", "s1", "proj", "Q", "s", {});
      const removed = inbox.removeStaleForSession("s1", "permission");
      expect(removed).toHaveLength(1);
      expect(removed[0].label).toBe("Perm");
      expect(inbox.all()).toHaveLength(1);
      expect(inbox.all()[0].label).toBe("Q");
    });

    it("returns empty array for unknown session", () => {
      const removed = inbox.removeStaleForSession("unknown");
      expect(removed).toHaveLength(0);
    });

    it("does not touch pending map entries for preserved items", () => {
      const sock = mockSocket();
      const item = inbox.add("permission", "s1", "proj", "A", "s", {}, sock);
      inbox.removeStaleForSession("s1");
      expect(inbox.getPending(item.id)).toBeDefined();
      expect(inbox.getPending(item.id)!.hookSocket).toBe(sock);
    });
  });

  describe("find", () => {
    it("finds item by ID", () => {
      const item = inbox.add("permission", "s1", "proj", "A", "s", {});
      expect(inbox.find(item.id)?.label).toBe("A");
    });

    it("returns undefined for non-existent ID", () => {
      expect(inbox.find(999)).toBeUndefined();
    });
  });

  describe("respond", () => {
    it("writes JSON response to hook socket and ends it", () => {
      const sock = mockSocket();
      const item = inbox.add("permission", "s1", "proj", "Read", "s", {}, sock);
      const result = inbox.respond(item.id, {
        hookSpecificOutput: {
          hookEventName: "PermissionRequest",
          decision: { behavior: "allow" },
        },
      });
      expect(result).toBe(true);
      expect(sock.written).toHaveLength(1);
      const parsed = JSON.parse(sock.written[0].replace("\n", ""));
      expect(parsed.hookSpecificOutput.decision.behavior).toBe("allow");
      expect(sock.ended).toBe(true);
    });

    it("removes item from inbox after responding", () => {
      const sock = mockSocket();
      const item = inbox.add("permission", "s1", "proj", "Read", "s", {}, sock);
      inbox.respond(item.id, { hookSpecificOutput: {} });
      expect(inbox.find(item.id)).toBeUndefined();
      expect(inbox.all()).toHaveLength(0);
    });

    it("removes from pending after responding", () => {
      const sock = mockSocket();
      const item = inbox.add("permission", "s1", "proj", "Read", "s", {}, sock);
      inbox.respond(item.id, { hookSpecificOutput: {} });
      expect(inbox.getPending(item.id)).toBeUndefined();
    });

    it("returns false for non-existent pending", () => {
      const result = inbox.respond(999, { hookSpecificOutput: {} });
      expect(result).toBe(false);
    });

    it("returns false for item without pending (fire-and-forget)", () => {
      const item = inbox.add("permission", "s1", "proj", "Read", "s", {});
      const result = inbox.respond(item.id, { hookSpecificOutput: {} });
      expect(result).toBe(false);
    });

    it("second respond to same item returns false", () => {
      const sock = mockSocket();
      const item = inbox.add("permission", "s1", "proj", "Read", "s", {}, sock);
      inbox.respond(item.id, { hookSpecificOutput: {} });
      const result2 = inbox.respond(item.id, { hookSpecificOutput: {} });
      expect(result2).toBe(false);
    });

    it("handles socket write error gracefully", () => {
      const sock = mockSocket();
      sock.write = () => {
        throw new Error("socket closed");
      };
      const item = inbox.add("permission", "s1", "proj", "Read", "s", {}, sock);
      // Should not throw
      const result = inbox.respond(item.id, { hookSpecificOutput: {} });
      expect(result).toBe(true);
      // Item should still be cleaned up
      expect(inbox.getPending(item.id)).toBeUndefined();
    });
  });
});

describe("Bidirectional flow: PermissionRequest", () => {
  it("permission allow produces correct hookSpecificOutput", () => {
    const inbox = new InboxManager();
    const sock = mockSocket();
    inbox.add("permission", "s1", "proj", "Read", "Reading file", {}, sock);
    const item = inbox.all()[0];

    // Simulate terminal sending action.permission
    inbox.respond(item.id, {
      hookSpecificOutput: {
        hookEventName: "PermissionRequest",
        decision: { behavior: "allow" },
      },
    });

    const response = JSON.parse(sock.written[0].replace("\n", ""));
    expect(response.hookSpecificOutput.hookEventName).toBe("PermissionRequest");
    expect(response.hookSpecificOutput.decision.behavior).toBe("allow");
  });

  it("permission deny with message produces correct output", () => {
    const inbox = new InboxManager();
    const sock = mockSocket();
    inbox.add("permission", "s1", "proj", "Bash", "Running npm", {}, sock);
    const item = inbox.all()[0];

    inbox.respond(item.id, {
      hookSpecificOutput: {
        hookEventName: "PermissionRequest",
        decision: { behavior: "deny", message: "Not allowed" },
      },
    });

    const response = JSON.parse(sock.written[0].replace("\n", ""));
    expect(response.hookSpecificOutput.decision.behavior).toBe("deny");
    expect(response.hookSpecificOutput.decision.message).toBe("Not allowed");
  });
});

describe("Bidirectional flow: AskUserQuestion", () => {
  it("question answer produces PreToolUse deny with answer", () => {
    const inbox = new InboxManager();
    const sock = mockSocket();
    inbox.add("question", "s1", "proj", "Question", "Which framework?", {}, sock);
    const item = inbox.all()[0];

    // Simulate the response format from server.ts action.question handler
    const answers = ["Use vitest"];
    inbox.respond(item.id, {
      hookSpecificOutput: {
        hookEventName: "PreToolUse",
        permissionDecision: "deny",
        permissionDecisionReason: `User answered: ${answers[0] || ""}`,
      },
      answer: answers[0] || "",
      answers,
    });

    const response = JSON.parse(sock.written[0].replace("\n", ""));
    expect(response.hookSpecificOutput.hookEventName).toBe("PreToolUse");
    expect(response.hookSpecificOutput.permissionDecision).toBe("deny");
    expect(response.answer).toBe("Use vitest");
    expect(response.answers).toEqual(["Use vitest"]);
  });
});

describe("Deny-as-approve workaround for ExitPlanMode (#15755)", () => {
  /**
   * Claude Code silently ignores "allow" responses from PermissionRequest hooks
   * for ExitPlanMode. The workaround converts allow → deny with a message.
   * Controlled by GRAVITY_DENY_AS_APPROVE=1 env var (disabled by default).
   * These tests simulate the server.ts action.plan-review handler logic.
   */

  it("ExitPlanMode allow passes through when DENY_AS_APPROVE is off (default)", () => {
    const inbox = new InboxManager();
    const sock = mockSocket();
    const item = inbox.add(
      "plan-review", "s1", "proj", "Plan Review", "Review plan",
      { tool_name: "ExitPlanMode" }, sock,
    );

    // Simulate server.ts logic with DENY_AS_APPROVE=false (default)
    const DENY_AS_APPROVE = false;
    const pending = inbox.getPending(item.id);
    const toolName = (pending?.inboxItem.data as Record<string, unknown>)?.tool_name;
    let decision: string = "allow";
    let message: string | undefined;

    if (DENY_AS_APPROVE && toolName === "ExitPlanMode" && decision === "allow") {
      decision = "deny";
      message = message || "User approved the plan. Proceed with implementation.";
    }

    inbox.respond(item.id, {
      hookSpecificOutput: {
        hookEventName: "PermissionRequest",
        decision: { behavior: decision, message },
      },
    });

    const response = JSON.parse(sock.written[0].replace("\n", ""));
    expect(response.hookSpecificOutput.decision.behavior).toBe("allow");
  });

  it("converts ExitPlanMode allow to deny-as-approve when flag enabled", () => {
    const inbox = new InboxManager();
    const sock = mockSocket();
    const item = inbox.add(
      "plan-review", "s1", "proj", "Plan Review", "Review plan",
      { tool_name: "ExitPlanMode" }, sock,
    );

    // Simulate server.ts logic with DENY_AS_APPROVE=true
    const DENY_AS_APPROVE = true;
    const pending = inbox.getPending(item.id);
    const toolName = (pending?.inboxItem.data as Record<string, unknown>)?.tool_name;
    let decision: string = "allow";
    let message: string | undefined;

    if (DENY_AS_APPROVE && toolName === "ExitPlanMode" && decision === "allow") {
      decision = "deny";
      message = message || "User approved the plan. Proceed with implementation.";
    }

    inbox.respond(item.id, {
      hookSpecificOutput: {
        hookEventName: "PermissionRequest",
        decision: { behavior: decision, message },
      },
    });

    const response = JSON.parse(sock.written[0].replace("\n", ""));
    expect(response.hookSpecificOutput.decision.behavior).toBe("deny");
    expect(response.hookSpecificOutput.decision.message).toBe(
      "User approved the plan. Proceed with implementation.",
    );
  });

  it("passes through deny with feedback unchanged", () => {
    const inbox = new InboxManager();
    const sock = mockSocket();
    inbox.add(
      "plan-review", "s1", "proj", "Plan Review", "Review plan",
      { tool_name: "ExitPlanMode" }, sock,
    );
    const item = inbox.all()[0];

    // Deny with feedback should pass through unchanged regardless of flag
    const decision = "deny";
    const message = "# Plan Feedback\n\n## General comment\nPlease also fix tests.";

    inbox.respond(item.id, {
      hookSpecificOutput: {
        hookEventName: "PermissionRequest",
        decision: { behavior: decision, message },
      },
    });

    const response = JSON.parse(sock.written[0].replace("\n", ""));
    expect(response.hookSpecificOutput.decision.behavior).toBe("deny");
    expect(response.hookSpecificOutput.decision.message).toContain("Plan Feedback");
  });

  it("non-ExitPlanMode allow passes through unchanged", () => {
    const inbox = new InboxManager();
    const sock = mockSocket();
    inbox.add(
      "permission", "s1", "proj", "Bash", "Running command",
      { tool_name: "Bash" }, sock,
    );
    const item = inbox.all()[0];

    // Non-ExitPlanMode allow should NOT be converted even with flag on
    const DENY_AS_APPROVE = true;
    const toolName = (inbox.getPending(item.id)?.inboxItem.data as Record<string, unknown>)
      ?.tool_name;
    let decision = "allow";
    if (DENY_AS_APPROVE && toolName === "ExitPlanMode" && decision === "allow") {
      decision = "deny";
    }

    inbox.respond(item.id, {
      hookSpecificOutput: {
        hookEventName: "PermissionRequest",
        decision: { behavior: decision },
      },
    });

    const response = JSON.parse(sock.written[0].replace("\n", ""));
    expect(response.hookSpecificOutput.decision.behavior).toBe("allow");
  });
});

describe("Plan review feedback bundling", () => {
  it("builds structured feedback message from inline comments", () => {
    // Simulate the server.ts plan-review handler feedback bundling
    const feedback = {
      inlineComments: [
        { line: 5, nearText: "Fix the heading", comment: "also fix indentation" },
        { line: 12, nearText: "Add tests", comment: "use vitest not jest" },
      ],
      claudeMarkers: [],
      diff: null as string | null,
      generalComment: null as string | null,
    };

    const parts: string[] = ["# Plan Feedback\n"];
    if (feedback.inlineComments.length > 0) {
      parts.push("## Inline comments");
      for (const c of feedback.inlineComments) {
        parts.push(`- Line ${c.line} (near "${c.nearText}"): ${c.comment}`);
      }
      parts.push("");
    }
    const message = parts.join("\n");

    expect(message).toContain("## Inline comments");
    expect(message).toContain('Line 5 (near "Fix the heading"): also fix indentation');
    expect(message).toContain('Line 12 (near "Add tests"): use vitest not jest');
  });

  it("builds structured feedback with @claude markers", () => {
    const feedback = {
      inlineComments: [],
      claudeMarkers: [{ line: 8, nearText: "word-wrapping", text: "also handle signatures" }],
      diff: null as string | null,
      generalComment: "Please check edge cases",
    };

    const parts: string[] = ["# Plan Feedback\n"];
    if (feedback.claudeMarkers.length > 0) {
      parts.push("## @claude markers");
      for (const m of feedback.claudeMarkers) {
        parts.push(`- Line ${m.line} (near "${m.nearText}"): ${m.text}`);
      }
      parts.push("");
    }
    if (feedback.generalComment) {
      parts.push("## General comment");
      parts.push(feedback.generalComment);
    }
    const message = parts.join("\n");

    expect(message).toContain("## @claude markers");
    expect(message).toContain("also handle signatures");
    expect(message).toContain("## General comment");
    expect(message).toContain("Please check edge cases");
  });

  it("builds feedback with diff", () => {
    const feedback = {
      inlineComments: [],
      claudeMarkers: [],
      diff: "--- original\n+++ edited\n@@ -1,1 +1,1 @@\n-old line\n+new line",
      generalComment: null as string | null,
    };

    const parts: string[] = ["# Plan Feedback\n"];
    if (feedback.diff) {
      parts.push("## Changes requested");
      parts.push(feedback.diff);
      parts.push("");
    }
    const message = parts.join("\n");

    expect(message).toContain("## Changes requested");
    expect(message).toContain("-old line");
    expect(message).toContain("+new line");
  });

  it("empty feedback produces minimal message", () => {
    const feedback = {
      inlineComments: [],
      claudeMarkers: [],
      diff: null as string | null,
      generalComment: null as string | null,
    };

    const parts: string[] = ["# Plan Feedback\n"];
    if (feedback.inlineComments.length > 0) parts.push("## Inline comments");
    if (feedback.claudeMarkers.length > 0) parts.push("## @claude markers");
    if (feedback.diff) parts.push("## Changes requested");
    if (feedback.generalComment) parts.push("## General comment");
    const message = parts.join("\n");

    expect(message).toBe("# Plan Feedback\n");
  });
});
