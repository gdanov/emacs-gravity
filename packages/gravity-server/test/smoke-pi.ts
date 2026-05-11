// Standalone smoke test: drives a real pi subprocess through startPiDriver,
// sends one prompt, collects TranslationResult events, prints summary.
//
// Run from repo root:
//   npx -w packages/gravity-server tsx packages/gravity-server/test/smoke-pi.ts

import { startPiDriver } from "../src/pi-driver/mod.js";
import type { TranslationResult } from "../src/pi-driver/types.js";

const PROMPT = "Run `echo hello` and tell me what it printed. Then stop.";
const TIMEOUT_MS = 120_000;

const events: TranslationResult[] = [];
let stopReceived = false;

const driver = startPiDriver({
  cwd: process.cwd(),
  thinkingLevel: "low",
  onTranslation: (r) => {
    events.push(r);
    if (r.hookEvent === "Stop") stopReceived = true;
    process.stderr.write(
      `[evt] ${r.hookEvent.padEnd(22)} sid=${r.sessionId.slice(0, 12)} ` +
      `tool=${(r.hookData as Record<string, unknown>).tool_name ?? "-"}\n`,
    );
  },
  onLifecycle: (event, metadata) => {
    process.stderr.write(`[lifecycle] ${event} sessionId=${metadata?.sessionId}\n`);
  },
});

process.stderr.write(`[smoke] sending prompt: ${PROMPT}\n`);
await driver.prompt(PROMPT);

const start = Date.now();
while (!stopReceived && Date.now() - start < TIMEOUT_MS) {
  await new Promise((r) => setTimeout(r, 250));
}

await driver.stop();

const counts: Record<string, number> = {};
const sessionIds = new Set<string>();
const toolUseIds = new Set<string>();
for (const e of events) {
  counts[e.hookEvent] = (counts[e.hookEvent] ?? 0) + 1;
  sessionIds.add(e.sessionId);
  const tid = (e.hookData as Record<string, unknown>).tool_use_id;
  if (typeof tid === "string") toolUseIds.add(tid);
}

console.log(JSON.stringify({
  totalEvents: events.length,
  sessionIdCount: sessionIds.size,
  sessionIds: [...sessionIds],
  uniqueToolUseIds: [...toolUseIds],
  counts,
  stopReceived,
  timedOut: !stopReceived,
  // First and last event for sanity
  firstHookEvent: events[0]?.hookEvent ?? null,
  lastHookEvent: events.at(-1)?.hookEvent ?? null,
  // Stop's stop_text if present (pi's final reply)
  stopText: ((events.find((e) => e.hookEvent === "Stop")?.hookData as Record<string, unknown>)?.stop_text ?? null),
}, null, 2));

process.exit(stopReceived ? 0 : 1);
