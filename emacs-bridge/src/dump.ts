import { readFileSync, writeFileSync, existsSync, mkdirSync } from "fs";
import { join } from "path";
import { log } from "./log.js";

// --- Fixture dump mode ---
// When CLAUDE_GRAVITY_DUMP_DIR is set, save raw input and enriched output
// as JSON files for replay testing. Files are named {seq}__{event}__{suffix}.json
// where seq is a zero-padded counter preserving event ordering.

export function nextDumpSeq(dumpDir: string): number {
  if (!existsSync(dumpDir)) mkdirSync(dumpDir, { recursive: true });
  const counterFile = join(dumpDir, "_counter.txt");
  let counter = 0;
  try { counter = parseInt(readFileSync(counterFile, "utf-8").trim(), 10) || 0; } catch {}
  counter++;
  writeFileSync(counterFile, String(counter));
  return counter;
}

export function writeDumpFile(dumpDir: string, seq: number, eventName: string, suffix: string, data: any): void {
  try {
    const filename = `${String(seq).padStart(4, "0")}__${eventName}__${suffix}.json`;
    writeFileSync(join(dumpDir, filename), JSON.stringify(data, null, 2) + "\n");
  } catch (e) {
    log(`writeDumpFile error: ${e}`, 'error');
  }
}
