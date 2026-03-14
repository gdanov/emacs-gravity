import { join, dirname } from "path";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { log } from "./log.js";

// --- Fixture dump mode ---
// Saves raw input and enriched output as JSON files alongside Claude's transcript.
// Directory: {transcript_dir}/gravity/dumps/
// Files are named {seq}__{event}__{suffix}.json

function getDumpDir(transcriptPath: string | undefined): string | undefined {
  if (!transcriptPath) return undefined;
  const transcriptDir = dirname(transcriptPath);
  return join(transcriptDir, "gravity", "dumps");
}

export function nextDumpSeq(transcriptPath: string | undefined): number | undefined {
  const dumpDir = getDumpDir(transcriptPath);
  if (!dumpDir) return undefined;
  
  if (!existsSync(dumpDir)) mkdirSync(dumpDir, { recursive: true });
  const counterFile = join(dumpDir, "_counter.txt");
  let counter = 0;
  try { counter = parseInt(readFileSync(counterFile, "utf-8").trim(), 10) || 0; } catch {}
  counter++;
  writeFileSync(counterFile, String(counter));
  return counter;
}

export function writeDumpFile(transcriptPath: string | undefined, seq: number | undefined, eventName: string, suffix: string, data: any): void {
  if (!transcriptPath || seq === undefined) return;
  
  const dumpDir = getDumpDir(transcriptPath);
  if (!dumpDir) return;
  
  try {
    if (!existsSync(dumpDir)) mkdirSync(dumpDir, { recursive: true });
    const filename = `${String(seq).padStart(4, "0")}__${eventName}__${suffix}.json`;
    writeFileSync(join(dumpDir, filename), JSON.stringify(data, null, 2) + "\n");
  } catch (e) {
    log(`writeDumpFile error: ${e}`, 'error');
  }
}
