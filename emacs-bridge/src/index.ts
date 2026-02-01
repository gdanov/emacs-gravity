import { createConnection } from "net";
import { readFileSync } from "fs";
import { homedir } from "os";
import { join } from "path";

// Helper to send data to Emacs socket
async function sendToEmacs(eventName: string, payload: any) {
  const socketPath = join(
    homedir(),
    "work/playground/emacs-gravity/claude-gravity.sock",
  );

  return new Promise<void>((resolve, reject) => {
    const client = createConnection(socketPath);

    client.on("connect", () => {
      const message =
        JSON.stringify({ event: eventName, data: payload }) + "\n";
      client.write(message);
      client.end();
    });

    client.on("error", (err) => {
      // Fail silently to avoid blocking Claude if Emacs is down
      resolve();
    });

    client.on("close", () => {
      resolve();
    });
  });
}

async function main() {
  try {
    const eventName = process.argv[2]; // e.g., "PreToolUse"

    // Read STDIN
    // Note: readFileSync(0) reads from stdin file descriptor
    let inputData = {};
    try {
      const stdinBuffer = readFileSync(0);
      if (stdinBuffer.length > 0) {
        inputData = JSON.parse(stdinBuffer.toString());
      }
    } catch (e) {
      // Ignore stdin read errors (e.g. if no input provided)
    }

    // Send to Emacs
    await sendToEmacs(eventName, inputData);

    // Always output valid JSON to stdout as expected by Claude Code
    console.log(JSON.stringify({}));
  } catch (error) {
    // Log error to stderr but output valid JSON to stdout to not break Claude
    // console.error("Emacs Bridge Logic Error:", error);
    console.log(JSON.stringify({}));
  }
}

main();
