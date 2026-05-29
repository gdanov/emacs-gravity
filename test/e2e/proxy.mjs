// Socket-spy proxy for the e2e harness.
//
// Emacs connects here (GRAVITY_TERMINAL_SOCK -> this path). The proxy
// forwards bytes verbatim to the real gravity-server terminal socket and
// records every newline-delimited JSON message in BOTH directions to a
// JSONL log the scenario runner asserts on. Raw chunks are piped through
// untouched (partial frames preserved on the wire); line parsing is for
// logging only.
//
// Usage: node proxy.mjs <listenSockPath> <upstreamSockPath> <jsonlLogPath>

import { createServer, createConnection } from "net";
import { appendFileSync, unlinkSync, existsSync } from "fs";

const [, , LISTEN, UPSTREAM, LOG] = process.argv;
if (!LISTEN || !UPSTREAM || !LOG) {
  console.error("usage: proxy.mjs <listen> <upstream> <log>");
  process.exit(2);
}

function logLine(dir, line) {
  const t = line.trim();
  if (!t) return;
  let parsed;
  try {
    parsed = JSON.parse(t);
  } catch {
    parsed = { _unparsed: t };
  }
  appendFileSync(
    LOG,
    JSON.stringify({ dir, ts: Date.now(), msg: parsed }) + "\n",
  );
}

// Per-connection, per-direction line accumulator → logLine on each '\n'.
function makeFramer(dir) {
  let buf = "";
  return (chunk) => {
    buf += chunk.toString("utf8");
    let i;
    while ((i = buf.indexOf("\n")) !== -1) {
      logLine(dir, buf.slice(0, i));
      buf = buf.slice(i + 1);
    }
  };
}

if (existsSync(LISTEN)) {
  try { unlinkSync(LISTEN); } catch { /* ignore */ }
}

const server = createServer((downstream) => {
  const upstream = createConnection(UPSTREAM);
  const c2s = makeFramer("c2s"); // Emacs/client -> server
  const s2c = makeFramer("s2c"); // server -> client

  downstream.on("data", (c) => { c2s(c); upstream.write(c); });
  upstream.on("data", (c) => { s2c(c); downstream.write(c); });

  const closeBoth = () => {
    downstream.destroy();
    upstream.destroy();
  };
  downstream.on("close", closeBoth);
  upstream.on("close", closeBoth);
  downstream.on("error", closeBoth);
  upstream.on("error", closeBoth);
});

server.on("error", (e) => {
  console.error("proxy server error:", e.message);
  process.exit(1);
});

server.listen(LISTEN, () => {
  process.stderr.write(`proxy: ${LISTEN} -> ${UPSTREAM} (log ${LOG})\n`);
});

for (const sig of ["SIGINT", "SIGTERM"]) {
  process.on(sig, () => {
    try { server.close(); } catch { /* ignore */ }
    try { if (existsSync(LISTEN)) unlinkSync(LISTEN); } catch { /* ignore */ }
    process.exit(0);
  });
}
