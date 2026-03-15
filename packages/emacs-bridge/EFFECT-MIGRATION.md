# Effect.ts v4 Migration — Progress

## Completed

### Phase 0: esbuild bundling + Effect install
- Added `effect@4.0.0-beta.9` and `esbuild@^0.27.4` to dependencies
- Created `build.mjs` with esbuild config (ESM, node platform, external Node builtins + optional heavy deps)
- Updated all 12 hook scripts: `tsx src/index.ts` → `node dist/index.mjs`
- Cold start: **63ms** without socket (vs 1.7s with tsx — 27x faster)
- With socket roundtrip: **548ms** (vs 1.7s — 3x faster)

### Phase 1: Service interfaces + tagged errors
- `src/services/errors.ts` — 7 tagged error classes (`StdinReadError`, `FileReadError`, `SocketError`, etc.)
- `src/services/process-io.ts` — `ProcessIO` service (stdin, stdout, argv, env) with Live + Test layers
- `src/services/fs.ts` — `Fs` service (readFile, writeFile, exists, stat, readBytes, mkdirp) with Live + Test layers
- `src/services/emacs-socket.ts` — `EmacsSocket` service (send, sendAndWait, socketExists) with Live + Test layers
- `src/services/config.ts` — `BridgeConfig` service (resolved once at startup from env vars + settings.json)
- `src/services/logger.ts` — Effect Logger writing to `/tmp/emacs-bridge.log`
- All using Effect v4 `ServiceMap.Service("Name")` pattern

### Phase 2: index.ts rewritten as Effect pipeline
- `main()` rewritten as `Effect.gen` generator
- Config injection via `BridgeConfig` service (replaces scattered `process.env` reads)
- Stdin parsing via `ProcessIO` service
- Effect logging replaces `log()` calls (still uses old `log()` for process exit callbacks)
- Enrichment still imperative (Phases 3-4), wrapped in `Effect.promise()` for async `enrichStop`
- Routing logic preserved exactly
- Top-level safety: `Effect.catch` wraps entire program, always outputs `{}`
- Layer composition: `ProcessIOLive + FsLive + LoggerLive → BridgeConfigLive`

### Phase 5: Test layers (infrastructure only)
- `test/test-layers.ts` — `testLayers()` factory providing all services with in-memory implementations
- Integration tests not yet written

---

## Remaining

### Phase 3: Convert I/O modules to use services

Each module replaces direct `fs.*` / `process.*` calls with the corresponding Effect service. Functions return `Effect<T, E, Fs | ProcessIO>` instead of raw values.

**socket.ts** — Wrap `net.Socket` in `Effect.callback`. The `EmacsSocket` service already does this; `socket.ts` becomes a thin re-export or is deleted once `index.ts` uses `EmacsSocket` directly.

**enrichment.ts** — `readTail`, `readHead`, `extractPrecedingContent`, etc. use `Fs` service:
```ts
const readTail = (path: string, maxBytes: number) =>
  Effect.gen(function* () {
    const fs = yield* Effect.service(Fs);
    const { size } = yield* fs.stat(path);
    const offset = Math.max(0, size - maxBytes);
    const buf = yield* fs.readBytes(path, offset, Math.min(maxBytes, size));
    // skip partial first line, return string
  })
```

**agent-state.ts** — `readAgentState` / `writeAgentState` use `Fs` service. Biggest testability win: tests provide in-memory map instead of temp files.

**enrich.ts** — Each `enrich*` function returns `Effect<HookData, never, Fs>`. Mechanical conversion: replace `try/catch` with `Effect.catch(() => Effect.succeed(data))`.

### Phase 4: Convert leaf modules

**safe-bash.ts** — Already pure logic. Only change: lift `process.env.CLAUDE_GRAVITY_NO_AUTO_APPROVE` check to caller (already done in `index.ts`). Keep `isSafeBashCommand` as pure function.

**dump.ts** — `nextDumpSeq` and `writeDumpFile` use `Fs` service.

**log.ts** — Delete. Replaced by Effect Logger in Phase 1. Callers switch from `log("msg", "warn")` to `Effect.logWarning("msg")`.

### Phase 5: Integration tests

Write integration tests for the main `program` flow using test layers:

```ts
it("SessionStart enriches and sends to Emacs", async () => {
  const { layers, stdoutCapture, socketSent } = testLayers({
    stdin: JSON.stringify({ session_id: "abc", cwd: "/tmp" }),
    argv: ["node", "index.mjs", "SessionStart"],
    configOverrides: { socketPath: "/tmp/test.sock" },
  });
  await Effect.runPromise(Effect.provide(program, layers));
  expect(socketSent[0]).toMatchObject({ event: "SessionStart" });
  expect(JSON.parse(stdoutCapture[0])).toEqual({});
});
```

Test scenarios:
- SessionStart → enriches + sends fire-and-forget + outputs `{}`
- Stop → enriches with trailing text + sends + outputs `{}`
- PermissionRequest (safe bash) → auto-approves + sends notification + outputs allow
- PermissionRequest (unsafe) → sends bidirectional + waits for response + applies deny-as-approve workaround
- AskUserQuestionIntercept → sends as PreToolUse + waits + outputs response
- Socket not found → early exit with `{}`
- Ignored session → passes through to TUI
- Stdin parse error → outputs `{}`

---

## Effect v4 API Notes

Effect v4 beta differs significantly from v3. Key patterns used:

| v3 | v4 |
|----|-----|
| `Context.Tag("Name")` | `ServiceMap.Service<T>("Name")` |
| `Effect.async(resume => ...)` | `Effect.callback(resume => ...)` |
| `Effect.catchAll(...)` | `Effect.catch(...)` |
| `Logger.replace(Logger.defaultLogger, ...)` | `Logger.layer([myLogger])` |
| `logLevel.label` | `logLevel` is a string directly |
| `import { Context } from "effect"` | `import { ServiceMap } from "effect"` |

Layer composition:
```ts
const MainLive = Layer.mergeAll(ProcessIOLive, FsLive, LoggerLive);
const ConfigLayer = Layer.provide(BridgeConfigLive, MainLive);  // BridgeConfig needs ProcessIO + Fs
const FullLayer = Layer.mergeAll(MainLive, ConfigLayer);
```

---

## Verification Checklist

1. `npx tsc --noEmit` — type checks clean
2. `npm run build` — esbuild bundles without errors
3. `npx vitest run` — all tests pass (233 local, pi-session network tests may flake)
4. `time echo '{}' | CLAUDE_GRAVITY_SOCK=/tmp/x node dist/index.mjs Stop` — ≤ 100ms
5. Manual: start Claude Code with plugin, verify hooks fire, Emacs receives events
6. Check `/tmp/emacs-bridge.log` for errors
7. Sync cache: `cp emacs-bridge/dist/index.mjs ~/.claude/plugins/cache/local-emacs-marketplace/emacs-bridge/2.0.0/dist/`
