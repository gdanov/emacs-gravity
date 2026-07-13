// inert.ts — Kill-switch for the pi extension emitter.
//
// The extension renders itself a no-op (does NOT register any pi.on(...)
// handlers, does NOT touch `net`) when either env var is set:
//   - GRAVITY_DRIVER === "1"          (RPC driver owns this session — #22)
//   - GRAVITY_PI_EMITTER === "off"    (explicit opt-out)
//
// Exported as a pure function so unit tests don't need to spawn `pi`.
// `env` defaults to process.env so call sites don't have to thread it
// through, but every test passes an explicit object so we never mutate
// the real process.env here.

/** Default kill-switch env var for the gravity RPC driver. */
export const GRAVITY_DRIVER_ENV = "GRAVITY_DRIVER";

/** Default kill-switch env var for explicit emitter opt-out. */
export const GRAVITY_PI_EMITTER_ENV = "GRAVITY_PI_EMITTER";

/** Value that disables the emitter via GRAVITY_PI_EMITTER. */
export const GRAVITY_PI_EMITTER_OFF = "off";

/**
 * Returns true iff the emitter should be inert for this process.
 *
 * Pure function — no I/O, no module-load side effects, no network. Tests
 * inject an env object directly; production callers omit it to default to
 * `process.env`.
 */
export function isInert(env?: NodeJS.ProcessEnv): boolean {
  const source = env ?? process.env;
  if (source[GRAVITY_DRIVER_ENV] === "1") return true;
  if (source[GRAVITY_PI_EMITTER_ENV] === GRAVITY_PI_EMITTER_OFF) return true;
  return false;
}