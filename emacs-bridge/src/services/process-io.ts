import { Effect, Layer, ServiceMap } from "effect";
import { readFileSync } from "fs";
import { StdinReadError } from "./errors.js";

export interface ProcessIOService {
  readonly readStdin: () => Effect.Effect<string, StdinReadError>;
  readonly writeStdout: (data: string) => Effect.Effect<void>;
  readonly writeStdoutRaw: (data: string) => Effect.Effect<void, never, never>;
  readonly getArg: (index: number) => Effect.Effect<string | undefined>;
  readonly getEnv: (key: string) => Effect.Effect<string | undefined>;
  readonly exit: (code: number) => Effect.Effect<never>;
}

export const ProcessIO = ServiceMap.Service<ProcessIOService>("ProcessIO");

export const ProcessIOLive = Layer.succeed(ProcessIO, {
  readStdin: () =>
    Effect.try({
      try: () => {
        const buf = readFileSync(0);
        return buf.length > 0 ? buf.toString() : "";
      },
      catch: (cause) => new StdinReadError({ cause }),
    }),

  writeStdout: (data: string) =>
    Effect.sync(() => {
      process.stdout.write(data);
    }),

  writeStdoutRaw: (data: string) =>
    Effect.sync(() => {
      process.stdout.write(data);
    }),

  getArg: (index: number) =>
    Effect.sync(() => process.argv[index]),

  getEnv: (key: string) =>
    Effect.sync(() => process.env[key]),

  exit: (code: number) =>
    Effect.sync(() => { process.exit(code); }) as Effect.Effect<never>,
});

export const ProcessIOTest = (opts: {
  stdin?: string;
  argv?: string[];
  env?: Record<string, string>;
  stdoutCapture?: string[];
}) =>
  Layer.succeed(ProcessIO, {
    readStdin: () => Effect.succeed(opts.stdin ?? ""),
    writeStdout: (data: string) =>
      Effect.sync(() => { opts.stdoutCapture?.push(data); }),
    writeStdoutRaw: (data: string) =>
      Effect.sync(() => { opts.stdoutCapture?.push(data); }),
    getArg: (index: number) =>
      Effect.succeed(opts.argv?.[index]),
    getEnv: (key: string) =>
      Effect.succeed(opts.env?.[key]),
    exit: (_code: number) =>
      Effect.die("exit called in test") as Effect.Effect<never>,
  });
