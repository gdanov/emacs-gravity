import { Effect, Layer, ServiceMap } from "effect";
import {
  readFileSync,
  writeFileSync,
  existsSync,
  statSync,
  openSync,
  readSync,
  closeSync,
  mkdirSync,
} from "fs";
import { FileReadError, FileWriteError } from "./errors.js";

export interface FsService {
  readonly readFile: (path: string) => Effect.Effect<string, FileReadError>;
  readonly writeFile: (path: string, data: string) => Effect.Effect<void, FileWriteError>;
  readonly exists: (path: string) => Effect.Effect<boolean>;
  readonly stat: (path: string) => Effect.Effect<{ size: number }, FileReadError>;
  readonly readBytes: (path: string, offset: number, length: number) => Effect.Effect<Buffer, FileReadError>;
  readonly mkdirp: (path: string) => Effect.Effect<void, FileWriteError>;
}

export const Fs = ServiceMap.Service<FsService>("Fs");

export const FsLive = Layer.succeed(Fs, {
  readFile: (path: string) =>
    Effect.try({
      try: () => readFileSync(path, "utf-8"),
      catch: (cause) => new FileReadError({ path, cause }),
    }),

  writeFile: (path: string, data: string) =>
    Effect.try({
      try: () => { writeFileSync(path, data, "utf-8"); },
      catch: (cause) => new FileWriteError({ path, cause }),
    }),

  exists: (path: string) =>
    Effect.sync(() => existsSync(path)),

  stat: (path: string) =>
    Effect.try({
      try: () => {
        const s = statSync(path);
        return { size: s.size };
      },
      catch: (cause) => new FileReadError({ path, cause }),
    }),

  readBytes: (path: string, offset: number, length: number) =>
    Effect.try({
      try: () => {
        const fd = openSync(path, "r");
        const buf = Buffer.alloc(length);
        readSync(fd, buf, 0, length, offset);
        closeSync(fd);
        return buf;
      },
      catch: (cause) => new FileReadError({ path, cause }),
    }),

  mkdirp: (path: string) =>
    Effect.try({
      try: () => { mkdirSync(path, { recursive: true }); },
      catch: (cause) => new FileWriteError({ path, cause }),
    }),
});

export const FsTest = (fixtures: Record<string, string>) => {
  const files = new Map(Object.entries(fixtures));
  return Layer.succeed(Fs, {
    readFile: (path: string) =>
      files.has(path)
        ? Effect.succeed(files.get(path)!)
        : Effect.fail(new FileReadError({ path, cause: new Error("ENOENT") })),

    writeFile: (path: string, data: string) =>
      Effect.sync(() => { files.set(path, data); }),

    exists: (path: string) =>
      Effect.succeed(files.has(path)),

    stat: (path: string) =>
      files.has(path)
        ? Effect.succeed({ size: files.get(path)!.length })
        : Effect.fail(new FileReadError({ path, cause: new Error("ENOENT") })),

    readBytes: (path: string, offset: number, length: number) =>
      files.has(path)
        ? Effect.succeed(Buffer.from(files.get(path)!.slice(offset, offset + length)))
        : Effect.fail(new FileReadError({ path, cause: new Error("ENOENT") })),

    mkdirp: (_path: string) =>
      Effect.void,
  });
};
