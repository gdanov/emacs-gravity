// errors.ts — Tagged error types for gravity-server

import { Data } from "effect";

export class FileReadError extends Data.TaggedError("FileReadError")<{ path: string; cause: unknown }> {}
export class FileWriteError extends Data.TaggedError("FileWriteError")<{ path: string; cause: unknown }> {}
