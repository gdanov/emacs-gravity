import { Data } from "effect";

export class StdinReadError extends Data.TaggedError("StdinReadError")<{ cause: unknown }> {}
export class StdinParseError extends Data.TaggedError("StdinParseError")<{ raw: string; cause: unknown }> {}
export class SocketNotFoundError extends Data.TaggedError("SocketNotFoundError")<{ path: string }> {}
export class SocketError extends Data.TaggedError("SocketError")<{ cause: unknown }> {}
export class SocketTimeoutError extends Data.TaggedError("SocketTimeoutError")<{ timeoutMs: number }> {}
export class FileReadError extends Data.TaggedError("FileReadError")<{ path: string; cause: unknown }> {}
export class FileWriteError extends Data.TaggedError("FileWriteError")<{ path: string; cause: unknown }> {}
