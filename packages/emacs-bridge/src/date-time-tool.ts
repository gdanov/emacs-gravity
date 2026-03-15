import { Type } from "@sinclair/typebox";
import type { AgentTool, AgentToolResult } from "@mariozechner/pi-agent-core";
import { log } from "./log.js";

const dateTimeSchema = Type.Object({
  operation: Type.Union([
    Type.Literal("now"),
    Type.Literal("format"),
    Type.Literal("add"),
    Type.Literal("subtract"),
    Type.Literal("diff"),
  ]),
  locale: Type.Optional(Type.String()),
  timezone: Type.Optional(Type.String()),
  format: Type.Optional(Type.String()),
  date: Type.Optional(Type.String()),
  amount: Type.Optional(Type.Number()),
  unit: Type.Optional(Type.Union([
    Type.Literal("second"),
    Type.Literal("minute"),
    Type.Literal("hour"),
    Type.Literal("day"),
    Type.Literal("week"),
    Type.Literal("month"),
    Type.Literal("year"),
  ])),
});

type DateTimeInput = typeof dateTimeSchema.static;
type DateTimeDetails = Record<string, unknown>;

const dateTimeTool: AgentTool<typeof dateTimeSchema, DateTimeDetails> = {
  name: "date_time",
  description: "Get current date/time, format dates, perform date arithmetic, and calculate time differences. Use this tool when you need to work with dates, times, or time zones.",
  parameters: dateTimeSchema,
  label: "DateTime",
  execute: async (toolCallId, params): Promise<AgentToolResult<DateTimeDetails>> => {
    log(`[date-time] operation: ${params.operation}`, "debug");

    try {
      const locale = params.locale ?? Intl.DateTimeFormat().resolvedOptions().locale;
      const timezone = params.timezone ?? Intl.DateTimeFormat().resolvedOptions().timeZone;

      const fmt = new Intl.DateTimeFormat(locale, { timeZone: timezone });

      let result: string;
      let details: DateTimeDetails = { locale, timezone };

      switch (params.operation) {
        case "now": {
          const now = new Date();
          const format = params.format ?? "medium";
          result = formatDateTime(now, format, locale, timezone);
          details = { ...details, iso: now.toISOString(), timestamp: now.getTime() };
          break;
        }

        case "format": {
          if (!params.date) {
            throw new Error("'date' parameter is required for 'format' operation");
          }
          const d = parseDate(params.date);
          const format = params.format ?? "medium";
          result = formatDateTime(d, format, locale, timezone);
          details = { ...details, input: params.date, iso: d.toISOString() };
          break;
        }

        case "add": {
          if (!params.date || params.amount === undefined || !params.unit) {
            throw new Error("'date', 'amount', and 'unit' parameters are required for 'add' operation");
          }
          const d = parseDate(params.date);
          const resultDate = addTime(d, params.amount, params.unit);
          result = resultDate.toISOString();
          details = {
            ...details,
            input: params.date,
            amount: params.amount,
            unit: params.unit,
            result: resultDate.toISOString(),
          };
          break;
        }

        case "subtract": {
          if (!params.date || params.amount === undefined || !params.unit) {
            throw new Error("'date', 'amount', and 'unit' parameters are required for 'subtract' operation");
          }
          const d = parseDate(params.date);
          const resultDate = addTime(d, -params.amount, params.unit);
          result = resultDate.toISOString();
          details = {
            ...details,
            input: params.date,
            amount: params.amount,
            unit: params.unit,
            result: resultDate.toISOString(),
          };
          break;
        }

        case "diff": {
          if (!params.date) {
            throw new Error("'date' parameter is required for 'diff' operation");
          }
          const now = new Date();
          const d = parseDate(params.date);
          const diffMs = now.getTime() - d.getTime();
          const diffSeconds = Math.floor(diffMs / 1000);
          const diffMinutes = Math.floor(diffSeconds / 60);
          const diffHours = Math.floor(diffMinutes / 60);
          const diffDays = Math.floor(diffHours / 24);

          result = JSON.stringify({
            milliseconds: diffMs,
            seconds: diffSeconds,
            minutes: diffMinutes,
            hours: diffHours,
            days: diffDays,
            human: fmt.format(d),
          });
          details = {
            ...details,
            input: params.date,
            iso: d.toISOString(),
            milliseconds: diffMs,
          };
          break;
        }

        default:
          throw new Error(`Unknown operation: ${(params as any).operation}`);
      }

      return {
        content: [{ type: "text", text: result }],
        details,
      };
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      log(`[date-time] error: ${message}`, "error");
      return {
        content: [{ type: "text", text: `Error: ${message}` }],
        details: { error: message },
      };
    }
  },
};

function parseDate(dateStr: string): Date {
  const d = new Date(dateStr);
  if (isNaN(d.getTime())) {
    throw new Error(`Invalid date: ${dateStr}. Use ISO 8601 format (e.g., "2024-01-15T10:30:00Z")`);
  }
  return d;
}

function addTime(date: Date, amount: number, unit: string): Date {
  const result = new Date(date);
  switch (unit) {
    case "second":
      result.setSeconds(result.getSeconds() + amount);
      break;
    case "minute":
      result.setMinutes(result.getMinutes() + amount);
      break;
    case "hour":
      result.setHours(result.getHours() + amount);
      break;
    case "day":
      result.setDate(result.getDate() + amount);
      break;
    case "week":
      result.setDate(result.getDate() + amount * 7);
      break;
    case "month":
      result.setMonth(result.getMonth() + amount);
      break;
    case "year":
      result.setFullYear(result.getFullYear() + amount);
      break;
    default:
      throw new Error(`Unknown unit: ${unit}`);
  }
  return result;
}

function formatDateTime(date: Date, format: string, locale: string, timezone: string): string {
  const options: Intl.DateTimeFormatOptions = {};

  switch (format) {
    case "full":
      options.dateStyle = "full";
      options.timeStyle = "full";
      break;
    case "long":
      options.dateStyle = "long";
      options.timeStyle = "long";
      break;
    case "medium":
      options.dateStyle = "medium";
      options.timeStyle = "medium";
      break;
    case "short":
      options.dateStyle = "short";
      options.timeStyle = "short";
      break;
    case "date":
      options.dateStyle = "long";
      break;
    case "time":
      options.timeStyle = "long";
      break;
    default:
      options.dateStyle = "medium";
      options.timeStyle = "medium";
  }

  return new Intl.DateTimeFormat(locale, { ...options, timeZone: timezone }).format(date);
}

export function createDateTimeTool(): AgentTool<typeof dateTimeSchema, DateTimeDetails> {
  return dateTimeTool;
}

export { dateTimeTool };
