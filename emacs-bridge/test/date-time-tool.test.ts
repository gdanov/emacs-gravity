import { describe, it, expect } from "vitest";
import { createDateTimeTool } from "../src/date-time-tool";

describe("date-time tool", () => {
  const tool = createDateTimeTool();

  it("should have correct name and description", () => {
    expect(tool.name).toBe("date_time");
    expect(tool.description).toContain("date/time");
  });

  it("should return current time for 'now' operation", async () => {
    const result = await tool.execute("call-1", {
      operation: "now",
    });

    expect(result.content).toHaveLength(1);
    expect(result.content[0].type).toBe("text");
    expect(result.details.locale).toBeDefined();
    expect(result.details.timezone).toBeDefined();
    expect(result.details.iso).toBeDefined();
    expect(result.details.timestamp).toBeDefined();
  });

  it("should format a date for 'format' operation", async () => {
    const result = await tool.execute("call-2", {
      operation: "format",
      date: "2024-01-15T10:30:00Z",
      format: "long",
      locale: "en-US",
    });

    expect(result.content[0].type).toBe("text");
    expect(result.details.input).toBe("2024-01-15T10:30:00Z");
    expect(result.details.iso).toBeDefined();
  });

  it("should add time for 'add' operation", async () => {
    const result = await tool.execute("call-3", {
      operation: "add",
      date: "2024-01-15T10:00:00Z",
      amount: 1,
      unit: "day",
    });

    expect(result.content[0].type).toBe("text");
    const expected = new Date("2024-01-16T10:00:00Z").toISOString();
    expect(result.content[0].text).toBe(expected);
  });

  it("should subtract time for 'subtract' operation", async () => {
    const result = await tool.execute("call-4", {
      operation: "subtract",
      date: "2024-01-15T10:00:00Z",
      amount: 2,
      unit: "hour",
    });

    expect(result.content[0].type).toBe("text");
    const expected = new Date("2024-01-15T08:00:00Z").toISOString();
    expect(result.content[0].text).toBe(expected);
  });

  it("should calculate diff for 'diff' operation", async () => {
    const result = await tool.execute("call-5", {
      operation: "diff",
      date: "2024-01-10T10:00:00Z",
    });

    expect(result.content[0].type).toBe("text");
    const parsed = JSON.parse(result.content[0].text);
    expect(parsed.days).toBeDefined();
    expect(parsed.hours).toBeDefined();
    expect(parsed.minutes).toBeDefined();
  });

  it("should use locale and timezone options", async () => {
    const result = await tool.execute("call-6", {
      operation: "now",
      locale: "bg-BG",
      timezone: "Europe/Sofia",
    });

    expect(result.details.locale).toBe("bg-BG");
    expect(result.details.timezone).toBe("Europe/Sofia");
  });

  it("should error on invalid date", async () => {
    const result = await tool.execute("call-7", {
      operation: "format",
      date: "not-a-date",
    });

    expect(result.details.error).toBeDefined();
    expect(result.content[0].text).toContain("Error");
  });

  it("should error when required params missing for add", async () => {
    const result = await tool.execute("call-8", {
      operation: "add",
      date: "2024-01-15T10:00:00Z",
      amount: 1,
      // missing unit
    });

    expect(result.details.error).toBeDefined();
  });
});
