// mermaid-ascii.test.ts — Tests for mermaid ASCII rendering
// ⚠️ VIBECODED EXPERIMENT — This code was generated in a single AI-assisted session.
// It may contain bugs, design issues, or unexpected behavior. Use with caution.
// Tests cover:
// - Basic flowchart rendering
// - Sequence diagram rendering
// - Class diagram rendering
// - Error handling
// - Unicode vs ASCII output

import { describe, it, expect } from "vitest";
import {
  renderMermaidToASCII,
  renderMermaidToANSI,
  ERROR_MESSAGES,
} from "../src/handlers/mermaid-ascii.js";

describe("renderMermaidToASCII", () => {
  describe("flowcharts", () => {
    it("renders simple LR flowchart", () => {
      const source = `graph LR
    A --> B --> C`;
      const result = renderMermaidToASCII(source);

      // Should contain node labels
      expect(result).toContain("A");
      expect(result).toContain("B");
      expect(result).toContain("C");

      // Should contain connectors (either Unicode or ASCII arrows)
      expect(result).toMatch(/(-->|──►|──>|──▶)/);
    });

    it("renders TD flowchart with decision", () => {
      const source = `graph TD
    A[Start] --> B{Decision}
    B -->|Yes| C[Action]
    B -->|No| D[End]`;
      const result = renderMermaidToASCII(source);

      // Should contain all nodes
      expect(result).toContain("Start");
      expect(result).toContain("Decision");
      expect(result).toContain("Action");
      expect(result).toContain("End");

      // Should contain connectors
      expect(result).toContain("Yes");
      expect(result).toContain("No");
    });

    it("renders flowchart with subgraph", () => {
      const source = `graph TD
    subgraph Main
        A --> B
    end
    A --> C`;
      const result = renderMermaidToASCII(source);

      expect(result).toContain("Main");
      expect(result).toContain("A");
      expect(result).toContain("B");
      expect(result).toContain("C");
    });
  });

  describe("sequence diagrams", () => {
    it("renders basic sequence", () => {
      const source = `sequenceDiagram
    Alice->>Bob: Hello Bob
    Bob-->>Alice: Hi Alice`;
      const result = renderMermaidToASCII(source);

      // Should contain actor names
      expect(result).toContain("Alice");
      expect(result).toContain("Bob");

      // Should contain message
      expect(result).toContain("Hello");
    });

    it("renders sequence with loops", () => {
      const source = `sequenceDiagram
    loop Every minute
        Alice->>Bob: Ping
    end`;
      const result = renderMermaidToASCII(source);

      expect(result).toContain("Alice");
      expect(result).toContain("Bob");
      expect(result).toContain("loop");
    });
  });

  describe("class diagrams", () => {
    it("renders basic class", () => {
      const source = `classDiagram
    class Animal {
        +String name
        +int age
        +makeSound()
    }`;
      const result = renderMermaidToASCII(source);

      expect(result).toContain("Animal");
      expect(result).toContain("name");
      expect(result).toContain("age");
    });
  });

  describe("state diagrams", () => {
    it("renders basic state", () => {
      const source = `stateDiagram-v2
    [*] --> Idle
    Idle --> Running : start
    Running --> Idle : stop`;
      const result = renderMermaidToASCII(source);

      expect(result).toMatch(/(Idle|Running|\[.*\]|\[\*\])/);
    });
  });

  describe("error handling", () => {
    it("returns error for empty source", () => {
      const result = renderMermaidToASCII("");
      expect(result).toContain("error");
    });

    it("returns error for invalid syntax", () => {
      const result = renderMermaidToASCII("not a valid mermaid");
      expect(result).toContain("error");
    });

    it("returns error for malformed flowchart", () => {
      const result = renderMermaidToASCII("graph XXX\n    [");
      expect(result).toContain("error");
    });
  });

  describe("useAscii option", () => {
    it("renders with ASCII characters when useAscii=true", () => {
      const source = `graph LR
    A --> B`;
      const result = renderMermaidToASCII(source, { useAscii: true });

      // Should contain ASCII connectors
      expect(result).toMatch(/(\+---|->)/);
    });

    it("renders with Unicode box-drawing when useAscii=false", () => {
      const source = `graph LR
    A --> B`;
      const result = renderMermaidToASCII(source, { useAscii: false });

      // Should contain Unicode box-drawing characters
      expect(result).toMatch(/(┌|└|│|─|►)/);
    });

    it("defaults to Unicode when useAscii not specified", () => {
      const source = `graph LR
    A --> B`;
      const result = renderMermaidToASCII(source);

      // Should contain Unicode box-drawing (default)
      expect(result).toMatch(/(┌|└|│|─|►)/);
    });
  });

  describe("theme options", () => {
    it("accepts theme options without error", () => {
      const source = `graph LR
    A --> B`;
      const result = renderMermaidToASCII(source, {
        theme: {
          fg: "#ffffff",
          border: "#888888",
          line: "#444444",
          arrow: "#00ff00",
        },
      });

      // Should render successfully despite colors
      expect(result).toContain("A");
      expect(result).toContain("B");
    });
  });
});

describe("renderMermaidToANSI", () => {
  it("renders with ANSI color codes", () => {
    const source = `graph LR
    A --> B`;
    const result = renderMermaidToANSI(source);

    // Should contain ANSI escape codes
    expect(result).toMatch(/\x1b\[/);
  });

  it("renders basic diagram with colors", () => {
    const source = `graph LR
    A --> B --> C`;
    const result = renderMermaidToANSI(source);

    expect(result).toContain("A");
    expect(result).toContain("B");
    expect(result).toContain("C");
    expect(result).toMatch(/\x1b\[/); // Has ANSI codes
  });

  it("applies useAscii option", () => {
    const source = `graph LR
    A --> B`;
    const result = renderMermaidToANSI(source, { useAscii: true });

    expect(result).toMatch(/\x1b\[/); // Has ANSI codes
    expect(result).toMatch(/(\+---|->)/); // ASCII connectors
  });
});

describe("ERROR_MESSAGES", () => {
  it("contains expected error message keys", () => {
    expect(ERROR_MESSAGES.EMPTY_SOURCE).toBe("[Mermaid: empty source]");
    expect(ERROR_MESSAGES.INVALID_SYNTAX).toBe("[Mermaid: invalid syntax]");
    expect(ERROR_MESSAGES.UNSUPPORTED_TYPE).toBe("[Mermaid: unsupported diagram type]");
  });
});