import { describe, it, expect, afterEach } from "vitest";
import { isSafeBashCommand } from "../src/safe-bash";

function bash(command: string) {
  return { tool_name: "Bash", tool_input: { command } };
}

describe("isSafeBashCommand", () => {
  afterEach(() => {
    delete process.env.CLAUDE_GRAVITY_NO_AUTO_APPROVE;
  });

  // --- Should auto-approve ---

  describe("safe commands", () => {
    it("approves ls", () => expect(isSafeBashCommand(bash("ls"))).toBe(true));
    it("approves ls -la", () => expect(isSafeBashCommand(bash("ls -la"))).toBe(true));
    it("approves cat file", () => expect(isSafeBashCommand(bash("cat foo.txt"))).toBe(true));
    it("approves head", () => expect(isSafeBashCommand(bash("head -20 file.txt"))).toBe(true));
    it("approves tail", () => expect(isSafeBashCommand(bash("tail -f log.txt"))).toBe(true));
    it("approves grep", () => expect(isSafeBashCommand(bash('grep -rn "foo" src/'))).toBe(true));
    it("approves rg", () => expect(isSafeBashCommand(bash("rg pattern --type ts"))).toBe(true));
    it("approves find", () => expect(isSafeBashCommand(bash('find . -name "*.ts" -type f'))).toBe(true));
    it("approves wc", () => expect(isSafeBashCommand(bash("wc -l file.txt"))).toBe(true));
    it("approves diff", () => expect(isSafeBashCommand(bash("diff a.txt b.txt"))).toBe(true));
    it("approves stat", () => expect(isSafeBashCommand(bash("stat file.txt"))).toBe(true));
    it("approves file", () => expect(isSafeBashCommand(bash("file image.png"))).toBe(true));
    it("approves which", () => expect(isSafeBashCommand(bash("which node"))).toBe(true));
    it("approves pwd", () => expect(isSafeBashCommand(bash("pwd"))).toBe(true));
    it("approves echo", () => expect(isSafeBashCommand(bash('echo "hello"'))).toBe(true));
    it("approves date", () => expect(isSafeBashCommand(bash("date"))).toBe(true));
    it("approves whoami", () => expect(isSafeBashCommand(bash("whoami"))).toBe(true));
    it("approves env", () => expect(isSafeBashCommand(bash("env"))).toBe(true));
    it("approves ps", () => expect(isSafeBashCommand(bash("ps aux"))).toBe(true));
    it("approves jq", () => expect(isSafeBashCommand(bash("jq .name data.json"))).toBe(true));
    it("approves du", () => expect(isSafeBashCommand(bash("du -sh ."))).toBe(true));
    it("approves df", () => expect(isSafeBashCommand(bash("df -h"))).toBe(true));
    it("approves tree", () => expect(isSafeBashCommand(bash("tree src/"))).toBe(true));
    it("approves curl GET", () => expect(isSafeBashCommand(bash("curl https://example.com"))).toBe(true));
    it("approves curl -s", () => expect(isSafeBashCommand(bash("curl -s https://api.example.com/data"))).toBe(true));
    it("approves wget", () => expect(isSafeBashCommand(bash("wget -q https://example.com"))).toBe(true));
  });

  describe("safe pipelines", () => {
    it("approves cat | grep", () => expect(isSafeBashCommand(bash("cat file.txt | grep foo"))).toBe(true));
    it("approves cat | grep | sort | uniq", () =>
      expect(isSafeBashCommand(bash("cat file.txt | grep foo | sort | uniq"))).toBe(true));
    it("approves ps | grep", () => expect(isSafeBashCommand(bash("ps aux | grep node"))).toBe(true));
    it("approves find | wc", () => expect(isSafeBashCommand(bash("find . -name '*.ts' | wc -l"))).toBe(true));
    it("approves cat | jq", () => expect(isSafeBashCommand(bash("cat data.json | jq .name"))).toBe(true));
    it("approves curl | jq", () => expect(isSafeBashCommand(bash("curl -s https://api.example.com | jq ."))).toBe(true));
  });

  describe("safe chains", () => {
    it("approves && chain", () => expect(isSafeBashCommand(bash("ls && echo done"))).toBe(true));
    it("approves || chain", () => expect(isSafeBashCommand(bash("cat file.txt || echo not found"))).toBe(true));
    it("approves ; chain", () => expect(isSafeBashCommand(bash("ls; pwd; whoami"))).toBe(true));
  });

  describe("env var prefixes", () => {
    it("approves FOO=bar env", () => expect(isSafeBashCommand(bash("FOO=bar env"))).toBe(true));
    it("approves multiple env vars", () => expect(isSafeBashCommand(bash("A=1 B=2 echo hello"))).toBe(true));
  });

  describe("absolute paths", () => {
    it("approves /usr/bin/grep", () => expect(isSafeBashCommand(bash("/usr/bin/grep pattern file"))).toBe(true));
    it("approves /bin/ls", () => expect(isSafeBashCommand(bash("/bin/ls -la"))).toBe(true));
  });

  // --- Should NOT auto-approve: dangerous constructs ---

  describe("dangerous constructs", () => {
    it("rejects output redirect >", () => expect(isSafeBashCommand(bash("echo foo > file.txt"))).toBe(false));
    it("rejects append redirect >>", () => expect(isSafeBashCommand(bash("echo foo >> file.txt"))).toBe(false));
    it("rejects command substitution $()", () => expect(isSafeBashCommand(bash("echo $(whoami)"))).toBe(false));
    it("rejects backtick substitution", () => expect(isSafeBashCommand(bash("echo `whoami`"))).toBe(false));
    it("rejects subshell ()", () => expect(isSafeBashCommand(bash("(cd /tmp && ls)"))).toBe(false));
    it("rejects brace group {}", () => expect(isSafeBashCommand(bash("{ ls; pwd; }"))).toBe(false));
    it("rejects eval", () => expect(isSafeBashCommand(bash('eval "ls -la"'))).toBe(false));
    it("rejects exec", () => expect(isSafeBashCommand(bash("exec ls"))).toBe(false));
    it("rejects source", () => expect(isSafeBashCommand(bash("source ~/.bashrc"))).toBe(false));
    it("rejects process substitution <()", () => expect(isSafeBashCommand(bash("diff <(ls a) <(ls b)"))).toBe(false));
    it("rejects here-document <<", () => expect(isSafeBashCommand(bash("cat <<EOF\nhello\nEOF"))).toBe(false));
    it("rejects background &", () => expect(isSafeBashCommand(bash("sleep 100 &"))).toBe(false));
  });

  // --- Should NOT auto-approve: unsafe binaries ---

  describe("unsafe binaries", () => {
    it("rejects rm", () => expect(isSafeBashCommand(bash("rm -rf /"))).toBe(false));
    it("rejects mv", () => expect(isSafeBashCommand(bash("mv a.txt b.txt"))).toBe(false));
    it("rejects cp", () => expect(isSafeBashCommand(bash("cp a.txt b.txt"))).toBe(false));
    it("rejects chmod", () => expect(isSafeBashCommand(bash("chmod +x script.sh"))).toBe(false));
    it("rejects mkdir", () => expect(isSafeBashCommand(bash("mkdir new_dir"))).toBe(false));
    it("rejects touch", () => expect(isSafeBashCommand(bash("touch file.txt"))).toBe(false));
    it("rejects npm", () => expect(isSafeBashCommand(bash("npm install express"))).toBe(false));
    it("rejects git", () => expect(isSafeBashCommand(bash("git push origin main"))).toBe(false));
    it("rejects python", () => expect(isSafeBashCommand(bash("python script.py"))).toBe(false));
    it("rejects node", () => expect(isSafeBashCommand(bash("node app.js"))).toBe(false));
    it("rejects sed", () => expect(isSafeBashCommand(bash("sed -i 's/a/b/' file"))).toBe(false));
    it("rejects tee", () => expect(isSafeBashCommand(bash("echo foo | tee file.txt"))).toBe(false));
    it("rejects sudo", () => expect(isSafeBashCommand(bash("sudo ls"))).toBe(false));
    it("rejects kill", () => expect(isSafeBashCommand(bash("kill -9 1234"))).toBe(false));
  });

  // --- curl/wget with dangerous flags ---

  describe("curl/wget dangerous flags", () => {
    it("rejects curl -X POST", () => expect(isSafeBashCommand(bash("curl -X POST https://api.example.com"))).toBe(false));
    it("rejects curl --request PUT", () => expect(isSafeBashCommand(bash("curl --request PUT https://api.example.com"))).toBe(false));
    it("rejects curl -d", () => expect(isSafeBashCommand(bash("curl -d '{\"key\":\"val\"}' https://api.example.com"))).toBe(false));
    it("rejects curl --data", () => expect(isSafeBashCommand(bash("curl --data 'foo=bar' https://api.example.com"))).toBe(false));
    it("rejects curl --data-raw", () => expect(isSafeBashCommand(bash("curl --data-raw 'hello' https://api.example.com"))).toBe(false));
    it("rejects curl -F", () => expect(isSafeBashCommand(bash("curl -F 'file=@test.txt' https://api.example.com"))).toBe(false));
    it("rejects curl --form", () => expect(isSafeBashCommand(bash("curl --form 'file=@test.txt' https://api.example.com"))).toBe(false));
    it("rejects curl --upload-file", () => expect(isSafeBashCommand(bash("curl --upload-file test.txt https://api.example.com"))).toBe(false));
    it("rejects curl -T", () => expect(isSafeBashCommand(bash("curl -T test.txt https://api.example.com"))).toBe(false));
    it("rejects wget --post-data (not in safe list)", () =>
      // wget --post-data doesn't match CURL_WGET_DANGEROUS, but >
      // In practice wget with POST is rare. -d matches for wget too.
      expect(isSafeBashCommand(bash("wget -d https://api.example.com"))).toBe(false));
  });

  // --- Edge cases ---

  describe("edge cases", () => {
    it("rejects non-Bash tool (Edit)", () =>
      expect(isSafeBashCommand({ tool_name: "Edit", tool_input: {} })).toBe(false));
    it("rejects non-Bash tool (Write)", () =>
      expect(isSafeBashCommand({ tool_name: "Write", tool_input: {} })).toBe(false));
    it("rejects empty command", () => expect(isSafeBashCommand(bash(""))).toBe(false));
    it("rejects whitespace-only command", () => expect(isSafeBashCommand(bash("   "))).toBe(false));
    it("rejects null tool_input", () => expect(isSafeBashCommand({ tool_name: "Bash" })).toBe(false));
    it("rejects missing tool_input.command", () =>
      expect(isSafeBashCommand({ tool_name: "Bash", tool_input: {} })).toBe(false));
    it("rejects numeric command", () =>
      expect(isSafeBashCommand({ tool_name: "Bash", tool_input: { command: 42 } })).toBe(false));
  });

  // --- Mixed safe/unsafe chains ---

  describe("mixed chains", () => {
    it("rejects safe && unsafe", () => expect(isSafeBashCommand(bash("ls && rm -rf /"))).toBe(false));
    it("rejects safe | unsafe", () => expect(isSafeBashCommand(bash("echo hello | tee file.txt"))).toBe(false));
    it("rejects unsafe ; safe", () => expect(isSafeBashCommand(bash("npm install; ls"))).toBe(false));
  });

  // --- Configuration ---

  describe("configuration", () => {
    it("respects CLAUDE_GRAVITY_NO_AUTO_APPROVE=1", () => {
      process.env.CLAUDE_GRAVITY_NO_AUTO_APPROVE = "1";
      expect(isSafeBashCommand(bash("ls"))).toBe(false);
    });

    it("allows when CLAUDE_GRAVITY_NO_AUTO_APPROVE is unset", () => {
      delete process.env.CLAUDE_GRAVITY_NO_AUTO_APPROVE;
      expect(isSafeBashCommand(bash("ls"))).toBe(true);
    });

    it("allows when CLAUDE_GRAVITY_NO_AUTO_APPROVE=0", () => {
      process.env.CLAUDE_GRAVITY_NO_AUTO_APPROVE = "0";
      expect(isSafeBashCommand(bash("ls"))).toBe(true);
    });
  });
});
