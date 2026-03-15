// bidirectional.ts — Permission/question/plan-review flow
//
// Handles bidirectional hook events where the bridge shim keeps
// its socket open waiting for a response from the terminal.

// TODO: Phase 3 — implement bidirectional flows
// PermissionRequest → create inbox item → terminal responds → write to hook socket
// AskUserQuestion → create inbox item → terminal responds → write to hook socket
// DENY-AS-APPROVE workaround for ExitPlanMode lives here
