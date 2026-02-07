#!/usr/bin/env node
// Gravity statusline script for managed Claude Code sessions.
// Receives JSON session data on stdin from Claude Code's statusLine system.
// Sends data to the gravity Emacs socket and outputs minimal terminal text.
'use strict';

const net = require('net');

let input = '';
process.stdin.on('data', chunk => input += chunk);
process.stdin.on('end', () => {
  let data;
  try {
    data = JSON.parse(input);
  } catch {
    process.exit(0);
  }

  // Send to gravity socket if available (fire-and-forget)
  const sockPath = process.env.CLAUDE_GRAVITY_SOCK;
  if (sockPath) {
    const envelope = JSON.stringify({
      event: 'StatusLine',
      session_id: data.session_id || 'unknown',
      cwd: data.cwd || (data.workspace && data.workspace.current_dir) || '',
      data: data
    });
    const c = net.createConnection(sockPath);
    c.on('connect', () => { c.write(envelope + '\n'); c.end(); });
    c.on('error', () => {});
  }

  // Terminal output
  const mode = process.env.CLAUDE_GRAVITY_STATUSLINE_MODE || 'minimal';
  if (mode === 'suppress') {
    process.exit(0);
  }

  const model = (data.model && data.model.display_name) || '?';
  const pct = Math.floor((data.context_window && data.context_window.used_percentage) || 0);
  const cost = ((data.cost && data.cost.total_cost_usd) || 0).toFixed(2);
  console.log('[' + model + '] ctx:' + pct + '% $' + cost);
});
