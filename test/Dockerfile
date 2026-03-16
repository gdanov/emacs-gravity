FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive

# System packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl ca-certificates emacs-nox git make gnupg && \
    rm -rf /var/lib/apt/lists/*

# Node.js 22 via NodeSource
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y nodejs && \
    rm -rf /var/lib/apt/lists/*

# tsx (TypeScript runner, needed by gravity-server and bridge)
RUN npm install -g tsx

# Claude Code CLI (for plugin validate + marketplace commands)
RUN npm install -g @anthropic-ai/claude-code

# Emacs packages: magit-section and transient from MELPA
# Also create an init file so package-initialize runs on every batch invocation
RUN emacs --batch --eval ' \
  (progn \
    (require (quote package)) \
    (add-to-list (quote package-archives) \
                 (cons "melpa" "https://melpa.org/packages/") t) \
    (package-initialize) \
    (package-refresh-contents) \
    (package-install (quote magit-section)) \
    (package-install (quote transient)))' && \
    mkdir -p /root/.emacs.d && \
    echo '(package-initialize)' > /root/.emacs.d/init.el

WORKDIR /workspace
