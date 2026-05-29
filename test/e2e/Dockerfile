# E2E harness image: real gravity-server + proxy + emacs --daemon client.
# Self-contained (repo baked in) for hermetic CI — no bind mount needed.
# AF_UNIX bind works as root in the container (no host sandbox).
FROM ubuntu:24.04
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl ca-certificates emacs-nox git make && \
    rm -rf /var/lib/apt/lists/*

# Node.js 22
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y nodejs && rm -rf /var/lib/apt/lists/*

# Emacs deps: magit-section + transient from MELPA; package-initialize on
# every batch invocation (the daemon starts with --eval (package-initialize)).
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
COPY package.json package-lock.json* ./
COPY packages/ ./packages/
RUN npm ci --no-audit --no-fund || npm install --no-audit --no-fund
# Source + harness (elisp modules, test/e2e/*)
COPY . .

# tsx is a workspace dependency; the runner invokes it via `npx tsx`.
CMD ["node", "test/e2e/run.mjs"]
