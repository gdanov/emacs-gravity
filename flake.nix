{
  description = "emacs-gravity: Emacs UI for Claude Code";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        mkEmacsWithDeps = emacs:
          (pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs: [
            epkgs.magit-section
            epkgs.transient
          ]);

        emacsDefault = mkEmacsWithDeps pkgs.emacs;
        emacsNox = mkEmacsWithDeps pkgs.emacs-nox;

        mkErtCheck = name: emacsWithDeps:
          pkgs.runCommand name {
            nativeBuildInputs = [ emacsWithDeps ];
          } ''
            cd ${self}
            emacs -nw --batch \
              -L . -L test \
              -l claude-gravity \
              -l cg-test-replay \
              -l claude-gravity-test \
              -l claude-gravity-contract-test \
              -f ert-run-tests-batch-and-exit
            touch $out
          '';

        shellHook = ''
          echo "emacs-gravity dev shell"
          echo ""
          echo "Available make targets:"
          echo "  make test        - run all tests (elisp + bridge)"
          echo "  make test-elisp  - run ERT tests"
          echo "  make test-bridge - run vitest bridge tests"
          echo "  make build       - build the Node.js bridge"
          echo "  make clean       - remove build artifacts"
        '';
      in
      {
        checks = {
          elisp-tests = mkErtCheck "elisp-tests" emacsDefault;
          elisp-tests-nox = mkErtCheck "elisp-tests-nox" emacsNox;
        };

        devShells = {
          default = pkgs.mkShell {
            buildInputs = [ emacsDefault pkgs.nodejs_22 pkgs.gnumake ];
            inherit shellHook;
          };

          nox = pkgs.mkShell {
            buildInputs = [ emacsNox pkgs.nodejs_22 pkgs.gnumake ];
            inherit shellHook;
          };
        };
      }
    );
}
