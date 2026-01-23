# Repository Guidelines

## Project Structure & Module Organization
- Haskell library in `src/MyPhoto` with apps under `app-stack`, `app-watch`, `app-align`, and `app-toPNG`; tests live in `test`.
- Wrapper scripts like `myphoto-stack.sh` and `myphoto-watch.sh` call the corresponding executables via Nix; helper scripts sit in `one-time-scripts/`.
- Assets are external (images on disk); no binary assets are versioned. Temporary build output is in `dist-newstyle/`.

## Build, Test, and Development Commands
- Develop/build: `cabal build all` (uses `myphoto.cabal`); `cabal run myphoto-stack -- --help` to inspect CLI flags.
- Test: `cabal test test-myphoto` runs the chunking suite; prefer running before pushing.
- Nix flows: `nix build .#myphoto-stack` for the stack binary; `nix run .#myphoto-watch -- <args>` to exercise the watcher.
- Formatting: `nix fmt` runs ormolu for Haskell sources and nixfmt-rfc-style for Nix files via pre-commit hooks.
- Check shell formatting: `nix build '.#checks.x86_64-linux.shell-fmt-check'` or use `, shfmt -d -s -i 4 -ci <files>` to verify.

## Coding Style & Naming Conventions
- Haskell: Follow `ormolu` formatting (two-space indentation, let-bindings aligned by the formatter); run `nix fmt` before commits.
- Shell scripts: Use 4-space indentation with `shfmt -i 4 -s -ci -w <script>`; `set -euo pipefail` for error handling.
- Modules use `MyPhoto.*` namespace; keep new actions under `MyPhoto.Actions.*` and utilities under `MyPhoto.Utils.*`.
- Enable warnings: `-Wall -threaded` is enforced in the common stanza—fix warnings rather than suppressing them.
- Prefer total functions or explicit error handling; keep IO boundaries narrow around wrapper modules.

## Testing Guidelines
- Framework: `test-framework` with `HUnit`. Add new groups to `test/Main.hs` using `testGroup` and `testCase`.
- Name tests after behavior (e.g., `fixed chunking`), and keep assertions deterministic (no filesystem state).
- For logic touching image pipelines, add pure unit tests where possible; if adding integration tests, gate them behind flags to avoid long runs.

## Commit & Pull Request Guidelines
- Commit messages in this repo are short and imperative (e.g., `fmt`, `update`, `add one time scripts`); follow that style and keep commits scoped.
- Pull requests should summarize purpose, list key changes, and note testing (`cabal test`/manual runs). Add screenshots only when UI changes occur (rare here).
- Link related issues or scripts if applicable; call out any external tools (focus-stack, enblend, dcraw) that new behavior depends on.

## Security & Configuration Tips
- Image processing depends on external binaries (focus-stack, enblend-enfuse, dcraw). Document required versions in PRs that rely on new flags.
- Avoid committing large sample images; place local samples outside the repo and reference their paths when describing reproduction steps.
