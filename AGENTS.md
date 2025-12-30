# Repository Guidelines

## Project Structure & Module Organization
- `unicode-inspector.el` implements the UI and main `unicode-inspector` command.
- `unicode-inspector-blocks.el` is generated Unicode block data; source template is `scripts/unicode-inspector-blocks.el.tpl`.
- `scripts/generate-unicode-blocks.py` regenerates the blocks file from Unicode data.
- `unicode-inspector-test.el` contains ERT tests.
- `Eask` and `Makefile` define package metadata and common tasks.

## Build, Test, and Development Commands
- `eask install` installs dependencies declared in `Eask` (requires Eask and Emacs 29.1+).
- `make compile` runs byte-compilation (preferred over `emacs -Q --batch -l unicode-inspector.el`).
- `make lint` runs checkdoc and declare linting.
- `make test` runs the test suite (see `CONTRIBUTING.md`).
- `python3 scripts/generate-unicode-blocks.py` regenerates `unicode-inspector-blocks.el` (network required).

## Coding Style & Naming Conventions
- Emacs Lisp with `lexical-binding: t`; keep top-of-file headers in standard package format.
- Use 2-space indentation, `defgroup`/`defcustom` for user-facing options, and `provide` at the end.
- Prefer `unicode-inspector-` prefix for public functions/variables; internal helpers can use `unicode-inspector--`.
- Treat `unicode-inspector-blocks.el` as generated; edit the template or generator instead.

## Testing Guidelines
- Tests live in `unicode-inspector-test.el` and use ERT.
- Run `make test` before submitting changes.

## Commit & Pull Request Guidelines
- No strict commit message convention is enforced.
- Use clear, imperative commit subjects (e.g., “Add Unicode property lookup”).
- PRs should include: a short summary, testing notes (or “not run”), and any new user-facing commands.
- Update `CHANGELOG.md` with end-user-facing, human-readable entries per Keep a Changelog.

## Security & Configuration Tips
- Dependencies are declared in `Eask`; avoid adding new ones without a clear feature need.
- Keep compatibility in mind for Emacs 29.1+ as specified by `Eask`.
