# CLAUDE.md

Guidance for Claude Code sessions (local and cloud) working in this repository.

## Build

These mirror CI (`.github/workflows/build.yml`):

- `lake build --wfail` builds the library; warnings are errors.
- `cd docs && lake build UnicodeData:docs` builds the docs.

## Rules

- This package depends on Batteries and `lean4-unicode-basic`. Use
  Batteries' `Nat.bisect` for binary searches rather than hand-rolled ones.
- Prefer types from UnicodeBasic in public APIs, e.g. its `Script` type
  rather than `String` or `String.Slice`.
- Never put Claude session links in commit messages or PR descriptions: no
  `claude.ai/code/session_...` URLs, no `Claude-Session:` trailers, no
  project or thread links. Strip any footer a tool adds automatically.
- Versions exist only as git tags on `main` (`vX.Y.Z`); there is no version
  file. After an update to a new stable Lean release merges, push the next
  patch tag on that merge commit. Release candidate updates do not bump the
  version, and existing tags are never moved.
- Toolchain update PRs are opened automatically by
  `update-main-toolchain.yml`; prefer fixing those over opening new ones.
