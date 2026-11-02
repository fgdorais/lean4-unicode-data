# Contributing to UnicodeData

Thanks for your interest in UnicodeData! Bug reports, questions and pull requests are all welcome.

UnicodeData is one of the [Lean4 / Unicode family](README.md#the-lean4--unicode-family) of libraries. If you are not sure which library a change belongs in, open an issue and ask.

## Issues

Please open an issue before starting on a larger change, such as a new feature or an API change, so we can agree on the approach first. Small fixes can go straight to a pull request.

## Building and testing

```sh
lake build --wfail                   # warnings are errors
```

CI runs the same commands on every pull request, and they must pass with no warnings. The Lean version is set in `lean-toolchain` and is updated automatically, so please don't change it in a pull request.

## Dependencies

UnicodeData depends on Batteries and UnicodeBasic. Use Batteries' `Nat.bisect` for binary searches rather than writing your own, and prefer UnicodeBasic's types (such as `Script`) over strings in the public API.

## Code style

- Public functions take code points as `UInt32` rather than `Char`, since the UCD files describe code points (including surrogates and unassigned ones). Prefer precise types over strings.
- Docstrings say what a function does, not how it is implemented.
- Prefer existing library functions over hand-rolled code.
- Tables are parsed lazily (as `Thunk`s), so a program only pays for the tables it uses. Keep new tables lazy too.

## Pull requests

- Start the title with one of these types:
  - `feat:` a new feature or new API.
  - `fix:` a bug fix.
  - `perf:` a speedup or smaller memory use, with no change in behavior.
  - `doc:` documentation only, such as docstrings or the README.
  - `refactor:` reorganizing code, such as moving, renaming or simplifying it, when the main point is not a feature or a fix.
  - `chore:` maintenance, such as toolchain updates, CI and build files.
- In the description, say what the library does after the change, from a user's point of view: what is new, what is fixed, and anything users must know, such as a breaking change. Leave out how the code worked before and how the change is implemented; the diff shows that. Skip "Before / After / How" sections, since only the "after" part matters to users.
- Add tests for new behavior and for bug fixes.
- Keep each pull request to one purpose, the one its title names. A fix and the test that catches it belong together, and small cleanups in code you are already changing are fine. If you notice an unrelated bug or improvement along the way, send it as a separate pull request.

## AI-assisted contributions

Contributions written with AI tools are welcome. You are responsible for the change as if you wrote it yourself: please make sure you understand it, that it follows this guide, and that it builds and passes the tests.

## License

UnicodeData is released under the Apache 2.0 license (see `LICENSE`). By contributing, you agree that your contributions are released under the same license.
