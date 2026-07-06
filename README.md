# Lean 4 / Unicode Data

Unicode Character Database (UCD) support for Lean 4.

## Installation

Add the following dependency to your project's `lakefile.toml`:

```toml
[[require]]
name = "UnicodeData"
git = "https://github.com/fgdorais/lean4-unicode-data.git"
rev = "main" # or any specific revision
```

Or this dependency to your project's `lakefile.lean`:

```lean4
require UnicodeData from git
  "https://github.com/fgdorais/lean4-unicode-data.git" @ "main"
```

## Documentation

You can generate documentation locally using `lake build UnicodeData:docs` in the `docs` directory.

-----

* The `Lean 4 / Unicode Data` library is copyright © 2023-2026 François G. Dorais. The library is released under the [Apache 2.0 license](http://www.apache.org/licenses/LICENSE-2.0). See the file LICENSE for additional details.
* The Unicode Character Database files are copyright © 1991-2026 Unicode®, Inc. The files are distributed under the [Unicode® Copyright and Terms of Use](https://www.unicode.org/copyright.html). See the file LICENSE-UNICODE for additional details.
