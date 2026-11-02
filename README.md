# Lean 4 / Unicode Data

Unicode Character Database (UCD) support for Lean 4.

## The Lean4 / Unicode family

This library is one of a small family of Lean 4 Unicode libraries. Each one has a narrow job, so
you only depend on what you need.

| Library | What it is for |
| --- | --- |
| [UnicodeBasic](https://github.com/fgdorais/lean4-unicode-basic) | The foundation. Fast lookups of commonly used character properties such as general category, case mappings, numeric values, scripts and names, from compact precompiled tables. Start here. |
| **UnicodeData** (this library) | Direct access to the Unicode Character Database files themselves, such as `UnicodeData.txt`, `PropList.txt`, `CaseFolding.txt` and `SpecialCasing.txt`. Heavier than UnicodeBasic, but complete, and handy for properties UnicodeBasic does not cover or for checking it against the source. |
| [UnicodeFormat](https://github.com/fgdorais/lean4-unicode-format) | Encoding and decoding: UTF-8, UTF-16 and UTF-32, the byte-level encoding schemes with byte order marks, strict and lossy decoding, and streaming iterators. |
| [UnicodeLemmas](https://github.com/fgdorais/lean4-unicode-lemmas) | Proofs about UnicodeBasic properties, such as the fact that every cased character is alphabetic. |
| [UnicodeExtra](https://github.com/fgdorais/lean4-unicode-extra) | Unicode algorithms on whole strings that are too large or too specialized for UnicodeBasic, built on top of it. |

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
