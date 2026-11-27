/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import UnicodeData

open Unicode

/-! ## `UnicodeData.txt` -/

-- names, including derived names
#guard (getUnicodeData 'A').name == "LATIN CAPITAL LETTER A"
#guard (getUnicodeData '\x00').name == "<control-0000>"
#guard (getUnicodeData '一').name == "CJK UNIFIED IDEOGRAPH-4E00"
#guard (getUnicodeData '가').name == "HANGUL SYLLABLE GA"
#guard (getUnicodeData '͸').name == "<reserved-0378>"
#guard (getUnicodeData '﷐').name == "<noncharacter-FDD0>"

-- general category, combining class, bidi
#guard (getUnicodeData 'A').gc == .Lu
#guard (getUnicodeData '').gc == .Co
#guard (getUnicodeData '́').cc == 230
#guard (getUnicodeData 'A').bidi == .L
#guard (getUnicodeData '(').bidiMirrored

-- numeric values
#guard (getUnicodeData '5').numeric == some (.decimal 5)
#guard (getUnicodeData '²').numeric == some (.digit 2)
#guard (getUnicodeData '⅕').numeric == some (.numeric 1 (some 5))
#guard (getUnicodeData '༳').numeric == some (.numeric (-1) (some 2))
#guard (getUnicodeData 'A').numeric == none

-- simple case mappings
#guard (getUnicodeData 'A').lowercase == some 'a'
#guard (getUnicodeData 'a').uppercase == some 'A'
#guard (getUnicodeData 'Ǆ').titlecase == some 'ǅ'

-- code points that are not characters
#guard (getUnicodeData? 0xD800).map (·.gc) == some .Cs
#guard (getUnicodeData? 0x110000).isNone

-- the stream agrees with the lookup at every code point
#guard Id.run do
  let mut s : UnicodeDataStream := {}
  let mut code : UInt32 := 0
  while true do
    match s.next? with
    | none => break
    | some (d, s') =>
      if d.code != code || some d != getUnicodeData? code then return false
      s := s'
      code := code + 1
  return code == Unicode.max + 1

/-! ## `CaseFolding.txt` -/

#guard CaseFolding.getSimple? 'A'.val == some 'a'.val
#guard CaseFolding.getSimple? 'a'.val == none
#guard CaseFolding.getSimple? 0x1E9E == some 0x00DF
#guard CaseFolding.getFull 0x00DF == #[0x0073, 0x0073]
#guard CaseFolding.getFull 'A'.val == #['a'.val]
#guard CaseFolding.getFull 'a'.val == #[]

/-! ## `SpecialCasing.txt` -/

#guard SpecialCasing.getUpper? 0x00DF == some #[0x0053, 0x0053]
#guard SpecialCasing.getTitle? 0x00DF == some #[0x0053, 0x0073]
#guard SpecialCasing.getLower? 0x0149 == some #[0x0149]
#guard SpecialCasing.getUpper? 'a'.val == none

/-! ## `PropList.txt` -/

#guard PropList.isWhiteSpace ' '.val
#guard !PropList.isWhiteSpace 'a'.val
#guard PropList.isNoncharacterCodePoint 0xFFFE
#guard !PropList.isNoncharacterCodePoint 'a'.val
#guard PropList.isOtherMath '^'.val
#guard PropList.isOtherAlphabetic 0x0345
#guard PropList.isOtherLowercase 0x00AA
#guard PropList.isOtherUppercase 0x2160
#guard PropList.isOtherDefaultIgnorableCodePoint 0x034F
#guard PropList.isPrependedConcatenationMark 0x0600
#guard PropList.isVariationSelector 0xFE0F
#guard PropList.isDeprecated 0x0149
#guard !PropList.isDeprecated 'a'.val

/-! ## `Scripts.txt` and `ScriptExtensions.txt` -/

#guard Scripts.get 'A'.val == .ofAbbrev! "Latn"
#guard Scripts.get 'α'.val == .ofAbbrev! "Grek"
#guard Scripts.get 0x0378 == .ofAbbrev! "Zzzz"
#guard (Scripts.getTable! "Latin").any fun (lo, hi) => lo ≤ 'A'.val && 'A'.val ≤ hi
#guard ScriptExtensions.getExplicit? 'A'.val == none
#guard ScriptExtensions.get 'A'.val == #[.ofAbbrev! "Latn"]
#guard (ScriptExtensions.get 0x0951).size == 15
#guard ScriptExtensions.contains (.ofAbbrev! "Deva") 0x0951
#guard !ScriptExtensions.contains (.ofAbbrev! "Grek") 0x0951
#guard (ScriptExtensions.getTable (.ofAbbrev! "Deva")).any fun (lo, hi) => lo ≤ 0x0951 && 0x0951 ≤ hi

/-! ## `PropertyAliases.txt` and `PropertyValueAliases.txt` -/

#guard PropertyAliases.getLongName? "gc" == some "General_Category"
#guard PropertyAliases.getShortName? "Script" == some "sc"
#guard PropertyAliases.getLongName? "nope" == none
#guard PropertyValueAliases.getLongName? "gc" "Lu" == some "Uppercase_Letter"
#guard PropertyValueAliases.getShortName? "Script" "Latin" == some "Latn"
