/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
import Batteries.Data.Nat.Bisect
import UnicodeBasic.Types
import UnicodeBasic.CharacterDatabase

namespace Unicode.SpecialCasing

/-- Raw string from `SpecialCasing.txt` -/
protected def txt := include_str "../data/ucd/SpecialCasing.txt"

/-- Parsed unconditional mappings from `SpecialCasing.txt`

  Each entry is `(c, l, t, u)` where `l`, `t` and `u` are the full lowercase,
  titlecase and uppercase mappings of `c`. Entries are sorted by code point. -/
public def data : Thunk (Array (UInt32 × Array UInt32 × Array UInt32 × Array UInt32)) :=
  .mk fun _ => Id.run do
    let stream := UCDStream.ofString SpecialCasing.txt
    let mut a := #[]
    for record in stream do
      -- skip conditional mappings
      if record.size > 4 && !record[4]!.isEmpty then continue
      let c : UInt32 := ofHexString! record[0]!
      let l := record[1]!.split " " |>.toArray.map ofHexString!
      let t := record[2]!.split " " |>.toArray.map ofHexString!
      let u := record[3]!.split " " |>.toArray.map ofHexString!
      a := a.push (c, l, t, u)
    return a.qsort (·.1 < ·.1)

/-- Binary search for the last entry with code at most `code` -/
def find (data : Array (UInt32 × Array UInt32 × Array UInt32 × Array UInt32)) (code : UInt32) :
    Nat :=
  let p i := decide (i < data.size) && data[i]!.1 ≤ code
  if h : 0 < data.size ∧ data[0]!.1 ≤ code then
    Nat.bisect (p := p) h.1 (by simp only [p, h.1, h.2, decide_true, Bool.and_self]) (by simp [p])
  else
    panic! "invalid binary search start"

/-- Get the entry for `code`, if any -/
def get? (code : UInt32) : Option (Array UInt32 × Array UInt32 × Array UInt32) :=
  let data := data.get
  if code < data[0]!.1 then none else
    match data[find data code]! with
    | (c, m) => if code == c then some m else none

/-- Get unconditional full lowercase mapping, if any -/
public def getLower? (code : UInt32) : Option (Array UInt32) :=
  (get? code).map (·.1)

/-- Get unconditional full titlecase mapping, if any -/
public def getTitle? (code : UInt32) : Option (Array UInt32) :=
  (get? code).map (·.2.1)

/-- Get unconditional full uppercase mapping, if any -/
public def getUpper? (code : UInt32) : Option (Array UInt32) :=
  (get? code).map (·.2.2)
