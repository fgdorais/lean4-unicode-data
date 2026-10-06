/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
import Batteries.Data.Nat.Bisect
import UnicodeBasic.Types
import UnicodeBasic.CharacterDatabase

namespace Unicode.CaseFolding

/-- Raw string from `CaseFolding.txt` -/
protected def txt := include_str "../data/ucd/CaseFolding.txt"

public initialize data : Array (UInt32 × Option UInt32 × Array UInt32) ← do
  let stream := UCDStream.ofString CaseFolding.txt
  let mut a := #[]
  for record in stream do
    let c : UInt32 := ofHexString! record[0]!
    if record[1]! == "C" then
      let s : UInt32 := ofHexString! record[2]!
      a := a.push (c, some s, #[s])
    else if record[1]! == "S" then
      let s : UInt32 := ofHexString! record[2]!
      if a.back!.1 == c then
        a := a.pop.push (c, some s, a.back!.2.2)
      else
        a := a.push (c, some s, #[s])
    else if record[1]! == "F" then
      let f : Array UInt32 := record[2]!.split " "  |>.toArray.map ofHexString!
      if a.back!.1 == c then
        a := a.pop.push (c, a.back!.2.1, f)
      else
        a := a.push (c, none, f)
    else continue
  return a

/-- Binary search for the last entry with code at most `code` -/
def find (code : UInt32) : Nat :=
  let p i := decide (i < data.size) && data[i]!.1 ≤ code
  if h : 0 < data.size ∧ data[0]!.1 ≤ code then
    Nat.bisect (p := p) h.1 (by simp only [p, h.1, h.2, decide_true, Bool.and_self]) (by simp [p])
  else
    panic! "invalid binary search start"

/-- Get simple case folding -/
public def getSimple? (code : UInt32) : Option UInt32 :=
  if code < data[0]!.1 then none else
    match data[find code]! with
    | (c, s, _) => if code == c then s else none

/-- Get full case folding -/
public def getFull (code : UInt32) : Array UInt32 :=
  if code < data[0]!.1 then #[] else
    match data[find code]! with
    | (c, s, f) =>
      if code == c then
        if f.isEmpty then #[s.getD c] else f
      else #[]
