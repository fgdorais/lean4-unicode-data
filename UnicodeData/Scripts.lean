/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeData.Aliases
import Batteries.Data.Nat.Bisect
public import UnicodeBasic.Types
import UnicodeBasic.CharacterDatabase

namespace Unicode

/-- Type for `Script` property data: long script names mapped to code point ranges.

Uses only the `Script` property (`Scripts.txt`), not `Script_Extensions`. -/
public abbrev Scripts := Std.HashMap String.Slice (Array (UInt32 × UInt32))

/-- Raw string from `Scripts.txt` (`Script` property only) -/
def Scripts.txt := include_str "../data/ucd/Scripts.txt"

/-- Code point ranges for each script, using only the `Script` property. -/
public initialize Scripts.data : Scripts ← do
  let stream := UCDStream.ofString Scripts.txt
  let mut t := {}
  for record in stream do
    let (c₀, c₁) : UInt32 × UInt32 :=
      match record[0]!.split ".." |>.toList with
      | [c] => (ofHexString! c, ofHexString! c)
      | [c₀, c₁] => (ofHexString! c₀, ofHexString! c₁)
      | _ => panic! "invalid record in Scripts.txt"
    match t.get? record[1]! with
    | some a =>
      let (d₀, d₁) := a.back!
      if c₀ = d₁ + 1 then
        t := t.insert record[1]! (a.pop.push (d₀, c₁))
      else
        t := t.insert record[1]! (a.push (c₀, c₁))
    | none =>
      t := t.insert record[1]! #[(c₀, c₁)]
  return t

/-- Get the script with the given four-letter short name.

Unlike `Script.ofAbbrev!`, this does not call into the UnicodeBasic C library, so it can
be used in initializers that also run in the interpreter. -/
public def Scripts.ofShortName! (name : String.Slice) : Script :=
  let code := name.bytes.fold (init := 0) fun (c : UInt32) b => (c <<< 8) ||| b.toUInt32
  if h : name.utf8ByteSize = 4 ∧ Script.isValid code then ⟨code, h.2⟩ else
    panic! s!"invalid script name {name}"

/-- `Script` property ranges indexed by code point. -/
initialize Scripts.codeData : Array (UInt32 × UInt32 × Script) ← do
  let mut data := #[]
  for (script, ranges) in Scripts.data do
    let sc := Scripts.ofShortName! <| PropertyValueAliases.getShortName! "Script" script
    for (c₀, c₁) in ranges do
      data := data.push (c₀, c₁, sc)
  return data.qsort fun a b => a.1 < b.1

/-- Get the `Script` property value for a code point, `Zzzz` (`Unknown`) if unassigned.

Uses only the `Script` property; see `ScriptExtensions.get` for `Script_Extensions`. -/
public def Scripts.get (code : UInt32) : Script :=
  let p i := decide (i < codeData.size) && codeData[i]!.1 ≤ code
  if h : 0 < codeData.size ∧ codeData[0]!.1 ≤ code then
    let i := Nat.bisect (p := p) h.1
      (by simp only [p, h.1, h.2, decide_true, Bool.and_self]) (by simp [p])
    let (_, top, script) := codeData[i]!
    if code ≤ top then script else default
  else default

/-- Get the code point ranges whose `Script` property is the given script.

Uses only the `Script` property; see `ScriptExtensions.getTable` for `Script_Extensions`. -/
@[inline]
public def Scripts.getTable? (sc : String.Slice) : Option <| Array (UInt32 × UInt32) := do
  let sc ← PropertyValueAliases.getLongName! "Script" sc
  data.get? sc

@[inline, inherit_doc Scripts.getTable?]
public def Scripts.getTable! (sc : String.Slice) : Array (UInt32 × UInt32) :=
  getTable? sc |>.get!
