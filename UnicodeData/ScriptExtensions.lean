/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeData.Scripts
import UnicodeBasic.Types
import UnicodeBasic.CharacterDatabase

namespace Unicode

/-- A code point range and its explicit short Script Extensions values. -/
public abbrev ScriptExtension := UInt32 × UInt32 × Array String.Slice

/-- Explicit short Script Extensions values indexed by both script and code point. -/
public structure ScriptExtensions where
  byScript : Std.HashMap String.Slice (Array (UInt32 × UInt32))
  byCode : Array ScriptExtension
deriving Inhabited

/-- Raw string form of `ScriptExtensions.txt`.

Code points not listed in this file have the value of their corresponding
`Script` property.
-/
protected def ScriptExtensions.txt := include_str "../data/ucd/ScriptExtensions.txt"

/-- Explicit short Script Extensions values indexed by both script and code point. -/
public initialize ScriptExtensions.data : ScriptExtensions ← do
  let stream := UCDStream.ofString ScriptExtensions.txt
  let mut data : ScriptExtensions := ⟨{}, #[]⟩
  for record in stream do
    let (c₀, c₁) : UInt32 × UInt32 :=
      match record[0]!.split ".." |>.toList with
      | [c] => (ofHexString! c, ofHexString! c)
      | [c₀, c₁] => (ofHexString! c₀, ofHexString! c₁)
      | _ => panic! "invalid record in ScriptExtensions.txt"
    let shortNames := record[1]!.split " " |>.toArray
    data := {data with byCode := data.byCode.push (c₀, c₁, shortNames)}
    for script in shortNames do
      let ranges := data.byScript.get? script |>.getD #[]
      let ranges :=
        if let some (d₀, d₁) := ranges.back? then
          if c₀ = d₁ + 1 then ranges.pop.push (d₀, c₁) else ranges.push (c₀, c₁)
        else
          #[(c₀, c₁)]
      data := {data with byScript := data.byScript.insert script ranges}
  return data

/-- Get the explicit script extension ranges for a script name. -/
@[inline]
public def ScriptExtensions.getTable (sc : String.Slice) : Array (UInt32 × UInt32) :=
  match PropertyValueAliases.getShortName? "Script" sc with
  | none => #[]
  | some sc => data.byScript.get? sc |>.getD #[]

/-- Find the last range whose lower bound is at most `code`. -/
private partial def ScriptExtensions.find
    (code : UInt32) (lo hi : Nat) : Nat :=
  assert! (hi ≤ data.byCode.size)
  assert! (lo < hi)
  assert! (data.byCode[lo]!.1 ≤ code)
  let mid := (lo + hi) / 2
  if lo = mid then
    mid
  else if code < data.byCode[mid]!.1 then
    find code lo mid
  else
    find code mid hi

/-- Get the explicit short Script Extensions values for a code point. -/
public def ScriptExtensions.getExplicit? (code : UInt32) : Option (Array String.Slice) :=
  if data.byCode.isEmpty || code < data.byCode[0]!.1 then none else
    match data.byCode[find code 0 data.byCode.size]! with
    | (_, top, scripts) => if code ≤ top then some scripts else none

/-- Find the last range whose lower bound is at most `code`. -/
private partial def ScriptExtensions.findRange
    (code : UInt32) (ranges : Array (UInt32 × UInt32)) (lo hi : Nat) : Nat :=
  assert! (hi ≤ ranges.size)
  assert! (lo < hi)
  assert! (ranges[lo]!.1 ≤ code)
  let mid := (lo + hi) / 2
  if lo = mid then
    mid
  else if code < ranges[mid]!.1 then
    findRange code ranges lo mid
  else
    findRange code ranges mid hi

/-- Check whether a code point explicitly lists a script in
`ScriptExtensions.txt`. -/
public def ScriptExtensions.containsExplicit (sc : String.Slice) (code : UInt32) : Bool :=
  let ranges := getTable sc
  if ranges.isEmpty || code < ranges[0]!.1 then false else
    let (_, top) := ranges[findRange code ranges 0 ranges.size]!
    code ≤ top

/-- Get the short Script Extensions values for a code point. -/
public def ScriptExtensions.get (code : UInt32) : Array String.Slice :=
  match getExplicit? code with
  | some scripts => scripts
  | none =>
    match Scripts.getScript? code with
    | some script => #[PropertyValueAliases.getShortName! "Script" script]
    | none => #["Zzzz"]
