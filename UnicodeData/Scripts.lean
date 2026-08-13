/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeData.Aliases
import UnicodeBasic.Types
import UnicodeBasic.CharacterDatabase

namespace Unicode

/-- Type for scripts data -/
public abbrev Scripts := Std.HashMap String.Slice (Array (UInt32 × UInt32))

/-- Raw string from `Scripts.txt` -/
def Scripts.txt := include_str "../data/ucd/Scripts.txt"

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

/-- Script ranges indexed by code point. -/
initialize Scripts.codeData : Array (UInt32 × UInt32 × String.Slice) ← do
  let mut data := #[]
  for (script, ranges) in Scripts.data do
    for (c₀, c₁) in ranges do
      data := data.push (c₀, c₁, script)
  return data.qsort fun a b => a.1 < b.1

/-- Find the last script range whose lower bound is at most `code`. -/
private partial def Scripts.find (code : UInt32) (lo hi : Nat) : Nat :=
  assert! (hi ≤ codeData.size)
  assert! (lo < hi)
  assert! (codeData[lo]!.1 ≤ code)
  let mid := (lo + hi) / 2
  if lo = mid then
    mid
  else if code < codeData[mid]!.1 then
    find code lo mid
  else
    find code mid hi

/-- Get the Script property value for a code point. -/
public def Scripts.getScript? (code : UInt32) : Option String.Slice :=
  if codeData.isEmpty || code < codeData[0]!.1 then none else
    match codeData[find code 0 codeData.size]! with
    | (_, top, script) => if code ≤ top then some script else none

/-- Get table for given script -/
@[inline]
public def Scripts.getTable? (sc : String.Slice) : Option <| Array (UInt32 × UInt32) := do
  let sc ← PropertyValueAliases.getLongName! "Script" sc
  data.get? sc

@[inline, inherit_doc Scripts.getTable?]
public def Scripts.getTable! (sc : String.Slice) : Array (UInt32 × UInt32) :=
  getTable? sc |>.get!
