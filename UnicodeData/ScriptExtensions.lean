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

/-- Script Extensions values indexed by both script and code point.

* `byScript` maps each short script name to the sorted ranges of all code points whose
  Script Extensions value contains that script, including code points that inherit
  their value from the `Script` property.
* `byCode` lists the code point ranges explicitly given in `ScriptExtensions.txt`,
  sorted by code point.
-/
public structure ScriptExtensions where
  byScript : Std.HashMap String.Slice (Array (UInt32 × UInt32))
  byCode : Array ScriptExtension
deriving Inhabited

/-- Raw string form of `ScriptExtensions.txt`.

Code points not listed in this file have the value of their corresponding
`Script` property.
-/
protected def ScriptExtensions.txt := include_str "../data/ucd/ScriptExtensions.txt"

/-- Sort ranges and merge those that overlap or are adjacent. -/
private def ScriptExtensions.normalize (ranges : Array (UInt32 × UInt32)) :
    Array (UInt32 × UInt32) := Id.run do
  let mut out := #[]
  for (c₀, c₁) in ranges.qsort fun a b => a.1 < b.1 do
    match out.back? with
    | some (d₀, d₁) =>
      if c₀ ≤ d₁ + 1 then
        out := out.pop.push (d₀, max c₁ d₁)
      else
        out := out.push (c₀, c₁)
    | none => out := out.push (c₀, c₁)
  return out

/-- Remove from `ranges` all code points in the sorted disjoint ranges `holes`. -/
private def ScriptExtensions.subtract (ranges : Array (UInt32 × UInt32))
    (holes : Array (UInt32 × UInt32)) : Array (UInt32 × UInt32) := Id.run do
  let mut out := #[]
  for (c₀, c₁) in ranges do
    let mut lo := c₀
    let mut done := false
    for (h₀, h₁) in holes do
      if h₁ < lo then continue
      if c₁ < h₀ then break
      if lo < h₀ then out := out.push (lo, h₀ - 1)
      if c₁ ≤ h₁ then
        done := true
        break
      lo := h₁ + 1
    unless done do out := out.push (lo, c₁)
  return out

/-- Script Extensions values indexed by both script and code point. -/
public initialize ScriptExtensions.data : ScriptExtensions ← do
  let stream := UCDStream.ofString ScriptExtensions.txt
  let mut byCode : Array ScriptExtension := #[]
  let mut explicit : Std.HashMap String.Slice (Array (UInt32 × UInt32)) := {}
  for record in stream do
    let (c₀, c₁) : UInt32 × UInt32 :=
      match record[0]!.split ".." |>.toList with
      | [c] => (ofHexString! c, ofHexString! c)
      | [c₀, c₁] => (ofHexString! c₀, ofHexString! c₁)
      | _ => panic! "invalid record in ScriptExtensions.txt"
    let shortNames := record[1]!.split " " |>.toArray
    byCode := byCode.push (c₀, c₁, shortNames)
    for script in shortNames do
      explicit := explicit.insert script <| (explicit.getD script #[]).push (c₀, c₁)
  byCode := byCode.qsort fun a b => a.1 < b.1
  let listed := ScriptExtensions.normalize <| byCode.map fun (c₀, c₁, _) => (c₀, c₁)
  -- Code points with an explicit value keep only that value; all others inherit
  -- their `Script` value.
  let mut byScript : Std.HashMap String.Slice (Array (UInt32 × UInt32)) := {}
  let mut assigned := #[]
  for (script, ranges) in Scripts.data do
    let sc := PropertyValueAliases.getShortName! "Script" script
    byScript := byScript.insert sc (ScriptExtensions.subtract (ScriptExtensions.normalize ranges) listed)
    assigned := assigned ++ ranges
  -- Code points without a `Script` value have the value `Unknown`.
  let unknown := ScriptExtensions.subtract #[(0, 0x10FFFF)] (ScriptExtensions.normalize (assigned ++ listed))
  byScript := byScript.insert "Zzzz" <| unknown ++ byScript.getD "Zzzz" #[]
  for (sc, ranges) in explicit do
    byScript := byScript.insert sc (byScript.getD sc #[] ++ ranges)
  byScript := byScript.map fun _ ranges => ScriptExtensions.normalize ranges
  return ⟨byScript, byCode⟩

/-- Get the ranges of all code points whose Script Extensions value contains the given
script. The script may be given by its short or long name. -/
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

/-- Check whether the Script Extensions value of a code point contains the given script.
The script may be given by its short or long name. -/
public def ScriptExtensions.contains (sc : String.Slice) (code : UInt32) : Bool :=
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
