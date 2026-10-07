/-
Copyright © 2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import UnicodeData.Scripts
import Batteries.Data.Nat.Bisect
public import UnicodeBasic.Types
import UnicodeBasic.CharacterDatabase

namespace Unicode

/-- A code point range and its `Script_Extensions` values as listed in
`ScriptExtensions.txt`. -/
public abbrev ScriptExtension := UInt32 × UInt32 × Array Script

/-- `Script_Extensions` values indexed by both script and code point.

* `byScript` maps each script to the sorted ranges of all code points whose
  `Script_Extensions` value contains that script. This uses both `ScriptExtensions.txt`
  and, for code points not listed there, the `Script` property. Each table is computed
  on first use.
* `byCode` lists only the code point ranges explicitly given in `ScriptExtensions.txt`,
  sorted by code point. It does not use the `Script` property.
-/
public structure ScriptExtensions where
  byScript : Std.HashMap Script (Thunk (Array (UInt32 × UInt32)))
  byCode : Array ScriptExtension
deriving Inhabited

/-- Raw string form of `ScriptExtensions.txt`.

Code points not listed in this file have a `Script_Extensions` value consisting of
just their `Script` property value.
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

/-- `Script_Extensions` values indexed by both script and code point, using both
`ScriptExtensions.txt` and the `Script` property. -/
public initialize ScriptExtensions.data : ScriptExtensions ← do
  let stream := UCDStream.ofString ScriptExtensions.txt
  let mut byCode : Array ScriptExtension := #[]
  let mut explicit : Std.HashMap Script (Array (UInt32 × UInt32)) := {}
  for record in stream do
    let (c₀, c₁) : UInt32 × UInt32 :=
      match record[0]!.split ".." |>.toList with
      | [c] => (ofHexString! c, ofHexString! c)
      | [c₀, c₁] => (ofHexString! c₀, ofHexString! c₁)
      | _ => panic! "invalid record in ScriptExtensions.txt"
    let scripts := record[1]!.split " " |>.toArray |>.map Scripts.ofShortName!
    byCode := byCode.push (c₀, c₁, scripts)
    for script in scripts do
      explicit := explicit.insert script <| (explicit.getD script #[]).push (c₀, c₁)
  byCode := byCode.qsort fun a b => a.1 < b.1
  let listed := ScriptExtensions.normalize <| byCode.map fun (c₀, c₁, _) => (c₀, c₁)
  -- Code points with an explicit value keep only that value; all others inherit
  -- their `Script` value.
  let mut byScript : Std.HashMap Script (Thunk (Array (UInt32 × UInt32))) := {}
  for (script, ranges) in Scripts.data do
    let sc := Scripts.ofShortName! <| PropertyValueAliases.getShortName! "Script" script
    let extra := explicit.getD sc #[]
    byScript := byScript.insert sc <| .mk fun _ =>
      ScriptExtensions.normalize <|
        ScriptExtensions.subtract (ScriptExtensions.normalize ranges) listed ++ extra
  for (sc, extra) in explicit do
    unless byScript.contains sc do
      byScript := byScript.insert sc <| .mk fun _ => ScriptExtensions.normalize extra
  -- Code points without a `Script` value have the value `Unknown`.
  let extra := explicit.getD default #[]
  byScript := byScript.insert default <| .mk fun _ =>
    let assigned := Scripts.data.fold (init := listed) fun a _ ranges => a ++ ranges
    ScriptExtensions.normalize <|
      ScriptExtensions.subtract #[(0, 0x10FFFF)] (ScriptExtensions.normalize assigned) ++ extra
  return ⟨byScript, byCode⟩

/-- Get the ranges of all code points whose `Script_Extensions` value contains the given
script.

Uses both `ScriptExtensions.txt` and the `Script` property; see `Scripts.getTable?` for
the `Script` property alone. -/
@[inline]
public def ScriptExtensions.getTable (sc : Script) : Array (UInt32 × UInt32) :=
  data.byScript.get? sc |>.map Thunk.get |>.getD #[]

/-- Binary search for the last entry with lower bound at most `code` -/
private def ScriptExtensions.find (code : UInt32) : Nat :=
  let p i := decide (i < data.byCode.size) && data.byCode[i]!.1 ≤ code
  if h : 0 < data.byCode.size ∧ data.byCode[0]!.1 ≤ code then
    Nat.bisect (p := p) h.1 (by simp only [p, h.1, h.2, decide_true, Bool.and_self]) (by simp [p])
  else
    panic! "invalid binary search start"

/-- Get the `Script_Extensions` values listed for a code point in
`ScriptExtensions.txt`, or `none` if it is not listed.

Uses only `ScriptExtensions.txt`, not the `Script` property; see `ScriptExtensions.get`
for the complete value. -/
public def ScriptExtensions.getExplicit? (code : UInt32) : Option (Array Script) :=
  if data.byCode.isEmpty || code < data.byCode[0]!.1 then none else
    match data.byCode[find code]! with
    | (_, top, scripts) => if code ≤ top then some scripts else none

/-- Binary search for the last entry with lower bound at most `code` -/
private def ScriptExtensions.findRange (code : UInt32) (ranges : Array (UInt32 × UInt32)) : Nat :=
  let p i := decide (i < ranges.size) && ranges[i]!.1 ≤ code
  if h : 0 < ranges.size ∧ ranges[0]!.1 ≤ code then
    Nat.bisect (p := p) h.1 (by simp only [p, h.1, h.2, decide_true, Bool.and_self]) (by simp [p])
  else
    panic! "invalid binary search start"

/-- Check whether the `Script_Extensions` value of a code point contains the given script.

Uses both `ScriptExtensions.txt` and the `Script` property. -/
public def ScriptExtensions.contains (sc : Script) (code : UInt32) : Bool :=
  let ranges := getTable sc
  if ranges.isEmpty || code < ranges[0]!.1 then false else
    let (_, top) := ranges[findRange code ranges]!
    code ≤ top

/-- Get the `Script_Extensions` values for a code point.

Uses `ScriptExtensions.txt` when the code point is listed there, and otherwise its `Script`
property value (`Zzzz` if unassigned); see `Scripts.get` for the `Script` property alone. -/
public def ScriptExtensions.get (code : UInt32) : Array Script :=
  getExplicit? code |>.getD #[Scripts.get code]
