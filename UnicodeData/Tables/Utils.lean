/-
Copyright © 2024-2026 François G. Dorais. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
public import Batteries.Data.BinaryHeap

namespace Unicode.Tables

public partial def propMergeAndCompress (as : Array (Array (UInt32 × UInt32))) : Array (UInt32 × UInt32) :=
  let heap : Batteries.BinaryHeap _ _ :=
    as.filter (! ·.isEmpty) |>.toBinaryHeap fun t₁ t₂ =>
      match t₁[0]?, t₂[0]? with
      | none, _ => false
      | _, none => true
      | some (c₀, c₁), some (d₀, d₁) => c₀ > d₀ || (c₀ = d₀ && c₁ > d₁)
  loop heap #[]
where
  loop (heap : Batteries.BinaryHeap _ _) (acc : Array _) :=
    match heap.max with
    | none => acc
    | some a =>
      let acc := match a[0]?, acc[0]? with
        | none, _ => panic! "empty array"
        | some (c₀, c₁), none => acc.push (c₀, c₁)
        | some (c₀, c₁), some (d₀, d₁) =>
          if c₁ + 1 ≥ d₀ then
            acc.pop.push (d₀, max c₁ d₁)
          else
            acc.push (c₀, c₁)
      let a := a.pop
      let heap := if a.isEmpty then heap.popMax else heap.popMax.insert a
      loop heap acc

public abbrev propCompress (a : Array (UInt32 × UInt32)) : Array (UInt32 × UInt32) :=
  propMergeAndCompress #[a]

public def propCompressStats (a : Array (UInt32 × UInt32)) : Id <| Nat × Nat := do
  let mut ct := 0
  for (c₀, c₁) in a do
    ct := ct + (c₁.toNat - c₀.toNat)
  return (a.size, ct)

public partial def dataMergeAndCompress [BEq α] (as : Array (Array (UInt32 × UInt32 × α))) : Array (UInt32 × UInt32 × α) :=
  let heap : Batteries.BinaryHeap _ _ :=
    as.filter (! ·.isEmpty) |>.toBinaryHeap fun t₁ t₂ =>
      match t₁[0]?, t₂[0]? with
      | none, _ => false
      | _, none => true
      | some (c₀, c₁, _), some (d₀, d₁, _) => c₀ > d₀ || (c₀ = d₀ && c₁ > d₁)
  loop heap #[]
where
  loop (heap : Batteries.BinaryHeap _ _) (acc : Array _) :=
    match heap.max with
    | none => acc
    | some a =>
      let acc := match a[0]?, acc[0]? with
        | none, _ => panic! "empty array"
        | some (c₀, c₁, x), none => acc.push (c₀, c₁, x)
        | some (c₀, c₁, x), some (d₀, d₁, y) =>
          if x == y then
            if c₁ + 1 ≥ d₀ then
              acc.pop.push (d₀, max c₁ d₁, x)
            else
              acc.push (c₀, c₁, x)
          else if d₁ ≤ c₀ then
            panic! "incompatible overlapping data"
          else
            acc.push (c₀, c₁, x)
      let a := a.pop
      let heap := if a.isEmpty then heap.popMax else heap.popMax.insert a
      loop heap acc

public abbrev dataCompress [BEq α] (a : Array (UInt32 × UInt32 × α)) : Array (UInt32 × UInt32 × α) :=
  dataMergeAndCompress #[a]

public def dataCompressStats (a : Array (UInt32 × UInt32 × α)) : Id <| Nat × Nat := do
  let mut ct := 0
  for (c₀, c₁, _) in a do
    ct := ct + (c₁.toNat - c₀.toNat)
  return (a.size, ct)
