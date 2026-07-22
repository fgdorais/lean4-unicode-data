module
public import UnicodeBasic
import UnicodeData.Basic

namespace Unicode.Tables

public def mkNumericValue : IO <| Array (UInt32 × UInt32 × NumericType) := do
  let mut t := #[]
  for d in UnicodeData.data do
    match d.numeric with
    | some (.decimal 0) =>
      t := t.push (d.code, d.code + 9, NumericType.decimal 0)
    | some (.digit v) =>
      match t.back! with
      | (c₀, c₁, n@(NumericType.digit x)) =>
        let last := x.val + c₁.toNat - c₀.toNat
        if d.code == c₁ + 1 && v.val == last + 1 then
          t := t.pop.push (c₀, d.code, n)
        else
          t := t.push (d.code, d.code, .digit v)
      | _ =>
        t := t.push (d.code, d.code, .digit v)
    | some n@(.numeric _ _) =>
      t := t.push (d.code, d.code, n)
    | _ => continue
  return t
