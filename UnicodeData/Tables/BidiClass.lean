module
public import UnicodeBasic
import UnicodeData.Basic

namespace Unicode.Tables

public def mkBidiClass : IO <| Array (UInt32 × UInt32 × BidiClass) := do
  let mut t := #[]
  for d in UnicodeData.data do
    if d.name.takeEnd 7 == ", Last>" then
      match t.back? with
      | some (c₀, _, bc) =>
        t := t.pop.push (c₀, d.code, bc)
      | none => unreachable!
    else
      match t.back? with
      | some (c₀, c₁, bc) =>
        if d.code = c₁ + 1 && d.bidi == bc then
          t := t.pop.push (c₀, c₁+1, bc)
        else
          t := t.push (d.code, d.code, d.bidi)
      | none =>
        t := t.push (d.code, d.code, d.bidi)
  return t
