module
import UnicodeData.Basic

namespace Unicode.Tables

public def mkBidiMirrored : IO <| Array (UInt32 × UInt32) := do
  let mut t := #[]
  for d in UnicodeData.data do
    if d.bidiMirrored then
      match t.back? with
      | some (c₀, c₁) =>
        if d.code == c₁ + 1 then
          t := t.pop.push (c₀, d.code)
        else
          t := t.push (d.code, d.code)
      | none =>
        t := t.push (d.code, d.code)
  return t
