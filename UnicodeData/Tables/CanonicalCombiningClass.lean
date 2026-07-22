module
import UnicodeData.Basic

namespace Unicode.Tables

def mkCanonicalCombiningClass : IO <| Array (UInt32 × UInt32 × Nat) := do
  let mut t := #[]
  for d in UnicodeData.data do
    if d.cc > 0 then
      match t.back? with
      | some (c₀, c₁, cc) =>
        if t.size != 0 && d.code == c₁ + 1 && d.cc == cc then
          t := t.pop.push (c₀, c₁+1, cc)
        else
          t := t.push (d.code, d.code, d.cc)
      | none =>
        t := t.push (d.code, d.code, d.cc)
  return t
