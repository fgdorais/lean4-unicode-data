module
import UnicodeData.Basic

namespace Unicode.Tables

def mkDecompositionMapping : IO <| Array (UInt32 × String) := do
  let mut t := #[]
  for data in UnicodeData.data do
    match data.decomp with
    | some ⟨none, l⟩ =>
      t := t.push (data.code, ";" ++ ";".intercalate (l.map (toHexStringRaw <| Char.val .)))
    | some ⟨some k, l⟩ =>
      t := t.push (data.code, s!"{k};" ++ ";".intercalate (l.map (toHexStringRaw <| Char.val ·)))
    | _ => continue
  return t
