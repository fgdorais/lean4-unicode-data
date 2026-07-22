module
import UnicodeData.Basic

namespace Unicode.Tables

partial def mkCanonicalDecompositionMapping : IO <| Array (UInt32 × List Char) := do
  let mut t := #[]
  for data in UnicodeData.data do
    match data.decomp with
    | some ⟨none, l⟩ =>
      t := t.push (data.code, fullDecomposition l)
    | _ => continue
  return t
where
  fullDecomposition : List Char → List Char
  | [] => unreachable!
  | h :: t =>
    match (getUnicodeData h).decomp with
    | some ⟨none, l⟩ => fullDecomposition (l ++ t)
    | _ => h :: t
