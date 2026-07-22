module
import UnicodeData.Basic
import UnicodeData.Tables.NoncharacterCodePoint
import UnicodeData.Tables.Utils

namespace Unicode.Tables

def mkName : IO <| Array (UInt32 × UInt32 × String) := do
  let mut t := #[(0,0,"<Control>")]
  for i in [1:UnicodeData.data.size] do
    let data := UnicodeData.data[i]!
    let c := data.code
    let n := data.name.copy
    if n.takeEnd 8 == ", First>" then
      if "<CJK Ideograph".isPrefixOf n then
        t := t.push (c, c, "<CJK Unified Ideograph>")
      else if "<Tangut Ideograph".isPrefixOf n then
        t := t.push (c, c, "<Tangut Ideograph>")
      else if n.takeEnd 17 == "Surrogate, First>" then
        match t.back! with
        | (c₀, c₁, n₀) =>
          if c == c₁ + 1 && n₀ == "<Surrogate>" then
            t := t.pop.push (c₀, c, "<Surrogate>")
          else
            t := t.push (c, c, "<Surrogate>")
      else if n.takeEnd 19 == "Private Use, First>" then
        t := t.push (c, c, "<Private Use>")
      else
        t := t.push (c, c, (n.dropEnd 8).copy ++ ">")
    else if n.takeEnd 7 == ", Last>" then
      match t.back! with
      | (c₀, _, n₀) =>
        t := t.pop.push (c₀, c, n₀)
    else if n == "<control>" then
      match t.back! with
      | (c₀, _, n₀) =>
        if n₀ == "<Control>" then
          t := t.pop.push (c₀, c, n₀)
        else
          t := t.push (c, c, "<Control>")
    else if "CJK COMPATIBILITY IDEOGRAPH-".isPrefixOf n then
      match t.back! with
      | (c₀, c₁, n) =>
        if c == c₁ + 1 && n == "<CJK Compatibility Ideograph>" then
          t := t.pop.push (c₀, c, n)
        else
          t := t.push (c, c, "<CJK Compatibility Ideograph>")
    else if "KHITAN SMALL SCRIPT CHARACTER-".isPrefixOf n then
      match t.back! with
      | (c₀, c₁, n) =>
        if c == c₁ + 1 && n == "<Khitan Small Script Character>" then
          t := t.pop.push (c₀, c, n)
        else
          t := t.push (c, c, "<Khitan Small Script Character>")
    else if "NUSHU CHARACTER-".isPrefixOf n then
      match t.back! with
      | (c₀, c₁, n) =>
        if c == c₁ + 1 && n == "<Nushu Character>" then
          t := t.pop.push (c₀, c, n)
        else
          t := t.push (c, c, "<Nushu Character>")
    else if "TANGUT COMPONENT-".isPrefixOf n then
      match t.back! with
      | (c₀, c₁, n) =>
        if c == c₁ + 1 && n == "<Tangut Component>" then
          t := t.pop.push (c₀, c, n)
        else
          t := t.push (c, c, "<Tangut Component>")
    else
      match t.back! with
      | (c₀, c₁, n₀) =>
        if c == c₁ + 1 && n == n₀ then
          t := t.pop.push (c₀, c, n)
        else
          t := t.push (c, c, n)
  return dataMergeAndCompress #[t, mkNoncharacterCodePoint.map fun (c₀, c₁) => (c₀, c₁, "<Reserved>")]
