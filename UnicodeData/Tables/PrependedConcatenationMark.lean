module
import UnicodeData.PropList

namespace Unicode.Tables

public def mkPrependedConcatenationMark : Array (UInt32 × UInt32) :=
  PropList.data.prependedConcatenationMark.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)
