module
import UnicodeData.PropList

namespace Unicode.Tables

public def mkVariationSelector : Array (UInt32 × UInt32) :=
  PropList.data.variationSelector.map fun
    | (c₀, some c₁) => (c₀, c₁)
    | (c₀, none) => (c₀, c₀)
