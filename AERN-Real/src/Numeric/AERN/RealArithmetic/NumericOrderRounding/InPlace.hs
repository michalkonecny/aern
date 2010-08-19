{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace
    Description :  common arithmetical operations rounded up/down  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    In-place versions of common arithmetical operations rounded up/down.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace
(
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.FieldOps,
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.MixedFieldOps,
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.Elementary
)
where

import Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.FieldOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.MixedFieldOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.Elementary

