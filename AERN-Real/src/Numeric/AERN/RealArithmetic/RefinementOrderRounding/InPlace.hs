{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace
    Description :  common arithmetical operations rounded up/down  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    In-place versions of common arithmetical operations rounded in/out.
    
    This module is hidden and reexported via its parent RefinementOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace
(
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.FieldOps,
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.MixedFieldOps,
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.Elementary
)
where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.FieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.MixedFieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.Elementary

