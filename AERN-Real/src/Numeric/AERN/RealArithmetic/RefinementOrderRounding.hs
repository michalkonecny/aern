{-|
    Module      :  Numeric.AERN.RefinementOrderRounding
    Description :  common arithmetical operations rounded in/out  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Common arithmetical operations rounded in/out.
    
    This module is meant to be imported qualified.
    It is recommended to use the prefix ArithInOut.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding
(
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.Numerals,
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.RoundedOps,
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps,
    module Numeric.AERN.RealArithmetic.RefinementOrderRounding.Elementary
)
where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Numerals
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.RoundedOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Elementary
