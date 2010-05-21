{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding
    Description :  common arithmetical operations rounded up/down  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Common arithmetical operations rounded up/down.
    
    This module is meant to be imported qualified.
    It is recommended to use the prefix ArithUpDn.
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding 
(
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.Conversion,
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps,
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.MixedFieldOps,
    module Numeric.AERN.RealArithmetic.NumericOrderRounding.Elementary
)
where

import Numeric.AERN.RealArithmetic.NumericOrderRounding.Conversion
import Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.MixedFieldOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Elementary
