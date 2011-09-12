{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Mutable
    Description :  instances of mutable arithmetic classes for MIntervals  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Instances of mutable arithmetic classes for MIntervals.
-}

module Numeric.AERN.RealArithmetic.Interval.Mutable
(
    module Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps,
    module Numeric.AERN.RealArithmetic.Interval.Mutable.FieldOps,
    module Numeric.AERN.RealArithmetic.Interval.Mutable.MixedFieldOps
)
where

import Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps
import Numeric.AERN.RealArithmetic.Interval.Mutable.FieldOps
import Numeric.AERN.RealArithmetic.Interval.Mutable.MixedFieldOps

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.Basics.Interval
import Numeric.AERN.RealArithmetic.Interval

instance 
    (ArithUpDn.RoundedRealInPlace e, NumOrd.HasExtrema e) => 
    ArithInOut.RoundedRealInPlace (Interval e)
