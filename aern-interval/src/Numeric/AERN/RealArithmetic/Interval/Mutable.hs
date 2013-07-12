{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
    module Numeric.AERN.RealArithmetic.Interval.Mutable.ElementaryFromFieldOps
)
where

import Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps ()
import Numeric.AERN.RealArithmetic.Interval.Mutable.FieldOps ()
import Numeric.AERN.RealArithmetic.Interval.Mutable.MixedFieldOps ()
import Numeric.AERN.RealArithmetic.Interval.Mutable.ElementaryFromFieldOps

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Interval
import Numeric.AERN.RealArithmetic.Interval ()

import Numeric.AERN.Basics.ShowInternals

instance
    (ShowInternals e,
     ArithUpDn.RoundedRealInPlace e, NumOrd.HasExtrema e,
     ArithInOut.RoundedField (Distance e), 
     Neg (Distance e),
     RefOrd.RoundedLattice (Distance e)) => 
    ArithInOut.RoundedRealInPlace (Interval e)
