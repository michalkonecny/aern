{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval
    Description :  instances of arithmetic classes for Intervals  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Instances of arithmetic classes for Intervals.
-}

module Numeric.AERN.RealArithmetic.Interval
(
    module Numeric.AERN.RealArithmetic.Interval.ExactOps,
    module Numeric.AERN.RealArithmetic.Interval.Measures,
    module Numeric.AERN.RealArithmetic.Interval.Conversion,
    module Numeric.AERN.RealArithmetic.Interval.FieldOps,
    module Numeric.AERN.RealArithmetic.Interval.MixedFieldOps,
    module Numeric.AERN.RealArithmetic.Interval.SpecialConst
)
where

import Numeric.AERN.RealArithmetic.Interval.ExactOps
import Numeric.AERN.RealArithmetic.Interval.Measures
import Numeric.AERN.RealArithmetic.Interval.Conversion
import Numeric.AERN.RealArithmetic.Interval.FieldOps
import Numeric.AERN.RealArithmetic.Interval.MixedFieldOps
import Numeric.AERN.RealArithmetic.Interval.SpecialConst

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Interval

instance 
    (ArithUpDn.Convertible Integer e, 
     Eq e, ShowInternals e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e, 
     HasZero e,
     ArithUpDn.RoundedRing e, 
     ArithUpDn.RoundedAbs e) => 
    Num (Interval e)
    where
    negate = neg
    (+) = (<+>)
    (*) = (<*>)
    abs a = ArithInOut.absOutEff (ArithInOut.absDefaultEffort a) a
    fromInteger n = 
        result
        where
        result =
            ArithInOut.convertOutEff (ArithInOut.convertDefaultEffort n result) n
    signum a =
        error $ "signum not implemented for Interval"

instance 
    (ArithUpDn.Convertible Integer e, 
     ArithUpDn.Convertible Rational e, 
     Eq e, ShowInternals e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e, 
     HasZero e, HasOne e, NumOrd.HasExtrema e,
     ArithUpDn.RoundedField e, 
     ArithUpDn.RoundedAbs e) => 
    Fractional (Interval e)
    where
    (/) = (</>)
    fromRational r = 
        result
        where
        result =
            ArithInOut.convertOutEff (ArithInOut.convertDefaultEffort r result) r
        