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

import Prelude hiding (EQ, LT, GT)
import qualified Prelude
import Numeric.AERN.Basics.PartialOrdering

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

import Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Exception

import Control.Exception

instance (HasLegalValues e) => HasLegalValues (Interval e) where
    isLegal (Interval l h) = isLegal l && isLegal h 

instance
    (NumOrd.PartialComparison e, ShowInternals e) =>
    Eq (Interval e)
    where
    i1 == i2 =
        case i1 |==? i2 of 
            Just r -> r
            _ -> throw $ 
                AERNException $
                    "equality cannot be decided for: " 
                    ++ show i1 ++ " == "
                    ++ show i2
                    ++ "\n consider replacing == with NumericOrder.OpsDefaultEffort.|==?"
                    ++ "\n                    or with NumericOrder.OpsImplicitEffort.|==?"
                    ++ "\n                    or with NumericOrder.pCompareEff"

instance
    (NumOrd.PartialComparison e, ShowInternals e, NumOrd.RoundedLattice e) =>
    Ord (Interval e)
    where
    compare i1 i2 =
        case NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort i1) i1 i2 of 
            Just EQ -> Prelude.EQ
            Just LT -> Prelude.LT
            Just GT -> Prelude.GT
            _ -> throw $ 
                AERNException $
                    "comparison cannot be decided for: compare " 
                    ++ show i1 ++ " "
                    ++ show i2
                    ++ "\n consider replacing with ops defined at NumericOrder.OpsDefaultEffort"
                    ++ "\n                                  or at NumericOrder.OpsImplicitEffort"
                    ++ "\n                 or with NumericOrder.pCompareEff"
    max i1 i2 = maxOut i1 i2
    min i1 i2 = minOut i1 i2

instance 
    (ArithUpDn.Convertible Integer e, 
     ShowInternals e,
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
    abs = absOut
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

instance
    (ArithUpDn.Convertible Integer e,
     ArithUpDn.Convertible Rational e,
     Eq e,
     ShowInternals e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e,
     HasZero e,
     ArithUpDn.RoundedField e,
     NumOrd.HasExtrema e,
     ArithUpDn.RoundedAbs e,
     ArithUpDn.RoundedSpecialConst e,
     ArithInOut.RoundedExponentiation (Interval e),
     ArithInOut.RoundedSquareRoot (Interval e)) =>
    Floating (Interval e)
    where
    pi = piOut
    exp = expOut
    sqrt = sqrtOut



