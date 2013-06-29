{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Floating
    Description :  Floating instance using outer rounding
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Floating instance using outer rounding.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.Floating where

import Prelude hiding (EQ, LT, GT)
import qualified Prelude
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.RealArithmetic.Interval.ExactOps ()
import Numeric.AERN.RealArithmetic.Interval.Measures ()
import Numeric.AERN.RealArithmetic.Interval.Conversion ()
import Numeric.AERN.RealArithmetic.Interval.FieldOps ()
import Numeric.AERN.RealArithmetic.Interval.MixedFieldOps ()
import Numeric.AERN.RealArithmetic.Interval.SpecialConst ()

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.RefinementOrder.Operators

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.Operators

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.Exception
import Control.Exception


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
        case NumOrd.pCompare i1 i2 of 
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
    max i1 i2 = NumOrd.maxOut i1 i2
    min i1 i2 = NumOrd.minOut i1 i2

instance 
    (ArithUpDn.Convertible Integer e, 
     HasSampleFromContext e, 
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
    abs = ArithInOut.absOut
    fromInteger n = 
        result
        where
        result :: Interval e =
            ArithInOut.convertOutEff (ArithUpDn.convertDefaultEffort n sample) sampleInt n
            where
            sampleInt = Interval sample sample
            sample :: e
            sample = sampleFromContext
    signum _ =
        error $ "signum not implemented for Interval"

instance 
    (ArithUpDn.Convertible Integer e, 
     ArithUpDn.Convertible Rational e,
     HasSampleFromContext e, 
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
        result :: Interval e =
            ArithInOut.convertOutEff (ArithUpDn.convertDefaultEffort r sample) sampleInt r
            where
            sampleInt = Interval sample sample
            sample :: e
            sample = sampleFromContext

instance
    (ArithUpDn.Convertible Integer e,
     ArithUpDn.Convertible Rational e,
     HasSampleFromContext e,
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
    pi = error $ "AERN: Floating instance of Interval data type: for the pi constant use piOut or piIn with a sample value parameter to determine the precision from."
    exp = ArithInOut.expOut
    sqrt = ArithInOut.sqrtOut
