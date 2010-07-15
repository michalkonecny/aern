{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations mixing an interval and another type.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.MixedFieldOps where

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.CInterval

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

instance (ArithUpDn.RoundedMixedAdd t e) => RoundedMixedAdd t (Interval e) where
    type MixedAddEffortIndicator t (Interval e) = ArithUpDn.MixedAddEffortIndicator t e
    mixedAddDefaultEffort t (Interval l h) = ArithUpDn.mixedAddDefaultEffort t l
    mixedAddInEff effort e (Interval l2 h2) =
        Interval
            (ArithUpDn.mixedAddUpEff effort e l2)
            (ArithUpDn.mixedAddDnEff effort e h2)
    mixedAddOutEff effort e (Interval l2 h2) =
        Interval 
            (ArithUpDn.mixedAddDnEff effort e l2)
            (ArithUpDn.mixedAddUpEff effort e h2)

instance (ArithUpDn.RoundedMixedMultiply t e,
          HasZero t, HasZero e,
          NumOrd.PartialComparison t, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
        RoundedMixedMultiply t (Interval e) where
    type MixedMultEffortIndicator t (Interval e) = 
        ((NumOrd.PartialCompareEffortIndicator t, 
          NumOrd.PartialCompareEffortIndicator e), 
         NumOrd.MinmaxEffortIndicator e,
         ArithUpDn.MixedMultEffortIndicator t e)
    mixedMultDefaultEffort s (Interval l h) = 
        ((NumOrd.pCompareDefaultEffort s, 
          NumOrd.pCompareDefaultEffort l), 
         NumOrd.minmaxDefaultEffort l,
         ArithUpDn.mixedMultDefaultEffort s l) 
    mixedMultInEff ((effortCompS, effortCompE), effortMinmax, effortMult) s i2 =
        fromEndpoints $
        multiplySingletonWithInterval 
            (pNonnegNonposEff effortCompS)
            (pNonnegNonposEff effortCompE)
            (ArithUpDn.mixedMultUpEff effortMult)
            (ArithUpDn.mixedMultDnEff effortMult) 
            (NumOrd.maxUpEff effortMinmax) 
            (NumOrd.minDnEff effortMinmax)
            s i2
    mixedMultOutEff ((effortCompS, effortCompE), effortMinmax, effortMult) s i2 =
        fromEndpoints $
        multiplySingletonWithInterval 
            (pNonnegNonposEff effortCompS)
            (pNonnegNonposEff effortCompE)
            (ArithUpDn.mixedMultDnEff effortMult) 
            (ArithUpDn.mixedMultUpEff effortMult)
            (NumOrd.minDnEff effortMinmax)
            (NumOrd.maxUpEff effortMinmax) 
            s i2

multiplySingletonWithInterval 
        sNonnegNonpos iNonnegNonpos timesL timesR 
        combineL combineR
        s1 (Interval l2 h2) =
    let _ = [combineL, combineR] in
        case (sNonnegNonpos s1, -- sign of s1 
              iNonnegNonpos l2, -- sign of l2
              iNonnegNonpos h2 -- sign of h2 
             ) of
             
            -- s1 is zero
            (Just (True, True), _, _) -> 
                (zero, zero)
 
            -- s1 non negative
            (Just (True, _), _, _) -> 
                (s1 `timesL` l2, s1 `timesR` h2)
            
            -- s1 non positive
            (Just (_, True), _, _) -> 
                (s1 `timesL` h2, s1 `timesR` l2)

            -- nothing known about s1, i2 positive
            (_, Just (True, _), Just (True, _)) -> 
                ((s1 `timesL` h2) `combineL` (s1 `timesL` l2), 
                 (s1 `timesR` h2) `combineR` (s1 `timesR` l2))

            -- nothing known about s1, i2 negative
            (_, Just (_, True), Just (_, True)) -> 
                ((s1 `timesL` h2) `combineL` (s1 `timesL` l2), 
                 (s1 `timesR` h2) `combineR` (s1 `timesR` l2))

            -- both s1 and i2 are around zero
            _ ->
                ((s1 `timesL` l2) `combineL` (s1 `timesL` h2) `combineL` zero,
                 (s1 `timesR` l2) `combineR` (s1 `timesR` h2) `combineR` zero) 
                -- need to include zero to account for 
                -- consistent vs anti-consistent cases giving constant 0

instance (RoundedDivide (Interval e),
          Convertible t (Interval e)) => 
        RoundedMixedDivide t (Interval e) where
    type MixedDivEffortIndicator t (Interval e) =
        (DivEffortIndicator (Interval e), 
         ConvertEffortIndicator t (Interval e))
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion
    mixedDivInEff = mixedDivInEffByConversion
    mixedDivOutEff = mixedDivOutEffByConversion
    
    
    