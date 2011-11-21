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

module Numeric.AERN.RealArithmetic.Interval.MixedFieldOps 
()
where

import Numeric.AERN.Basics.Interval

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

instance (ArithUpDn.RoundedMixedAddEffort e tn) => 
    RoundedMixedAddEffort (Interval e) tn 
    where
    type MixedAddEffortIndicator (Interval e) tn = ArithUpDn.MixedAddEffortIndicator e tn
    mixedAddDefaultEffort (Interval l r) n = ArithUpDn.mixedAddDefaultEffort l n

instance (ArithUpDn.RoundedMixedAdd e tn) => 
    RoundedMixedAdd (Interval e) tn 
    where
    mixedAddInEff effort (Interval l2 r2) n =
        Interval
            (ArithUpDn.mixedAddUpEff effort l2 n)
            (ArithUpDn.mixedAddDnEff effort r2 n)
    mixedAddOutEff effort (Interval l2 r2) n =
        Interval 
            (ArithUpDn.mixedAddDnEff effort l2 n)
            (ArithUpDn.mixedAddUpEff effort r2 n)

instance (ArithUpDn.RoundedMixedMultiplyEffort e tn,
          NumOrd.PartialComparison tn, NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedMultiplyEffort (Interval e) tn 
    where
    type MixedMultEffortIndicator (Interval e) tn = 
        ((NumOrd.PartialCompareEffortIndicator tn, 
          NumOrd.PartialCompareEffortIndicator e), 
         NumOrd.MinmaxEffortIndicator e,
         ArithUpDn.MixedMultEffortIndicator e tn)
    mixedMultDefaultEffort (Interval l r) n = 
        ((NumOrd.pCompareDefaultEffort n, 
          NumOrd.pCompareDefaultEffort l), 
         NumOrd.minmaxDefaultEffort l,
         ArithUpDn.mixedMultDefaultEffort l n) 

instance (ArithUpDn.RoundedMixedMultiply e tn,
          HasZero tn, HasZero e,
          NumOrd.PartialComparison tn, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedMultiply (Interval e) tn 
    where
    mixedMultInEff ((effortCompS, effortCompE), effortMinmax, effortMult) i n =
        fromEndpoints $
        multiplySingletonWithInterval 
            (pNonnegNonposEff effortCompS)
            (pNonnegNonposEff effortCompE)
            (flip $ ArithUpDn.mixedMultUpEff effortMult)
            (flip $ ArithUpDn.mixedMultDnEff effortMult) 
            (NumOrd.maxUpEff effortMinmax) 
            (NumOrd.minDnEff effortMinmax)
            n i
    mixedMultOutEff ((effortCompS, effortCompE), effortMinmax, effortMult) i n =
        fromEndpoints $
        multiplySingletonWithInterval 
            (pNonnegNonposEff effortCompS)
            (pNonnegNonposEff effortCompE)
            (flip $ ArithUpDn.mixedMultDnEff effortMult) 
            (flip $ ArithUpDn.mixedMultUpEff effortMult)
            (NumOrd.minDnEff effortMinmax)
            (NumOrd.maxUpEff effortMinmax) 
            n i

multiplySingletonWithInterval 
        sNonnegNonpos iNonnegNonpos timesL timesR 
        combineL combineR
        s1 (Interval l2 r2) =
    let _ = [combineL, combineR] in
        case (sNonnegNonpos s1, -- sign of s1 
              iNonnegNonpos l2, -- sign of l2
              iNonnegNonpos r2 -- sign of r2 
             ) of
             
            -- s1 is zero
            ((Just True, Just True), _, _) -> 
                (zero, zero)
 
            -- s1 non negative
            ((Just True, _), _, _) -> 
                (s1 `timesL` l2, s1 `timesR` r2)
            
            -- s1 non positive
            ((_, Just True), _, _) -> 
                (s1 `timesL` r2, s1 `timesR` l2)

            -- nothing known about s1, i2 positive
            (_, (Just True, _), (Just True, _)) -> 
                ((s1 `timesL` r2) `combineL` (s1 `timesL` l2), 
                 (s1 `timesR` r2) `combineR` (s1 `timesR` l2))

            -- nothing known about s1, i2 negative
            (_, (_, Just True), (_, Just True)) -> 
                ((s1 `timesL` r2) `combineL` (s1 `timesL` l2), 
                 (s1 `timesR` r2) `combineR` (s1 `timesR` l2))

            -- both s1 and i2 are around zero
            _ ->
                ((s1 `timesL` l2) `combineL` (s1 `timesL` r2) `combineL` zero,
                 (s1 `timesR` l2) `combineR` (s1 `timesR` r2) `combineR` zero) 
                -- need to include zero to account for 
                -- consistent vs anti-consistent cases giving constant 0

instance (RoundedDivideEffort (Interval e),
          Convertible tn (Interval e)) => 
    RoundedMixedDivideEffort (Interval e) tn 
    where
    type MixedDivEffortIndicator (Interval e) tn =
        (DivEffortIndicator (Interval e), 
         ConvertEffortIndicator tn (Interval e))
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion

instance (RoundedDivide (Interval e),
          Convertible tn (Interval e)) => 
    RoundedMixedDivide (Interval e) tn 
    where
    mixedDivInEff = mixedDivInEffByConversion
    mixedDivOutEff = mixedDivOutEffByConversion
    
instance (RoundedMixedAddEffort (Interval e) tn,
          RoundedMixedMultiplyEffort (Interval e) tn, 
          NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e,
          NumOrd.PartialComparison tn,
          ArithUpDn.RoundedMixedRingEffort e tn) => 
        RoundedMixedRingEffort (Interval e) tn
    where
    type MixedRingOpsEffortIndicator (Interval e) tn =
        (ArithUpDn.MixedRingOpsEffortIndicator e tn,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator tn))
    mixedRingOpsDefaultEffort i@(Interval l r) n =
        (ArithUpDn.mixedRingOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxringEffortAdd (Interval l r) n (effortRing, _) = 
        ArithUpDn.mxringEffortAdd l n effortRing
    mxringEffortMult (Interval l r) n (effortRing, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxringEffortMult l n effortRing) 

instance (RoundedMixedAdd (Interval e) tn,
          RoundedMixedMultiply (Interval e) tn,
          RoundedMixedRingEffort (Interval e) tn) => 
        RoundedMixedRing (Interval e) tn
    
instance (RoundedMixedRingEffort (Interval e) tn,
          RoundedMixedDivideEffort (Interval e) tn,
          NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e,
          NumOrd.PartialComparison tn,
          ArithUpDn.RoundedMixedFieldEffort e tn) => 
        RoundedMixedFieldEffort (Interval e) tn
    where
    type MixedFieldOpsEffortIndicator (Interval e) tn =
        (ArithUpDn.MixedFieldOpsEffortIndicator e tn,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator tn),
         MixedDivEffortIndicator (Interval e) tn)
    mixedFieldOpsDefaultEffort i@(Interval l r) n =
        (ArithUpDn.mixedFieldOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n),
         mixedDivDefaultEffort i n)
    mxfldEffortAdd (Interval l r) n (effortFld, _, _) = 
        ArithUpDn.mxfldEffortAdd l n effortFld
    mxfldEffortMult (Interval l r) n (effortFld, (effortCompEpt, effortMinmax, effortCompS), _) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortMult l n effortFld) 
    mxfldEffortDiv _ _ (_, _, effortDiv) = effortDiv
    
instance (RoundedMixedRing (Interval e) tn,
          RoundedMixedDivide (Interval e) tn,
          RoundedMixedFieldEffort (Interval e) tn) => 
        RoundedMixedField (Interval e) tn
    