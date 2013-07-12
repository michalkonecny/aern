{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
(
    intervalMixedAddInEff, intervalMixedAddOutEff,
    IntervalMixedMultEffortIndicator, intervalMixedMultDefaultEffort,
    intervalMixedMultInEff, intervalMixedMultOutEff,
    IntervalMixedDivEffortIndicator, intervalMixedDivDefaultEffort,
    intervalMixedDivInEff, intervalMixedDivOutEff
)
where

import Numeric.AERN.Basics.Interval

import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Interval.ExactOps
import Numeric.AERN.RealArithmetic.Interval.FieldOps ()

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.RealArithmetic.RefinementOrderRounding

import qualified Numeric.AERN.NumericOrder as NumOrd
--import qualified Numeric.AERN.RefinementOrder as RefOrd

{---- mixed addition ----}

instance (ArithUpDn.RoundedMixedAddEffort e Integer) => 
    RoundedMixedAddEffort (Interval e) Integer 
    where
    type MixedAddEffortIndicator (Interval e) Integer = ArithUpDn.MixedAddEffortIndicator e Integer
    mixedAddDefaultEffort (Interval l _) n = ArithUpDn.mixedAddDefaultEffort l n

instance (ArithUpDn.RoundedMixedAdd e Integer) => 
    RoundedMixedAdd (Interval e) Integer 
    where
    mixedAddInEff = intervalMixedAddInEff
    mixedAddOutEff = intervalMixedAddOutEff 
    
instance (ArithUpDn.RoundedMixedAddEffort e Int) => 
    RoundedMixedAddEffort (Interval e) Int 
    where
    type MixedAddEffortIndicator (Interval e) Int = ArithUpDn.MixedAddEffortIndicator e Int
    mixedAddDefaultEffort (Interval l _) n = ArithUpDn.mixedAddDefaultEffort l n

instance (ArithUpDn.RoundedMixedAdd e Int) => 
    RoundedMixedAdd (Interval e) Int 
    where
    mixedAddInEff = intervalMixedAddInEff
    mixedAddOutEff = intervalMixedAddOutEff 
    
    
instance (ArithUpDn.RoundedMixedAddEffort e Rational) => 
    RoundedMixedAddEffort (Interval e) Rational 
    where
    type MixedAddEffortIndicator (Interval e) Rational = ArithUpDn.MixedAddEffortIndicator e Rational
    mixedAddDefaultEffort (Interval l _) n = ArithUpDn.mixedAddDefaultEffort l n

instance (ArithUpDn.RoundedMixedAdd e Rational) => 
    RoundedMixedAdd (Interval e) Rational 
    where
    mixedAddInEff = intervalMixedAddInEff
    mixedAddOutEff = intervalMixedAddOutEff 
    
instance (ArithUpDn.RoundedMixedAddEffort e Double) => 
    RoundedMixedAddEffort (Interval e) Double 
    where
    type MixedAddEffortIndicator (Interval e) Double = ArithUpDn.MixedAddEffortIndicator e Double
    mixedAddDefaultEffort (Interval l _) n = ArithUpDn.mixedAddDefaultEffort l n

instance (ArithUpDn.RoundedMixedAdd e Double) => 
    RoundedMixedAdd (Interval e) Double 
    where
    mixedAddInEff = intervalMixedAddInEff
    mixedAddOutEff = intervalMixedAddOutEff 
    
instance (ArithUpDn.RoundedMixedAddEffort e (Interval e2)) => 
    RoundedMixedAddEffort (Interval e) (Interval e2) 
    where
    type MixedAddEffortIndicator (Interval e) (Interval e2) = ArithUpDn.MixedAddEffortIndicator e (Interval e2)
    mixedAddDefaultEffort (Interval l _) n = ArithUpDn.mixedAddDefaultEffort l n

instance (ArithUpDn.RoundedMixedAdd e (Interval e2)) => 
    RoundedMixedAdd (Interval e) (Interval e2) 
    where
    mixedAddInEff = intervalMixedAddInEff
    mixedAddOutEff = intervalMixedAddOutEff 
    
intervalMixedAddInEff, intervalMixedAddOutEff :: 
    ArithUpDn.RoundedMixedAdd e tn 
    =>
    ArithUpDn.MixedAddEffortIndicator e tn
    -> Interval e -> tn -> Interval e
intervalMixedAddInEff effort (Interval l2 r2) n =
    Interval
        (ArithUpDn.mixedAddUpEff effort l2 n)
        (ArithUpDn.mixedAddDnEff effort r2 n)
intervalMixedAddOutEff effort (Interval l2 r2) n =
    Interval 
        (ArithUpDn.mixedAddDnEff effort l2 n)
        (ArithUpDn.mixedAddUpEff effort r2 n)


{---- mixed multiplication ----}

instance (ArithUpDn.RoundedMixedMultiplyEffort e Integer,
          NumOrd.PartialComparison Integer, NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedMultiplyEffort (Interval e) Integer 
    where
    type MixedMultEffortIndicator (Interval e) Integer =
        IntervalMixedMultEffortIndicator e Integer 
    mixedMultDefaultEffort =
        intervalMixedMultDefaultEffort
     
instance (ArithUpDn.RoundedMixedMultiply e Integer,
          HasZero Integer, HasZero e,
          NumOrd.PartialComparison Integer, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedMultiply (Interval e) Integer 
    where
    mixedMultInEff = intervalMixedMultInEff
    mixedMultOutEff = intervalMixedMultOutEff

instance (ArithUpDn.RoundedMixedMultiplyEffort e Int,
          NumOrd.PartialComparison Int, NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedMultiplyEffort (Interval e) Int 
    where
    type MixedMultEffortIndicator (Interval e) Int = 
        IntervalMixedMultEffortIndicator e Int 
    mixedMultDefaultEffort =
        intervalMixedMultDefaultEffort

instance (ArithUpDn.RoundedMixedMultiply e Int,
          HasZero Int, HasZero e,
          NumOrd.PartialComparison Int, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedMultiply (Interval e) Int 
    where
    mixedMultInEff = intervalMixedMultInEff
    mixedMultOutEff = intervalMixedMultOutEff

instance (ArithUpDn.RoundedMixedMultiplyEffort e Rational,
          NumOrd.PartialComparison Rational, NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedMultiplyEffort (Interval e) Rational 
    where
    type MixedMultEffortIndicator (Interval e) Rational = 
        IntervalMixedMultEffortIndicator e Rational 
    mixedMultDefaultEffort =
        intervalMixedMultDefaultEffort

instance (ArithUpDn.RoundedMixedMultiply e Rational,
          HasZero Rational, HasZero e,
          NumOrd.PartialComparison Rational, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedMultiply (Interval e) Rational 
    where
    mixedMultInEff = intervalMixedMultInEff
    mixedMultOutEff = intervalMixedMultOutEff

instance (ArithUpDn.RoundedMixedMultiplyEffort e Double,
          NumOrd.PartialComparison Double, NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedMultiplyEffort (Interval e) Double 
    where
    type MixedMultEffortIndicator (Interval e) Double = 
        IntervalMixedMultEffortIndicator e Double 
    mixedMultDefaultEffort =
        intervalMixedMultDefaultEffort

instance (ArithUpDn.RoundedMixedMultiply e Double,
          HasZero Double, HasZero e,
          NumOrd.PartialComparison Double, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedMultiply (Interval e) Double 
    where
    mixedMultInEff = intervalMixedMultInEff
    mixedMultOutEff = intervalMixedMultOutEff

instance (ArithUpDn.RoundedMixedMultiplyEffort e (Interval e2),
          NumOrd.PartialComparison (Interval e2), NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedMultiplyEffort (Interval e) (Interval e2) 
    where
    type MixedMultEffortIndicator (Interval e) (Interval e2) = 
        IntervalMixedMultEffortIndicator e (Interval e2) 
    mixedMultDefaultEffort =
        intervalMixedMultDefaultEffort

instance (ArithUpDn.RoundedMixedMultiply e (Interval e2),
          HasZero (Interval e2), HasZero e,
          NumOrd.PartialComparison (Interval e2), NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedMultiply (Interval e) (Interval e2) 
    where
    mixedMultInEff = intervalMixedMultInEff
    mixedMultOutEff = intervalMixedMultOutEff

type IntervalMixedMultEffortIndicator e tn = 
    ((NumOrd.PartialCompareEffortIndicator tn, 
      NumOrd.PartialCompareEffortIndicator e), 
     NumOrd.MinmaxEffortIndicator e,
     ArithUpDn.MixedMultEffortIndicator e tn)

intervalMixedMultDefaultEffort :: 
     (NumOrd.PartialComparison t,
      NumOrd.PartialComparison tn,
      NumOrd.RoundedLatticeEffort t,
      ArithUpDn.RoundedMixedMultiplyEffort t tn) 
     =>
     Interval t
     -> tn
     -> ((NumOrd.PartialCompareEffortIndicator tn,
          NumOrd.PartialCompareEffortIndicator t),
         NumOrd.MinmaxEffortIndicator t,
         ArithUpDn.MixedMultEffortIndicator t tn)
intervalMixedMultDefaultEffort (Interval l _) n = 
    ((NumOrd.pCompareDefaultEffort n, 
      NumOrd.pCompareDefaultEffort l), 
     NumOrd.minmaxDefaultEffort l,
     ArithUpDn.mixedMultDefaultEffort l n) 

intervalMixedMultInEff, intervalMixedMultOutEff :: 
    (NumOrd.PartialComparison t, NumOrd.PartialComparison tn,
     NumOrd.RoundedLattice t, HasZero t, HasZero tn,
     ArithUpDn.RoundedMixedMultiply t tn) 
    =>
    ((NumOrd.PartialCompareEffortIndicator tn,
      NumOrd.PartialCompareEffortIndicator t),
     NumOrd.MinmaxEffortIndicator t,
     ArithUpDn.MixedMultEffortIndicator t tn)
    -> Interval t -> tn -> Interval t
intervalMixedMultInEff ((effortCompS, effortCompE), effortMinmax, effortMult) i@(Interval l _) n =
    fromEndpoints $
    multiplySingletonWithInterval 
        (pNonnegNonposEff effortCompS)
        (pNonnegNonposEff effortCompE)
        (flip $ ArithUpDn.mixedMultUpEff effortMult)
        (flip $ ArithUpDn.mixedMultDnEff effortMult) 
        (NumOrd.maxUpEff effortMinmax) 
        (NumOrd.minDnEff effortMinmax)
        (zero l) (zero l)
        n i
intervalMixedMultOutEff ((effortCompS, effortCompE), effortMinmax, effortMult) i@(Interval l _) n =
    fromEndpoints $
    multiplySingletonWithInterval 
        (pNonnegNonposEff effortCompS)
        (pNonnegNonposEff effortCompE)
        (flip $ ArithUpDn.mixedMultDnEff effortMult) 
        (flip $ ArithUpDn.mixedMultUpEff effortMult)
        (NumOrd.minDnEff effortMinmax)
        (NumOrd.maxUpEff effortMinmax)
        (zero l) (zero l)
        n i
        
multiplySingletonWithInterval :: 
    (tn -> (Maybe Bool, Maybe Bool))
    -> (e -> (Maybe Bool, Maybe Bool))
    -> (tn -> e -> e)
    -> (tn -> e -> e)
    -> (e -> e -> e)
    -> (e -> e -> e)
    -> e
    -> e
    -> tn
    -> Interval e
    -> (e, e)
multiplySingletonWithInterval 
        sNonnegNonpos iNonnegNonpos timesL timesR 
        combineL combineR
        zeroResL zeroResR
        s1 (Interval l2 r2) =
    let _ = [combineL, combineR] in
        case (sNonnegNonpos s1, -- sign of s1 
              iNonnegNonpos l2, -- sign of l2
              iNonnegNonpos r2 -- sign of r2 
             ) of
             
            -- s1 is zero
            ((Just True, Just True), _, _) -> 
                (zeroResL, zeroResR)
 
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
                ((s1 `timesL` l2) `combineL` (s1 `timesL` r2) `combineL` zeroResL,
                 (s1 `timesR` l2) `combineR` (s1 `timesR` r2) `combineR` zeroResR) 
                -- need to include zero to account for 
                -- consistent vs anti-consistent cases giving constant 0
        
{---- mixed division ----}        
        
instance (ArithUpDn.RoundedMixedDivideEffort e Integer,
          NumOrd.PartialComparison Integer, NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedDivideEffort (Interval e) Integer 
    where
    type MixedDivEffortIndicator (Interval e) Integer =
        IntervalMixedDivEffortIndicator e Integer
    mixedDivDefaultEffort =
        intervalMixedDivDefaultEffort

instance (ArithUpDn.RoundedMixedDivide e Integer,
          HasZero Integer, HasZero e, HasInfinities e,
          NumOrd.PartialComparison Integer, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedDivide (Interval e) Integer 
    where
    mixedDivInEff = intervalMixedDivInEff
    mixedDivOutEff = intervalMixedDivOutEff
    
instance (ArithUpDn.RoundedMixedDivideEffort e Int,
          NumOrd.PartialComparison Int, NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedDivideEffort (Interval e) Int 
    where
    type MixedDivEffortIndicator (Interval e) Int = 
        IntervalMixedDivEffortIndicator e Int
    mixedDivDefaultEffort =
        intervalMixedDivDefaultEffort

instance (ArithUpDn.RoundedMixedDivide e Int,
          HasZero Int, HasZero e, HasInfinities e,
          NumOrd.PartialComparison Int, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedDivide (Interval e) Int 
    where
    mixedDivInEff = intervalMixedDivInEff
    mixedDivOutEff = intervalMixedDivOutEff
    
instance (ArithUpDn.RoundedMixedDivideEffort e Rational,
          NumOrd.PartialComparison Rational, NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedDivideEffort (Interval e) Rational 
    where
    type MixedDivEffortIndicator (Interval e) Rational = 
        IntervalMixedDivEffortIndicator e Rational
    mixedDivDefaultEffort =
        intervalMixedDivDefaultEffort

instance (ArithUpDn.RoundedMixedDivide e Rational,
          HasZero Rational, HasZero e, HasInfinities e,
          NumOrd.PartialComparison Rational, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedDivide (Interval e) Rational 
    where
    mixedDivInEff = intervalMixedDivInEff
    mixedDivOutEff = intervalMixedDivOutEff
    
instance (ArithUpDn.RoundedMixedDivideEffort e Double,
          NumOrd.PartialComparison Double, NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedDivideEffort (Interval e) Double 
    where
    type MixedDivEffortIndicator (Interval e) Double = 
        IntervalMixedDivEffortIndicator e Double
    mixedDivDefaultEffort =
        intervalMixedDivDefaultEffort

instance (ArithUpDn.RoundedMixedDivide e Double,
          HasZero Double, HasZero e, HasInfinities e,
          NumOrd.PartialComparison Double, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedDivide (Interval e) Double 
    where
    mixedDivInEff = intervalMixedDivInEff
    mixedDivOutEff = intervalMixedDivOutEff
    
instance (ArithUpDn.RoundedMixedDivideEffort e (Interval e2),
          NumOrd.PartialComparison (Interval e2), NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedDivideEffort (Interval e) (Interval e2) 
    where
    type MixedDivEffortIndicator (Interval e) (Interval e2) = 
        IntervalMixedDivEffortIndicator e (Interval e2)
    mixedDivDefaultEffort =
        intervalMixedDivDefaultEffort

instance (ArithUpDn.RoundedMixedDivide e (Interval e2),
          HasZero (Interval e2), HasZero e, HasInfinities e,
          NumOrd.PartialComparison (Interval e2), NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedDivide (Interval e) (Interval e2) 
    where
    mixedDivInEff = intervalMixedDivInEff
    mixedDivOutEff = intervalMixedDivOutEff
    
type IntervalMixedDivEffortIndicator e tn = 
    ((NumOrd.PartialCompareEffortIndicator tn, 
      NumOrd.PartialCompareEffortIndicator e), 
     NumOrd.MinmaxEffortIndicator e,
     ArithUpDn.MixedDivEffortIndicator e tn)

intervalMixedDivDefaultEffort :: 
    (NumOrd.PartialComparison t,
     NumOrd.PartialComparison tn,
     NumOrd.RoundedLatticeEffort t,
     ArithUpDn.RoundedMixedDivideEffort t tn) 
    =>
    Interval t
    -> tn
    -> ((NumOrd.PartialCompareEffortIndicator tn,
         NumOrd.PartialCompareEffortIndicator t),
        NumOrd.MinmaxEffortIndicator t,
        ArithUpDn.MixedDivEffortIndicator t tn)
intervalMixedDivDefaultEffort (Interval l _) n = 
    ((NumOrd.pCompareDefaultEffort n, 
      NumOrd.pCompareDefaultEffort l), 
     NumOrd.minmaxDefaultEffort l,
     ArithUpDn.mixedDivDefaultEffort l n) 

intervalMixedDivInEff, intervalMixedDivOutEff :: 
   (NumOrd.PartialComparison t, NumOrd.PartialComparison tn,
    NumOrd.RoundedLattice t, HasZero t, HasZero tn, HasInfinities t,
    ArithUpDn.RoundedMixedDivide t tn) 
   =>
   ((NumOrd.PartialCompareEffortIndicator tn,
     NumOrd.PartialCompareEffortIndicator t),
    NumOrd.MinmaxEffortIndicator t,
    ArithUpDn.MixedDivEffortIndicator t tn)
   -> Interval t -> tn -> Interval t
intervalMixedDivInEff ((effortCompS, effortCompE), effortMinmax, effortDiv) i@(Interval l _) n =
    fromEndpoints $
    multiplySingletonWithInterval 
        (pNonnegNonposEff effortCompS)
        (pNonnegNonposEff effortCompE)
        (flip $ ArithUpDn.mixedDivUpEff effortDiv)
        (flip $ ArithUpDn.mixedDivDnEff effortDiv) 
        (NumOrd.maxUpEff effortMinmax) 
        (NumOrd.minDnEff effortMinmax)
        (plusInfinity l) (minusInfinity l)
        n i
intervalMixedDivOutEff ((effortCompS, effortCompE), effortMinmax, effortDiv) i@(Interval l _) n =
    fromEndpoints $
    multiplySingletonWithInterval 
        (pNonnegNonposEff effortCompS)
        (pNonnegNonposEff effortCompE)
        (flip $ ArithUpDn.mixedDivDnEff effortDiv) 
        (flip $ ArithUpDn.mixedDivUpEff effortDiv)
        (NumOrd.minDnEff effortMinmax)
        (NumOrd.maxUpEff effortMinmax) 
        (minusInfinity l) (plusInfinity l)
        n i
        

{---- mixed ring ----}
        
instance (RoundedMixedAddEffort (Interval e) Integer,
          RoundedMixedMultiplyEffort (Interval e) Integer, 
          NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e,
          NumOrd.PartialComparison Integer,
          ArithUpDn.RoundedMixedRingEffort e Integer) => 
        RoundedMixedRingEffort (Interval e) Integer
    where
    type MixedRingOpsEffortIndicator (Interval e) Integer =
        (ArithUpDn.MixedRingOpsEffortIndicator e Integer,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator Integer))
    mixedRingOpsDefaultEffort (Interval l _) n =
        (ArithUpDn.mixedRingOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxringEffortAdd (Interval l _) n (effortRing, _) = 
        ArithUpDn.mxringEffortAdd l n effortRing
    mxringEffortMult (Interval l _) n (effortRing, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxringEffortMult l n effortRing) 

instance 
    (RoundedMixedAdd (Interval e) Integer,
     RoundedMixedMultiply (Interval e) Integer,
     RoundedMixedRingEffort (Interval e) Integer) 
    => 
    RoundedMixedRing (Interval e) Integer
    
instance (RoundedMixedAddEffort (Interval e) Int,
          RoundedMixedMultiplyEffort (Interval e) Int, 
          NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e,
          NumOrd.PartialComparison Int,
          ArithUpDn.RoundedMixedRingEffort e Int) => 
        RoundedMixedRingEffort (Interval e) Int
    where
    type MixedRingOpsEffortIndicator (Interval e) Int =
        (ArithUpDn.MixedRingOpsEffortIndicator e Int,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator Int))
    mixedRingOpsDefaultEffort (Interval l _) n =
        (ArithUpDn.mixedRingOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxringEffortAdd (Interval l _) n (effortRing, _) = 
        ArithUpDn.mxringEffortAdd l n effortRing
    mxringEffortMult (Interval l _) n (effortRing, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxringEffortMult l n effortRing) 

instance 
    (RoundedMixedAdd (Interval e) Int,
     RoundedMixedMultiply (Interval e) Int,
     RoundedMixedRingEffort (Interval e) Int) 
    => 
    RoundedMixedRing (Interval e) Int
    
instance (RoundedMixedAddEffort (Interval e) Rational,
          RoundedMixedMultiplyEffort (Interval e) Rational, 
          NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e,
          NumOrd.PartialComparison Rational,
          ArithUpDn.RoundedMixedRingEffort e Rational) => 
        RoundedMixedRingEffort (Interval e) Rational
    where
    type MixedRingOpsEffortIndicator (Interval e) Rational =
        (ArithUpDn.MixedRingOpsEffortIndicator e Rational,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator Rational))
    mixedRingOpsDefaultEffort (Interval l _) n =
        (ArithUpDn.mixedRingOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxringEffortAdd (Interval l _) n (effortRing, _) = 
        ArithUpDn.mxringEffortAdd l n effortRing
    mxringEffortMult (Interval l _) n (effortRing, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxringEffortMult l n effortRing) 

instance 
    (RoundedMixedAdd (Interval e) Rational,
     RoundedMixedMultiply (Interval e) Rational,
     RoundedMixedRingEffort (Interval e) Rational) 
    => 
    RoundedMixedRing (Interval e) Rational
    
instance (RoundedMixedAddEffort (Interval e) Double,
          RoundedMixedMultiplyEffort (Interval e) Double, 
          NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e,
          NumOrd.PartialComparison Double,
          ArithUpDn.RoundedMixedRingEffort e Double) => 
        RoundedMixedRingEffort (Interval e) Double
    where
    type MixedRingOpsEffortIndicator (Interval e) Double =
        (ArithUpDn.MixedRingOpsEffortIndicator e Double,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator Double))
    mixedRingOpsDefaultEffort (Interval l _) n =
        (ArithUpDn.mixedRingOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxringEffortAdd (Interval l _) n (effortRing, _) = 
        ArithUpDn.mxringEffortAdd l n effortRing
    mxringEffortMult (Interval l _) n (effortRing, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxringEffortMult l n effortRing) 

instance 
    (RoundedMixedAdd (Interval e) Double,
     RoundedMixedMultiply (Interval e) Double,
     RoundedMixedRingEffort (Interval e) Double) 
    => 
    RoundedMixedRing (Interval e) Double
    
instance (RoundedMixedAddEffort (Interval e) (Interval e2),
          RoundedMixedMultiplyEffort (Interval e) (Interval e2), 
          NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e,
          NumOrd.PartialComparison (Interval e2),
          ArithUpDn.RoundedMixedRingEffort e (Interval e2)) => 
        RoundedMixedRingEffort (Interval e) (Interval e2)
    where
    type MixedRingOpsEffortIndicator (Interval e) (Interval e2) =
        (ArithUpDn.MixedRingOpsEffortIndicator e (Interval e2),
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator (Interval e2)))
    mixedRingOpsDefaultEffort (Interval l _) n =
        (ArithUpDn.mixedRingOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxringEffortAdd (Interval l _) n (effortRing, _) = 
        ArithUpDn.mxringEffortAdd l n effortRing
    mxringEffortMult (Interval l _) n (effortRing, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxringEffortMult l n effortRing) 

instance 
    (RoundedMixedAdd (Interval e) (Interval e2),
     RoundedMixedMultiply (Interval e) (Interval e2),
     RoundedMixedRingEffort (Interval e) (Interval e2)) 
    => 
    RoundedMixedRing (Interval e) (Interval e2)
    
{---- mixed field ----}

instance 
    (Convertible Integer  (Interval e),
     ArithUpDn.RoundedMultiplyEffort e, 
     ArithUpDn.RoundedDivideEffort e,
     RoundedMixedRingEffort (Interval e) Integer ,
     RoundedMixedDivideEffort (Interval e) Integer ,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e,
     NumOrd.PartialComparison Integer ,
     ArithUpDn.RoundedMixedFieldEffort e Integer ) 
    => 
    RoundedMixedFieldEffort (Interval e) Integer 
    where
    type MixedFieldOpsEffortIndicator (Interval e) Integer  =
        (ArithUpDn.MixedFieldOpsEffortIndicator e Integer ,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator Integer ))
    mixedFieldOpsDefaultEffort (Interval l _) n =
        (ArithUpDn.mixedFieldOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxfldEffortAdd (Interval l _) n (effortFld, _) = 
        ArithUpDn.mxfldEffortAdd l n effortFld
    mxfldEffortMult (Interval l _) n (effortFld, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortMult l n effortFld) 
    mxfldEffortDiv (Interval l _) n (effortFld, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortDiv l n effortFld) 
    
instance (RoundedMixedRing (Interval e) Integer ,
          RoundedMixedDivide (Interval e) Integer ,
          RoundedMixedFieldEffort (Interval e) Integer ) => 
        RoundedMixedField (Interval e) Integer 

instance 
    (Convertible Int (Interval e),
     ArithUpDn.RoundedMultiplyEffort e, 
     ArithUpDn.RoundedDivideEffort e,
     RoundedMixedRingEffort (Interval e) Int,
     RoundedMixedDivideEffort (Interval e) Int,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e,
     NumOrd.PartialComparison Int,
     ArithUpDn.RoundedMixedFieldEffort e Int) 
    => 
    RoundedMixedFieldEffort (Interval e) Int
    where
    type MixedFieldOpsEffortIndicator (Interval e) Int =
        (ArithUpDn.MixedFieldOpsEffortIndicator e Int,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator Int))
    mixedFieldOpsDefaultEffort (Interval l _) n =
        (ArithUpDn.mixedFieldOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxfldEffortAdd (Interval l _) n (effortFld, _) = 
        ArithUpDn.mxfldEffortAdd l n effortFld
    mxfldEffortMult (Interval l _) n (effortFld, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortMult l n effortFld) 
    mxfldEffortDiv (Interval l _) n (effortFld, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortDiv l n effortFld) 
    
instance (RoundedMixedRing (Interval e) Int,
          RoundedMixedDivide (Interval e) Int,
          RoundedMixedFieldEffort (Interval e) Int) => 
        RoundedMixedField (Interval e) Int

instance 
    (Convertible Rational (Interval e),
     ArithUpDn.RoundedMultiplyEffort e, 
     ArithUpDn.RoundedDivideEffort e,
     RoundedMixedRingEffort (Interval e) Rational,
     RoundedMixedDivideEffort (Interval e) Rational,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e,
     NumOrd.PartialComparison Rational,
     ArithUpDn.RoundedMixedFieldEffort e Rational) 
    => 
    RoundedMixedFieldEffort (Interval e) Rational
    where
    type MixedFieldOpsEffortIndicator (Interval e) Rational =
        (ArithUpDn.MixedFieldOpsEffortIndicator e Rational,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator Rational))
    mixedFieldOpsDefaultEffort (Interval l _) n =
        (ArithUpDn.mixedFieldOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxfldEffortAdd (Interval l _) n (effortFld, _) = 
        ArithUpDn.mxfldEffortAdd l n effortFld
    mxfldEffortMult (Interval l _) n (effortFld, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortMult l n effortFld) 
    mxfldEffortDiv (Interval l _) n (effortFld, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortDiv l n effortFld) 
    
instance (RoundedMixedRing (Interval e) Rational,
          RoundedMixedDivide (Interval e) Rational,
          RoundedMixedFieldEffort (Interval e) Rational) => 
        RoundedMixedField (Interval e) Rational
 
instance 
    (Convertible Double (Interval e),
     ArithUpDn.RoundedMultiplyEffort e, 
     ArithUpDn.RoundedDivideEffort e,
     RoundedMixedRingEffort (Interval e) Double,
     RoundedMixedDivideEffort (Interval e) Double,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e,
     NumOrd.PartialComparison Double,
     ArithUpDn.RoundedMixedFieldEffort e Double) 
    => 
    RoundedMixedFieldEffort (Interval e) Double
    where
    type MixedFieldOpsEffortIndicator (Interval e) Double =
        (ArithUpDn.MixedFieldOpsEffortIndicator e Double,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator Double))
    mixedFieldOpsDefaultEffort (Interval l _) n =
        (ArithUpDn.mixedFieldOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxfldEffortAdd (Interval l _) n (effortFld, _) = 
        ArithUpDn.mxfldEffortAdd l n effortFld
    mxfldEffortMult (Interval l _) n (effortFld, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortMult l n effortFld) 
    mxfldEffortDiv (Interval l _) n (effortFld, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortDiv l n effortFld) 
    
instance (RoundedMixedRing (Interval e) Double,
          RoundedMixedDivide (Interval e) Double,
          RoundedMixedFieldEffort (Interval e) Double) => 
        RoundedMixedField (Interval e) Double


instance 
    (Convertible (Interval e2) (Interval e),
     ArithUpDn.RoundedMultiplyEffort e, 
     ArithUpDn.RoundedDivideEffort e,
     RoundedMixedRingEffort (Interval e) (Interval e2),
     RoundedMixedDivideEffort (Interval e) (Interval e2),
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e,
     NumOrd.PartialComparison (Interval e2),
     ArithUpDn.RoundedMixedFieldEffort e (Interval e2)) 
    => 
    RoundedMixedFieldEffort (Interval e) (Interval e2)
    where
    type MixedFieldOpsEffortIndicator (Interval e) (Interval e2) =
        (ArithUpDn.MixedFieldOpsEffortIndicator e (Interval e2),
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator (Interval e2)))
    mixedFieldOpsDefaultEffort (Interval l _) n =
        (ArithUpDn.mixedFieldOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxfldEffortAdd (Interval l _) n (effortFld, _) = 
        ArithUpDn.mxfldEffortAdd l n effortFld
    mxfldEffortMult (Interval l _) n (effortFld, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortMult l n effortFld) 
    mxfldEffortDiv (Interval l _) n (effortFld, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortDiv l n effortFld) 
    
instance (RoundedMixedRing (Interval e) (Interval e2),
          RoundedMixedDivide (Interval e) (Interval e2),
          RoundedMixedFieldEffort (Interval e) (Interval e2)) => 
        RoundedMixedField (Interval e) (Interval e2)


{- to allow the above instance to be applied when tn = Interval e,
   ie to allow "mixed" addition of Interval and Interval, 
   we define the following weird mixed operations: -}
   
instance (ArithUpDn.RoundedAddEffort e) => 
    ArithUpDn.RoundedMixedAddEffort e (Interval e)
    where
    type MixedAddEffortIndicator e (Interval e) = 
        ArithUpDn.AddEffortIndicator e
    mixedAddDefaultEffort _e (Interval l _r) = 
        ArithUpDn.addDefaultEffort l
   
instance (ArithUpDn.RoundedAdd e) => 
    ArithUpDn.RoundedMixedAdd e (Interval e) 
    where
    mixedAddUpEff effort e i =
        case addOutEff effort (Interval e e) i of
            Interval _l r -> r  
    mixedAddDnEff effort e i =
        case addOutEff effort (Interval e e) i of
            Interval l _r -> l  

instance 
    (ArithUpDn.RoundedMultiplyEffort e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e
    ) 
    => 
    ArithUpDn.RoundedMixedMultiplyEffort e (Interval e) 
    where
    type MixedMultEffortIndicator e (Interval e) = 
        MultEffortIndicator (Interval e)
    mixedMultDefaultEffort _e i = 
        multDefaultEffort i
   
instance 
    (ArithUpDn.RoundedMultiply e,
     HasZero e, Neg e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e
    )
    =>
    ArithUpDn.RoundedMixedMultiply e (Interval e) 
    where
    mixedMultUpEff effort e i =
        case multOutEff effort (Interval e e) i of
            Interval _l r -> r  
    mixedMultDnEff effort e i =
        case multOutEff effort (Interval e e) i of
            Interval l _r -> l  

instance 
    (ArithUpDn.RoundedMultiplyEffort e,
     ArithUpDn.RoundedDivideEffort e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e
    ) 
    => 
    ArithUpDn.RoundedMixedDivideEffort e (Interval e) 
    where
    type MixedDivEffortIndicator e (Interval e) = 
        DivEffortIndicator (Interval e)
    mixedDivDefaultEffort _e i = 
        divDefaultEffort i
   
instance 
    (ArithUpDn.RoundedMultiply e,
     ArithUpDn.RoundedDivide e,
     NumOrd.HasExtrema e,
     HasZero e, Neg e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e
    )
    =>
    ArithUpDn.RoundedMixedDivide e (Interval e) 
    where
    mixedDivUpEff effort e i =
        case divOutEff effort (Interval e e) i of
            Interval _l r -> r  
    mixedDivDnEff effort e i =
        case divOutEff effort (Interval e e) i of
            Interval l _r -> l  

instance
    (ArithUpDn.RoundedRingEffort e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e
    )
    =>
    ArithUpDn.RoundedMixedRingEffort e (Interval e)
    where
    type MixedRingOpsEffortIndicator e (Interval e) =
        (RingOpsEffortIndicator (Interval e))
    mixedRingOpsDefaultEffort _ sampleI = ringOpsDefaultEffort sampleI
    mxringEffortAdd _ sampleI eff = ringEffortAdd sampleI eff
    mxringEffortMult _ sampleI eff = ringEffortMult sampleI eff

instance
    (ArithUpDn.RoundedRing e,
     NumOrd.HasExtrema e,
     HasZero e, Neg e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e
    )
    =>
    ArithUpDn.RoundedMixedRing e (Interval e)

instance
    (ArithUpDn.RoundedFieldEffort e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e
    )
    =>
    ArithUpDn.RoundedMixedFieldEffort e (Interval e)
    where
    type MixedFieldOpsEffortIndicator e (Interval e) =
        (FieldOpsEffortIndicator (Interval e))
    mixedFieldOpsDefaultEffort _ sampleI = fieldOpsDefaultEffort sampleI
    mxfldEffortAdd _ sampleI eff = fldEffortAdd sampleI eff
    mxfldEffortMult _ sampleI eff = fldEffortMult sampleI eff
    mxfldEffortDiv _ sampleI eff = fldEffortDiv sampleI eff

instance
    (ArithUpDn.RoundedField e,
     NumOrd.HasExtrema e,
     HasZero e, Neg e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e
    )
    =>
    ArithUpDn.RoundedMixedField e (Interval e)

{- end of weird mixed operations -}   
