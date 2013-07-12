{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Mutable.MixedFieldOps
    Description :  mixed field operations for mutable intervals 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Mixed field operations for mutable intervals. 
    
    This module is hidden and reexported via its parent Interval.Mutable. 
-}

module Numeric.AERN.RealArithmetic.Interval.Mutable.MixedFieldOps() where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Interval
--import Numeric.AERN.Basics.Interval.Mutable 

import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
-- import Numeric.AERN.RealArithmetic.Interval.FieldOps
import Numeric.AERN.RealArithmetic.Interval.MixedFieldOps ()

import qualified Numeric.AERN.NumericOrder as NumOrd
--import qualified Numeric.AERN.RefinementOrder as RefOrd

import Control.Monad.ST (ST)

{---- mixed in-place addition ----}

instance (ArithUpDn.RoundedMixedAddInPlace t Integer, CanBeMutable t) => 
    RoundedMixedAddInPlace (Interval t) Integer
    where
    mixedAddInInPlaceEff = mixedAddInInPlaceEffGeneric
    mixedAddOutInPlaceEff = mixedAddOutInPlaceEffGeneric

instance (ArithUpDn.RoundedMixedAddInPlace t Int, CanBeMutable t) => 
    RoundedMixedAddInPlace (Interval t) Int
    where
    mixedAddInInPlaceEff = mixedAddInInPlaceEffGeneric
    mixedAddOutInPlaceEff = mixedAddOutInPlaceEffGeneric

instance (ArithUpDn.RoundedMixedAddInPlace t Rational, CanBeMutable t) => 
    RoundedMixedAddInPlace (Interval t) Rational
    where
    mixedAddInInPlaceEff = mixedAddInInPlaceEffGeneric
    mixedAddOutInPlaceEff = mixedAddOutInPlaceEffGeneric

instance (ArithUpDn.RoundedMixedAddInPlace t Double, CanBeMutable t) => 
    RoundedMixedAddInPlace (Interval t) Double
    where
    mixedAddInInPlaceEff = mixedAddInInPlaceEffGeneric
    mixedAddOutInPlaceEff = mixedAddOutInPlaceEffGeneric

mixedAddInInPlaceEffGeneric, mixedAddOutInPlaceEffGeneric :: 
  ArithUpDn.RoundedMixedAddInPlace t tn 
  =>
  ArithUpDn.MixedAddEffortIndicator t tn
  -> Mutable (Interval t) s
  -> Mutable (Interval t) s
  -> tn
  -> ST s ()
mixedAddInInPlaceEffGeneric eff (MInterval resL resR) (MInterval aL aR) n =
    do
    ArithUpDn.mixedAddUpInPlaceEff eff resL aL n
    ArithUpDn.mixedAddDnInPlaceEff eff resR aR n
mixedAddOutInPlaceEffGeneric eff (MInterval resL resR) (MInterval aL aR) n =
    do
    ArithUpDn.mixedAddDnInPlaceEff eff resL aL n
    ArithUpDn.mixedAddUpInPlaceEff eff resR aR n

{---- mixed in-place multiplication ----}

instance
    (ArithUpDn.RoundedMixedMultiplyInPlace e Integer,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  NumOrd.PartialComparison e,  
     HasZero Integer,  NumOrd.PartialComparison Integer,  
     CanBeMutable e) => 
    RoundedMixedMultiplyInPlace (Interval e) Integer
    where
    mixedMultInInPlaceEff = mixedMultInInPlaceEffGeneric
    mixedMultOutInPlaceEff = mixedMultOutInPlaceEffGeneric
    
    
instance
    (ArithUpDn.RoundedMixedMultiplyInPlace e Int,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  NumOrd.PartialComparison e,  
     HasZero Int,  NumOrd.PartialComparison Int,  
     CanBeMutable e) => 
    RoundedMixedMultiplyInPlace (Interval e) Int
    where
    mixedMultInInPlaceEff = mixedMultInInPlaceEffGeneric
    mixedMultOutInPlaceEff = mixedMultOutInPlaceEffGeneric
    
instance
    (ArithUpDn.RoundedMixedMultiplyInPlace e Rational,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  NumOrd.PartialComparison e,  
     HasZero Rational,  NumOrd.PartialComparison Rational,  
     CanBeMutable e) => 
    RoundedMixedMultiplyInPlace (Interval e) Rational
    where
    mixedMultInInPlaceEff = mixedMultInInPlaceEffGeneric
    mixedMultOutInPlaceEff = mixedMultOutInPlaceEffGeneric
    
instance
    (ArithUpDn.RoundedMixedMultiplyInPlace e Double,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  NumOrd.PartialComparison e,  
     HasZero Double,  NumOrd.PartialComparison Double,  
     CanBeMutable e) => 
    RoundedMixedMultiplyInPlace (Interval e) Double
    where
    mixedMultInInPlaceEff = mixedMultInInPlaceEffGeneric
    mixedMultOutInPlaceEff = mixedMultOutInPlaceEffGeneric

mixedMultInInPlaceEffGeneric, mixedMultOutInPlaceEffGeneric :: 
   (NumOrd.PartialComparison e, NumOrd.PartialComparison tn,
    NumOrd.RoundedLatticeInPlace e, HasZero tn, HasZero e,
    ArithUpDn.RoundedMixedMultiplyInPlace e tn) 
   =>
   ((NumOrd.PartialCompareEffortIndicator tn,
     NumOrd.PartialCompareEffortIndicator e),
    NumOrd.MinmaxEffortIndicator e,
    ArithUpDn.MixedMultEffortIndicator e tn)
   -> Mutable (Interval e) s
   -> Mutable (Interval e) s
   -> tn
   -> ST s ()
mixedMultInInPlaceEffGeneric
        ((effortCompS,effortCompE), effortMinmax, effortMult)
        r i1 s =
    do
    (Interval l _) <- readMutable i1
    multiplySingletonAndIntervalInPlace
        (pNonnegNonposEff effortCompS)
        (pNonnegNonposEff effortCompE)
        (ArithUpDn.mixedMultUpInPlaceEff effortMult) 
        (ArithUpDn.mixedMultDnInPlaceEff effortMult)
        (NumOrd.maxUpInPlaceEff effortMinmax)
        (NumOrd.minDnInPlaceEff effortMinmax)
        (zero l) (zero l)
        r s i1
mixedMultOutInPlaceEffGeneric
        ((effortCompS,effortCompE), effortMinmax, effortMult)
        r i1 s =
    do
    (Interval l _) <- readMutable i1
    multiplySingletonAndIntervalInPlace
        (pNonnegNonposEff effortCompS)
        (pNonnegNonposEff effortCompE)
        (ArithUpDn.mixedMultDnInPlaceEff effortMult) 
        (ArithUpDn.mixedMultUpInPlaceEff effortMult)
        (NumOrd.minDnInPlaceEff effortMinmax)
        (NumOrd.maxUpInPlaceEff effortMinmax)
        (zero l) (zero l)
        r s i1

multiplySingletonAndIntervalInPlace ::
    (CanBeMutable e, HasZero e) =>
    (tn -> (Maybe Bool, Maybe Bool)) ->
    (e -> (Maybe Bool, Maybe Bool)) ->
    (OpMutableNonmut e tn s) ->
    (OpMutableNonmut e tn s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    e ->
    e ->
    (Mutable (Interval e) s) ->
    tn ->
    (Mutable (Interval e) s) ->
    ST s ()
multiplySingletonAndIntervalInPlace
        sNonnegNonpos iNonnegNonpos 
        timesLInPlace timesRInPlace
        combineLInPlace combineRInPlace
        zeroResL zeroResR
        (MInterval lResM rResM) s1 (MInterval l2M r2M) =
    do
    let _ = [combineLInPlace, combineRInPlace]
    l2 <- readMutable l2M
    r2 <- readMutable r2M
    let _ = [l2,r2]
    case (sNonnegNonpos s1, -- sign of s1 
              iNonnegNonpos l2, -- sign of l2
              iNonnegNonpos r2 -- sign of r2 
             ) of
             
            -- s1 is zero
            ((Just True, Just True), _, _) -> 
--                (zero, zero)
                do
                writeMutable lResM zeroResL
                writeMutable rResM zeroResR
 
            -- s1 non negative
            ((Just True, _), _, _) -> 
--                (s1 `timesL` l2, s1 `timesR` r2)
                do
                assignResEndpointsUsingTimesLR l2M r2M
            
            -- s1 non positive
            ((_, Just True), _, _) -> 
--                (s1 `timesL` r2, s1 `timesR` l2)
                do
                assignResEndpointsUsingTimesLR r2M l2M

            -- nothing known about s1, i2 positive
            (_, (Just True, _), (Just True, _)) -> 
--                ((s1 `timesL` r2) `combineL` (s1 `timesL` l2), 
--                 (s1 `timesR` r2) `combineR` (s1 `timesR` l2))
                do
                _ <- assignResEndpointsUsingBothOptions
                return ()
                
            -- nothing known about s1, i2 negative
            (_, (_, Just True), (_, Just True)) -> 
--                ((s1 `timesL` r2) `combineL` (s1 `timesL` l2), 
--                 (s1 `timesR` r2) `combineR` (s1 `timesR` l2))
                do
                _ <- assignResEndpointsUsingBothOptions
                return ()


            -- both s1 and i2 are around zero
            _ ->
--                ((s1 `timesL` l2) `combineL` (s1 `timesL` r2) `combineL` zero,
--                 (s1 `timesR` l2) `combineR` (s1 `timesR` r2) `combineR` zero)
--                -- need to include zero to account for 
--                -- consistent vs anti-consistent cases giving constant 0
                do
                temp1 <- assignResEndpointsUsingBothOptions
                writeMutable temp1 zeroResL
                combineLInPlace lResM lResM temp1
                writeMutable temp1 zeroResR
                combineRInPlace rResM rResM temp1
    where
    assignResEndpointsUsingTimesLR lM rM =
        do
        temp1 <- cloneMutable lM
        timesLInPlace temp1 lM s1 -- beware of aliasing between res and param
        timesRInPlace rResM rM s1
        assignMutable lResM temp1
    assignResEndpointsUsingBothOptions =
        do
        temp1 <- cloneMutable r2M
        temp2 <- cloneMutable r2M
        temp3 <- cloneMutable r2M
        timesLInPlace temp1 r2M s1 
        timesLInPlace temp2 l2M s1 
        combineLInPlace temp3 temp1 temp2
        timesRInPlace temp1 r2M s1 
        timesRInPlace temp2 l2M s1 
        combineRInPlace rResM temp1 temp2
        assignMutable lResM temp3
        return temp1
    
{---- mixed in-place division ----}
    
instance    
    (ArithUpDn.RoundedMixedDivideInPlace e Integer,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  HasInfinities e, NumOrd.PartialComparison e,  
     HasZero Integer,  NumOrd.PartialComparison Integer,  
     CanBeMutable e) => 
    RoundedMixedDivideInPlace (Interval e) Integer
    where
    mixedDivInInPlaceEff = mixedDivInInPlaceEffGeneric
    mixedDivOutInPlaceEff = mixedDivOutInPlaceEffGeneric

instance    
    (ArithUpDn.RoundedMixedDivideInPlace e Int,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  HasInfinities e, NumOrd.PartialComparison e,  
     HasZero Int,  NumOrd.PartialComparison Int,  
     CanBeMutable e) => 
    RoundedMixedDivideInPlace (Interval e) Int
    where
    mixedDivInInPlaceEff = mixedDivInInPlaceEffGeneric
    mixedDivOutInPlaceEff = mixedDivOutInPlaceEffGeneric

instance    
    (ArithUpDn.RoundedMixedDivideInPlace e Rational,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  HasInfinities e, NumOrd.PartialComparison e,  
     HasZero Rational,  NumOrd.PartialComparison Rational,  
     CanBeMutable e) => 
    RoundedMixedDivideInPlace (Interval e) Rational
    where
    mixedDivInInPlaceEff = mixedDivInInPlaceEffGeneric
    mixedDivOutInPlaceEff = mixedDivOutInPlaceEffGeneric

instance    
    (ArithUpDn.RoundedMixedDivideInPlace e Double,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  HasInfinities e, NumOrd.PartialComparison e,  
     HasZero Double,  NumOrd.PartialComparison Double,  
     CanBeMutable e) => 
    RoundedMixedDivideInPlace (Interval e) Double
    where
    mixedDivInInPlaceEff = mixedDivInInPlaceEffGeneric
    mixedDivOutInPlaceEff = mixedDivOutInPlaceEffGeneric


mixedDivInInPlaceEffGeneric, mixedDivOutInPlaceEffGeneric :: 
  (NumOrd.PartialComparison e, NumOrd.PartialComparison tn,
   NumOrd.RoundedLatticeInPlace e, HasZero tn, HasZero e,
   HasInfinities e,
   ArithUpDn.RoundedMixedDivideInPlace e tn) 
  =>
  ((NumOrd.PartialCompareEffortIndicator tn,
    NumOrd.PartialCompareEffortIndicator e),
   NumOrd.MinmaxEffortIndicator e,
   ArithUpDn.MixedDivEffortIndicator e tn)
  -> Mutable (Interval e) s
  -> Mutable (Interval e) s
  -> tn
  -> ST s ()
mixedDivInInPlaceEffGeneric
        ((effortCompS,effortCompE), effortMinmax, effortDiv)
        r i1 s =
    do
    (Interval l _) <- readMutable i1
    multiplySingletonAndIntervalInPlace
        (pNonnegNonposEff effortCompS)
        (pNonnegNonposEff effortCompE)
        (ArithUpDn.mixedDivUpInPlaceEff effortDiv) 
        (ArithUpDn.mixedDivDnInPlaceEff effortDiv)
        (NumOrd.maxUpInPlaceEff effortMinmax)
        (NumOrd.minDnInPlaceEff effortMinmax)
        (plusInfinity l) (minusInfinity l)
        r s i1
mixedDivOutInPlaceEffGeneric
        ((effortCompS,effortCompE), effortMinmax, effortDiv)
        r i1 s =
    do
    (Interval l _) <- readMutable i1
    multiplySingletonAndIntervalInPlace
        (pNonnegNonposEff effortCompS)
        (pNonnegNonposEff effortCompE)
        (ArithUpDn.mixedDivDnInPlaceEff effortDiv) 
        (ArithUpDn.mixedDivUpInPlaceEff effortDiv)
        (NumOrd.minDnInPlaceEff effortMinmax)
        (NumOrd.maxUpInPlaceEff effortMinmax)
        (minusInfinity l) (plusInfinity l)
        r s i1


instance 
    (ArithUpDn.RoundedMixedRingInPlace e Integer,
     NumOrd.PartialComparison Integer,
     HasZero Integer,
     HasZero e, 
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedMixedRingInPlace (Interval e) Integer

instance 
    (ArithUpDn.RoundedMixedFieldInPlace e Integer,
     RoundedDivideInPlace (Interval e),
     -- MK has no idea why the following four need to be stated;
     --    they should be inferred from the one above automatically...
     ArithUpDn.RoundedMultiplyEffort e, 
     ArithUpDn.RoundedDivideEffort e,  
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeEffort e,
     --
     Convertible Integer (Interval e),
     NumOrd.PartialComparison Integer,
     HasZero Integer,
     HasZero e, HasInfinities e,
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedMixedFieldInPlace (Interval e) Integer

instance 
    (ArithUpDn.RoundedMixedRingInPlace e Int,
     NumOrd.PartialComparison Int,
     HasZero Int,
     HasZero e, 
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedMixedRingInPlace (Interval e) Int

instance 
    (ArithUpDn.RoundedMixedFieldInPlace e Int,
     RoundedDivideInPlace (Interval e),
     -- MK has no idea why the following four need to be stated;
     --    they should be inferred from the one above automatically...
     ArithUpDn.RoundedMultiplyEffort e, 
     ArithUpDn.RoundedDivideEffort e,  
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeEffort e,
     --
     Convertible Int (Interval e),
     NumOrd.PartialComparison Int,
     HasZero Int,
     HasZero e, HasInfinities e,
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedMixedFieldInPlace (Interval e) Int

instance 
    (ArithUpDn.RoundedMixedRingInPlace e Rational,
     NumOrd.PartialComparison Rational,
     HasZero Rational,
     HasZero e, 
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedMixedRingInPlace (Interval e) Rational

instance 
    (ArithUpDn.RoundedMixedFieldInPlace e Rational,
     RoundedDivideInPlace (Interval e),
     -- MK has no idea why the following four need to be stated;
     --    they should be inferred from the one above automatically...
     ArithUpDn.RoundedMultiplyEffort e, 
     ArithUpDn.RoundedDivideEffort e,  
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeEffort e,
     --
     Convertible Rational (Interval e),
     NumOrd.PartialComparison Rational,
     HasZero Rational,
     HasZero e, HasInfinities e,
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedMixedFieldInPlace (Interval e) Rational

instance 
    (ArithUpDn.RoundedMixedRingInPlace e Double,
     NumOrd.PartialComparison Double,
     HasZero Double,
     HasZero e, 
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedMixedRingInPlace (Interval e) Double

instance 
    (ArithUpDn.RoundedMixedFieldInPlace e Double,
     RoundedDivideInPlace (Interval e),
     -- MK has no idea why the following four need to be stated;
     --    they should be inferred from the one above automatically...
     ArithUpDn.RoundedMultiplyEffort e, 
     ArithUpDn.RoundedDivideEffort e,  
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeEffort e,
     --
     Convertible Double (Interval e),
     NumOrd.PartialComparison Double,
     HasZero Double,
     HasZero e, HasInfinities e,
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedMixedFieldInPlace (Interval e) Double


    