{-# LANGUAGE FlexibleContexts, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Mutable.MixedFieldOps
    Description :  mixed field operations for mutable intervals 
    Copyright   :  (c) Michal Konecny
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
import Numeric.AERN.Basics.Interval.Mutable

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
-- import Numeric.AERN.RealArithmetic.Interval.FieldOps
import Numeric.AERN.RealArithmetic.Interval.MixedFieldOps

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Control.Monad.ST (ST)


instance (ArithUpDn.RoundedMixedAddInPlace t tn, CanBeMutable t) => 
    RoundedMixedAddInPlace (Interval t) tn
    where
    mixedAddInInPlaceEff (Interval sample _) eff (MInterval resL resH) (MInterval aL aH) n =
        do
        ArithUpDn.mixedAddUpInPlaceEff sample eff resL aL n
        ArithUpDn.mixedAddDnInPlaceEff sample eff resH aH n
    mixedAddOutInPlaceEff (Interval sample _) eff (MInterval resL resH) (MInterval aL aH) n =
        do
        ArithUpDn.mixedAddDnInPlaceEff sample eff resL aL n
        ArithUpDn.mixedAddUpInPlaceEff sample eff resH aH n

instance    
    (ArithUpDn.RoundedMixedMultiplyInPlace e tn,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  NumOrd.PartialComparison e,  
     HasZero tn,  NumOrd.PartialComparison tn,  
     CanBeMutable e) => 
    RoundedMixedMultiplyInPlace (Interval e) tn
    where
    mixedMultInInPlaceEff 
            (Interval sample _) 
            ((effortCompS,effortCompE), effortMinmax, effortMult)
            r i1 s =
        multiplySingletonAndIntervalInPlace sample
            (pNonnegNonposEff effortCompS)
            (pNonnegNonposEff effortCompE)
            (ArithUpDn.mixedMultUpInPlaceEff sample effortMult) 
            (ArithUpDn.mixedMultDnInPlaceEff sample effortMult)
            (NumOrd.maxUpInPlaceEff sample effortMinmax)
            (NumOrd.minDnInPlaceEff sample effortMinmax)
            r s i1
    mixedMultOutInPlaceEff 
            (Interval sample _) 
            ((effortCompS,effortCompE), effortMinmax, effortMult)
            r i1 s =
        multiplySingletonAndIntervalInPlace sample
            (pNonnegNonposEff effortCompS)
            (pNonnegNonposEff effortCompE)
            (ArithUpDn.mixedMultDnInPlaceEff sample effortMult) 
            (ArithUpDn.mixedMultUpInPlaceEff sample effortMult)
            (NumOrd.minDnInPlaceEff sample effortMinmax)
            (NumOrd.maxUpInPlaceEff sample effortMinmax)
            r s i1

multiplySingletonAndIntervalInPlace ::
    (CanBeMutable e, HasZero e) =>
    e ->
    (tn -> (Maybe Bool, Maybe Bool)) ->
    (e -> (Maybe Bool, Maybe Bool)) ->
    (OpMutableNonmut e tn s) ->
    (OpMutableNonmut e tn s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (Mutable (Interval e) s) ->
    tn ->
    (Mutable (Interval e) s) ->
    ST s ()
multiplySingletonAndIntervalInPlace sample
        sNonnegNonpos iNonnegNonpos 
        timesLInPlace timesRInPlace
        combineLInPlace combineRInPlace
        (MInterval lResM hResM) s1 (MInterval l2M h2M) =
    do
    let _ = [combineLInPlace, combineRInPlace]
    l2 <- readMutable l2M
    h2 <- readMutable h2M
    let _ = [l2,h2,sample]
    case (sNonnegNonpos s1, -- sign of s1 
              iNonnegNonpos l2, -- sign of l2
              iNonnegNonpos h2 -- sign of h2 
             ) of
             
            -- s1 is zero
            ((Just True, Just True), _, _) -> 
--                (zero, zero)
                do
                let z = zero
                let _ = [z,sample]
                writeMutable lResM z
                writeMutable hResM z
 
            -- s1 non negative
            ((Just True, _), _, _) -> 
--                (s1 `timesL` l2, s1 `timesR` h2)
                do
                assignResEndpointsUsingTimesLR l2M h2M
            
            -- s1 non positive
            ((_, Just True), _, _) -> 
--                (s1 `timesL` h2, s1 `timesR` l2)
                do
                assignResEndpointsUsingTimesLR h2M l2M

            -- nothing known about s1, i2 positive
            (_, (Just True, _), (Just True, _)) -> 
--                ((s1 `timesL` h2) `combineL` (s1 `timesL` l2), 
--                 (s1 `timesR` h2) `combineR` (s1 `timesR` l2))
                do
                assignResEndpointsUsingBothOptions
                return ()
                
            -- nothing known about s1, i2 negative
            (_, (_, Just True), (_, Just True)) -> 
--                ((s1 `timesL` h2) `combineL` (s1 `timesL` l2), 
--                 (s1 `timesR` h2) `combineR` (s1 `timesR` l2))
                do
                assignResEndpointsUsingBothOptions
                return ()


            -- both s1 and i2 are around zero
            _ ->
--                ((s1 `timesL` l2) `combineL` (s1 `timesL` h2) `combineL` zero,
--                 (s1 `timesR` l2) `combineR` (s1 `timesR` h2) `combineR` zero)
--                -- need to include zero to account for 
--                -- consistent vs anti-consistent cases giving constant 0
                do
                temp1 <- assignResEndpointsUsingBothOptions
                let z = zero
                let _ = [z,sample]
                writeMutable temp1 z
                combineLInPlace lResM lResM temp1
                combineRInPlace hResM hResM temp1
    where
    assignResEndpointsUsingTimesLR l2M h2M =
        do
        temp1 <- makeMutable sample
        timesLInPlace temp1 l2M s1 -- beware of aliasing between res and param
        timesRInPlace hResM h2M s1
        assignMutable sample lResM temp1
    assignResEndpointsUsingBothOptions =
        do
        temp1 <- makeMutable sample
        temp2 <- makeMutable sample
        temp3 <- makeMutable sample
        timesLInPlace temp1 h2M s1 
        timesLInPlace temp2 l2M s1 
        combineLInPlace temp3 temp1 temp2
        timesRInPlace temp1 h2M s1 
        timesRInPlace temp2 l2M s1 
        combineRInPlace hResM temp1 temp2
        assignMutable sample lResM temp3
        return temp1
    
instance (RoundedDivideInPlace (Interval e),
          Convertible tn (Interval e)) => 
    RoundedMixedDivideInPlace (Interval e) tn 
    where
    mixedDivInInPlaceEff = mixedDivInInPlaceEffByConversion
    mixedDivOutInPlaceEff = mixedDivOutInPlaceEffByConversion

instance 
    (ArithUpDn.RoundedMixedRingInPlace e tn,
     NumOrd.PartialComparison tn,
     HasZero tn,
     HasZero e, 
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedMixedRingInPlace (Interval e) tn

instance 
    (ArithUpDn.RoundedMixedFieldInPlace e tn,
     RoundedDivideInPlace (Interval e),
     Convertible tn (Interval e),
     NumOrd.PartialComparison tn,
     HasZero tn,
     HasZero e, 
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedMixedFieldInPlace (Interval e) tn
    