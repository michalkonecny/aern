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
import Numeric.AERN.Basics.Interval.Mutable

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
-- import Numeric.AERN.RealArithmetic.Interval.FieldOps
import Numeric.AERN.RealArithmetic.Interval.MixedFieldOps

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

import Control.Monad.ST (ST)


instance (ArithUpDn.RoundedMixedAddInPlace t tn, CanBeMutable t) => 
    RoundedMixedAddInPlace (Interval t) tn
    where
    mixedAddInInPlaceEff eff (MInterval resL resR) (MInterval aL aR) n =
        do
        ArithUpDn.mixedAddUpInPlaceEff eff resL aL n
        ArithUpDn.mixedAddDnInPlaceEff eff resR aR n
    mixedAddOutInPlaceEff eff (MInterval resL resR) (MInterval aL aR) n =
        do
        ArithUpDn.mixedAddDnInPlaceEff eff resL aL n
        ArithUpDn.mixedAddUpInPlaceEff eff resR aR n

instance    
    (ArithUpDn.RoundedMixedMultiplyInPlace e tn,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  NumOrd.PartialComparison e,  
     HasZero tn,  NumOrd.PartialComparison tn,  
     CanBeMutable e) => 
    RoundedMixedMultiplyInPlace (Interval e) tn
    where
    mixedMultInInPlaceEff  
            ((effortCompS,effortCompE), effortMinmax, effortMult)
            r i1 s =
        multiplySingletonAndIntervalInPlace
            (pNonnegNonposEff effortCompS)
            (pNonnegNonposEff effortCompE)
            (ArithUpDn.mixedMultUpInPlaceEff effortMult) 
            (ArithUpDn.mixedMultDnInPlaceEff effortMult)
            (NumOrd.maxUpInPlaceEff effortMinmax)
            (NumOrd.minDnInPlaceEff effortMinmax)
            r s i1
    mixedMultOutInPlaceEff 
            ((effortCompS,effortCompE), effortMinmax, effortMult)
            r i1 s =
        multiplySingletonAndIntervalInPlace
            (pNonnegNonposEff effortCompS)
            (pNonnegNonposEff effortCompE)
            (ArithUpDn.mixedMultDnInPlaceEff effortMult) 
            (ArithUpDn.mixedMultUpInPlaceEff effortMult)
            (NumOrd.minDnInPlaceEff effortMinmax)
            (NumOrd.maxUpInPlaceEff effortMinmax)
            r s i1

multiplySingletonAndIntervalInPlace ::
    (CanBeMutable e, HasZero e) =>
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
multiplySingletonAndIntervalInPlace
        sNonnegNonpos iNonnegNonpos 
        timesLInPlace timesRInPlace
        combineLInPlace combineRInPlace
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
                let z = zero l2
                writeMutable lResM z
                writeMutable rResM z
 
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
                assignResEndpointsUsingBothOptions
                return ()
                
            -- nothing known about s1, i2 negative
            (_, (_, Just True), (_, Just True)) -> 
--                ((s1 `timesL` r2) `combineL` (s1 `timesL` l2), 
--                 (s1 `timesR` r2) `combineR` (s1 `timesR` l2))
                do
                assignResEndpointsUsingBothOptions
                return ()


            -- both s1 and i2 are around zero
            _ ->
--                ((s1 `timesL` l2) `combineL` (s1 `timesL` r2) `combineL` zero,
--                 (s1 `timesR` l2) `combineR` (s1 `timesR` r2) `combineR` zero)
--                -- need to include zero to account for 
--                -- consistent vs anti-consistent cases giving constant 0
                do
                temp1 <- assignResEndpointsUsingBothOptions
                let z = zero l2
                writeMutable temp1 z
                combineLInPlace lResM lResM temp1
                combineRInPlace rResM rResM temp1
    where
    assignResEndpointsUsingTimesLR l2M r2M =
        do
        temp1 <- cloneMutable l2M
        timesLInPlace temp1 l2M s1 -- beware of aliasing between res and param
        timesRInPlace rResM r2M s1
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
    