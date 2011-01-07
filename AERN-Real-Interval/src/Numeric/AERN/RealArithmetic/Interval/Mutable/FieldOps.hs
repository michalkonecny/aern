{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Mutable.FieldOps
    Description :  field operations for mutable intervals 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Field operations for mutable intervals. 
    
    This module is hidden and reexported via its parent Interval.Mutable. 
-}

module Numeric.AERN.RealArithmetic.Interval.Mutable.FieldOps() where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Interval

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.Interval.FieldOps

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Control.Monad.ST (ST)


instance (ArithUpDn.RoundedAddInPlace e, CanBeMutable e) => 
    RoundedAddInPlace (Interval e) 
    where
    addInInPlaceEff (Interval sample _) eff (MInterval resL resH) (MInterval aL aH) (MInterval bL bH) =
        do
        ArithUpDn.addUpInPlaceEff sample eff resL aL bL
        ArithUpDn.addDnInPlaceEff sample eff resH aH bH
    addOutInPlaceEff (Interval sample _) eff (MInterval resL resH) (MInterval aL aH) (MInterval bL bH) =
        do
        ArithUpDn.addDnInPlaceEff sample eff resL aL bL
        ArithUpDn.addUpInPlaceEff sample eff resH aH bH
    
instance 
    (ArithUpDn.RoundedAddInPlace e,
     CanBeMutable e,
     NegInPlace e) => 
    RoundedSubtrInPlace (Interval e) 

instance (RoundedAbs (Interval e), CanBeMutable (Interval e)) => 
    RoundedAbsInPlace (Interval e) 

instance 
    (RoundedMultiply (Interval e),
     ArithUpDn.RoundedMultiplyInPlace e,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  NumOrd.PartialComparison e,
     CanBeMutable e) => 
    RoundedMultiplyInPlace (Interval e) 
    where
    multOutInPlaceEff (Interval sample _) (effortComp, effortMinmax, effortMult) r i1 i2 =
        multiplyIntervalsInPlace sample
            (pNonnegNonposEff effortComp)
            (ArithUpDn.multDnInPlaceEff sample effortMult) 
            (ArithUpDn.multUpInPlaceEff sample effortMult)
            (NumOrd.minDnInPlaceEff sample effortMinmax) -- minL
            (NumOrd.minUpInPlaceEff sample effortMinmax) -- minR
            (NumOrd.maxDnInPlaceEff sample effortMinmax) -- maxL
            (NumOrd.maxUpInPlaceEff sample effortMinmax) -- maxR
            (NumOrd.minDnInPlaceEff sample effortMinmax)
            (NumOrd.maxUpInPlaceEff sample effortMinmax) 
            r i1 i2
    multInInPlaceEff (Interval sample _) (effortComp, effortMinmax, effortMult) r i1 i2 =
        multiplyIntervalsInPlace sample
            (pNonnegNonposEff effortComp)
            (ArithUpDn.multUpInPlaceEff sample effortMult) 
            (ArithUpDn.multDnInPlaceEff sample effortMult)
            (NumOrd.minDnInPlaceEff sample effortMinmax) -- minL
            (NumOrd.minUpInPlaceEff sample effortMinmax) -- minR
            (NumOrd.maxDnInPlaceEff sample effortMinmax) -- maxL
            (NumOrd.maxUpInPlaceEff sample effortMinmax) -- maxR
            (NumOrd.maxDnInPlaceEff sample effortMinmax)
            (NumOrd.minUpInPlaceEff sample effortMinmax) 
            r i1 i2
    
multiplyIntervalsInPlace ::
    (CanBeMutable e) =>
    e ->
    (e -> (Maybe Bool, Maybe Bool)) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (MInterval (Mutable e) s) ->
    (MInterval (Mutable e) s) ->
    (MInterval (Mutable e) s) ->
    ST s ()
multiplyIntervalsInPlace
        sample
        pNonnegNonpos timesLInPlace timesRInPlace 
        minLInPlace minRInPlace maxLInPlace maxRInPlace 
        combineLInPlace combineRInPlace
        (MInterval lResM hResM) (MInterval l1M h1M) (MInterval l2M h2M) =
    do
    let _ = [minLInPlace, maxRInPlace, combineLInPlace, combineRInPlace]
    l1 <- readMutable l1M
    h1 <- readMutable h1M
    l2 <- readMutable l2M
    h2 <- readMutable h2M
    let _ = [l1,h1,l2,h2,sample]
    case (pNonnegNonpos l1, -- sign of l1 
              pNonnegNonpos h1, -- sign of h1
              pNonnegNonpos l2, -- sign of l2
              pNonnegNonpos h2 -- sign of h2 
             ) of
             
            -----------------------------------------------------------
            -- cases where i1 or i2 is known to be positive or negative
            -----------------------------------------------------------
            -- i1 negative, i2 positive
            ((_, Just True), (_, Just True), (Just True, _), (Just True, _)) ->
                do
                timesLInPlace lResM l1M h2M 
                timesRInPlace hResM h1M l2M 
--                (l1 `timesL` h2, h1 `timesR` l2)
--            -- i1 negative, i2 negative
--            ((_, Just True), (_, Just True), (_, Just True), (_, Just True)) -> 
--                (h1 `timesL` h2, l1 `timesR` l2)
--            -- i1 negative, i2 consistent and containing zero
--            ((_, Just True), (_, Just True), (_, Just True), (Just True, _)) -> 
--                (l1 `timesL` h2, l1 `timesR` l2)
--            -- i1 negative, i2 anti-consistent and anti-containing zero
--            ((_, Just True), (_, Just True), (Just True, _), (_, Just True)) -> 
--                (h1 `timesL` h2, h1 `timesR` l2)
--            -- i1 negative, nothing known about i2:
--            ((_, Just True), (_, Just True), _, _) -> 
--                ((h1 `timesL` h2) `combineL` (l1 `timesL` h2), 
--                 (h1 `timesR` l2) `combineR` (l1 `timesR` l2))
--
--            -- i1 positive, i2 positive
--            ((Just True, _), (Just True, _), (Just True, _), (Just True, _)) -> 
--                (l1 `timesL` l2, h1 `timesR` h2)
--            -- i1 positive, i2 negative
--            ((Just True, _), (Just True, _), (_, Just True), (_, Just True)) -> 
--                (h1 `timesL` l2, l1 `timesR` h2)
--            -- i1 positive, i2 consistent and containing zero
--            ((Just True, _), (Just True, _), (_, Just True), (Just True, _)) -> 
--                (h1 `timesL` l2, h1 `timesR` h2)
--            -- i1 positive, i2 anti-consistent and anti-containing zero
--            ((Just True, _), (Just True, _), (Just True, _), (_, Just True)) -> 
--                (l1 `timesL` l2, l1 `timesR` h2)
--
--            -- i1 positive, nothing known about i2:
--            ((Just True, _), (Just True, _), _, _) -> 
--                ((h1 `timesL` l2) `combineL` (l1 `timesL` l2), 
--                 (h1 `timesR` h2) `combineR` (l1 `timesR` h2))
--            
-- 
--            -- i1 consistent and containing zero, i2 positive
--            ((_, Just True), (Just True, _), (Just True, _), (Just True, _)) -> 
--                (l1 `timesL` h2, h1 `timesR` h2)
--            -- i1 anti-consistent and anti-containing zero, i2 positive
--            ((Just True, _), (_, Just True), (Just True, _), (Just True, _)) -> 
--                (l1 `timesL` l2, h1 `timesR` l2)
--            -- nothing known about i1, i2 positive
--            (_, _, (Just True, _), (Just True, _)) -> 
--                ((l1 `timesL` h2) `combineL` (l1 `timesL` l2), 
--                 (h1 `timesR` h2) `combineR` (h1 `timesR` l2))
--
--            -- i1 consistent and containing zero, i2 negative
--            ((_, Just True), (Just True, _), (_, Just True), (_, Just True)) -> 
--                (h1 `timesL` l2, l1 `timesR` l2)
--            -- i1 anti-consistent and anti-containing zero, i2 negative
--            ((Just True, _), (_, Just True), (_, Just True), (_, Just True)) -> 
--                (h1 `timesL` h2, l1 `timesR` h2)
--            -- nothing known about i1, i2 negative
--            (_, _, (_, Just True), (_, Just True)) -> 
--                ((h1 `timesL` h2) `combineL` (h1 `timesL` l2), 
--                 (l1 `timesR` h2) `combineR` (l1 `timesR` l2))
--
--            -----------------------------------------------------------
--            -- cases where both i1 or i2 are around zero
--            -----------------------------------------------------------
--
--            -- i1 consistent and containing zero, i2 consistent and containing zero
--            ((_, Just True), (Just True, _), (_, Just True), (Just True, _)) ->
--                ((l1 `timesL` h2) `minL` (h1 `timesL` l2), 
--                 (l1 `timesR` l2) `maxR` (h1 `timesR` h2))
--            -- i1 consistent and containing zero, i2 anti-consistent and anti-containing zero
--            ((_, Just True), (Just True, _), (Just True, _), (_, Just True)) ->
--                (zero, zero)
--            -- i1 consistent and containing zero, i2 unknown
--            ((_, Just True), (Just True, _), _, _) ->
--                (((l1 `timesL` h2) `combineL` (h1 `timesL` l2)) `combineL` zero,
--                 ((l1 `timesR` l2) `combineR` (h1 `timesR` h2)) `combineR` zero)
--                
--            -- i1 anti-consistent and anti-containing zero, i2 consistent and containing zero
--            ((Just True, _), (_, Just True), (_, Just True), (Just True, _)) ->
--                (zero, zero)
--            -- i1 anti-consistent and anti-containing zero, i2 anti-consistent and anti-containing zero
--            ((Just True, _), (_, Just True), (Just True, _), (_, Just True)) ->
--                ((l1 `timesL` l2) `maxL` (h1 `timesL` h2),
--                 (l1 `timesR` h2) `minR` (h1 `timesR` l2)) 
--            -- i1 anti-consistent and anti-containing zero, i2 unknown
--            ((Just True, _), (_, Just True), _, _) -> 
--                ((l1 `timesL` l2) `combineL` (h1 `timesL` h2) `combineL` zero,
--                 (l1 `timesR` h2) `combineR` (h1 `timesR` l2) `combineR` zero) 
--                
--            -- i1 unknown, i2 anti-consistent and anti-containing zero
--            (_, _, (Just True, _), (_, Just True)) -> 
--                ((l1 `timesL` l2) `combineL` (h1 `timesL` h2) `combineL` zero,
--                 (l1 `timesR` h2) `combineR` (h1 `timesR` l2) `combineR` zero) 
--
--            -- i1 unknown, i2 consistent and containing zero
--            (_, _, (_, Just True), (Just True, _)) -> 
--                ((l1 `timesL` h2) `combineL` (h1 `timesL` l2) `combineL` zero, 
--                 (l1 `timesR` l2) `combineR` (h1 `timesR` h2) `combineR` zero)
--
--            -- both i1 and i2 unknown sign
--            _ ->
--                (foldl1 combineL [l1 `timesL` h2, h1 `timesL` l2, l1 `timesL` l2, h1 `timesL` h2], 
--                 foldl1 combineR [l1 `timesR` h2, h1 `timesR` l2, l1 `timesR` l2, h1 `timesR` h2])
--
--
--    