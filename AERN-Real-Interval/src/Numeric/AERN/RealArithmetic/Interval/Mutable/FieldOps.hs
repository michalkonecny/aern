{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Mutable.FieldOps
    Description :  field operations for mutable intervals 
    Copyright   :  (c) Michal Konecny, Jan Duracz
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
import Numeric.AERN.Basics.Interval.Mutable

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.Interval.FieldOps

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

import Control.Monad.ST (ST)


instance (ArithUpDn.RoundedAddInPlace e, CanBeMutable e) => 
    RoundedAddInPlace (Interval e) 
    where
    addInInPlaceEff eff (MInterval resL resR) (MInterval aL aR) (MInterval bL bR) =
        do
        ArithUpDn.addUpInPlaceEff eff resL aL bL
        ArithUpDn.addDnInPlaceEff eff resR aR bR
    addOutInPlaceEff eff (MInterval resL resR) (MInterval aL aR) (MInterval bL bR) =
        do
        ArithUpDn.addDnInPlaceEff eff resL aL bL
        ArithUpDn.addUpInPlaceEff eff resR aR bR
    
instance 
    (ArithUpDn.RoundedAddInPlace e,
     CanBeMutable e,
     Neg e,
     NegInPlace e) => 
    RoundedSubtrInPlace (Interval e) 

instance (RoundedAbs (Interval e), CanBeMutable (Interval e)) => 
    RoundedAbsInPlace (Interval e) 

instance 
    (ArithUpDn.RoundedMultiplyInPlace e,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  NumOrd.PartialComparison e,
     CanBeMutable e) => 
    RoundedMultiplyInPlace (Interval e) 
    where
    multOutInPlaceEff (effortComp, effortMinmax, effortMult) r i1 i2 =
        multiplyIntervalsInPlace
            (pNonnegNonposEff effortComp)
            (ArithUpDn.multDnInPlaceEff effortMult) 
            (ArithUpDn.multUpInPlaceEff effortMult)
            (NumOrd.minDnInPlaceEff effortMinmax) -- minL
            (NumOrd.minUpInPlaceEff effortMinmax) -- minR
            (NumOrd.maxDnInPlaceEff effortMinmax) -- maxL
            (NumOrd.maxUpInPlaceEff effortMinmax) -- maxR
            (NumOrd.minDnInPlaceEff effortMinmax)
            (NumOrd.maxUpInPlaceEff effortMinmax) 
            r i1 i2
    multInInPlaceEff (effortComp, effortMinmax, effortMult) r i1 i2 =
        multiplyIntervalsInPlace
            (pNonnegNonposEff effortComp)
            (ArithUpDn.multUpInPlaceEff effortMult) 
            (ArithUpDn.multDnInPlaceEff effortMult)
            (NumOrd.minUpInPlaceEff effortMinmax) -- minL
            (NumOrd.minDnInPlaceEff effortMinmax) -- minR
            (NumOrd.maxUpInPlaceEff effortMinmax) -- maxL
            (NumOrd.maxDnInPlaceEff effortMinmax) -- maxR
            (NumOrd.maxUpInPlaceEff effortMinmax)
            (NumOrd.minDnInPlaceEff effortMinmax) 
            r i1 i2
    
multiplyIntervalsInPlace ::
    (CanBeMutable e, HasZero e) =>
    (e -> (Maybe Bool, Maybe Bool)) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (OpMutable2 e s) ->
    (Mutable (Interval e) s) ->
    (Mutable (Interval e) s) ->
    (Mutable (Interval e) s) ->
    ST s ()
multiplyIntervalsInPlace
        pNonnegNonpos timesLInPlace timesRInPlace 
        minLInPlace minRInPlace maxLInPlace maxRInPlace 
        combineLInPlace combineRInPlace
        (MInterval lResM rResM) (MInterval l1M r1M) (MInterval l2M r2M) =
    do
    let _ = [minLInPlace, maxRInPlace, combineLInPlace, combineRInPlace]
    l1 <- readMutable l1M
    r1 <- readMutable r1M
    l2 <- readMutable l2M
    r2 <- readMutable r2M
    case (pNonnegNonpos l1, -- sign of l1 
              pNonnegNonpos r1, -- sign of r1
              pNonnegNonpos l2, -- sign of l2
              pNonnegNonpos r2 -- sign of r2 
             ) of
             
            -----------------------------------------------------------
            -- cases where i1 or i2 is known to be positive or negative
            -----------------------------------------------------------
            -- i1 negative, i2 positive
            ((_, Just True), (_, Just True), (Just True, _), (Just True, _)) ->
--                (l1 `timesL` r2, r1 `timesR` l2)
                assignResEndpointsUsingTimesLR l1 l1M r2M  r1M l2M
            -- i1 negative, i2 negative
            ((_, Just True), (_, Just True), (_, Just True), (_, Just True)) -> 
--                (r1 `timesL` r2, l1 `timesR` l2)
                assignResEndpointsUsingTimesLR l1 r1M r2M  l1M l2M
            -- i1 negative, i2 consistent and containing zero
            ((_, Just True), (_, Just True), (_, Just True), (Just True, _)) -> 
--                (l1 `timesL` r2, l1 `timesR` l2)
                assignResEndpointsUsingTimesLR l1 l1M r2M  l1M l2M
            -- i1 negative, i2 anti-consistent and anti-containing zero
            ((_, Just True), (_, Just True), (Just True, _), (_, Just True)) -> 
--                (r1 `timesL` r2, r1 `timesR` l2)
                assignResEndpointsUsingTimesLR l1 r1M r2M  r1M l2M
            -- i1 negative, nothing known about i2:
            ((_, Just True), (_, Just True), _, _) -> 
--                ((r1 `timesL` r2) `combineL` (l1 `timesL` r2), 
--                 (r1 `timesR` l2) `combineR` (l1 `timesR` l2))
                do
                temp1 <- makeMutable (zero l1) 
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                timesLInPlace temp1 r1M r2M 
                timesLInPlace temp2 l1M r2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 r1M l2M 
                timesRInPlace temp2 l1M l2M 
                combineRInPlace rResM temp1 temp2
                assignMutable lResM temp3

            -- i1 positive, i2 positive
            ((Just True, _), (Just True, _), (Just True, _), (Just True, _)) -> 
--                (l1 `timesL` l2, r1 `timesR` r2)
                do
                timesLInPlace lResM l1M l2M 
                timesRInPlace rResM r1M r2M 
            -- i1 positive, i2 negative
            ((Just True, _), (Just True, _), (_, Just True), (_, Just True)) -> 
--                (r1 `timesL` l2, l1 `timesR` r2)
                assignResEndpointsUsingTimesLR l1 r1M l2M  l1M r2M
            -- i1 positive, i2 consistent and containing zero
            ((Just True, _), (Just True, _), (_, Just True), (Just True, _)) -> 
--                (r1 `timesL` l2, r1 `timesR` r2)
                assignResEndpointsUsingTimesLR l1 r1M l2M  r1M r2M
            -- i1 positive, i2 anti-consistent and anti-containing zero
            ((Just True, _), (Just True, _), (Just True, _), (_, Just True)) -> 
--                (l1 `timesL` l2, l1 `timesR` r2)
                assignResEndpointsUsingTimesLR l1 l1M l2M  l1M r2M
            -- i1 positive, nothing known about i2:
            ((Just True, _), (Just True, _), _, _) -> 
--                ((r1 `timesL` l2) `combineL` (l1 `timesL` l2), 
--                 (r1 `timesR` r2) `combineR` (l1 `timesR` r2))
                do
                temp1 <- makeMutable (zero l1) 
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                timesLInPlace temp1 r1M l2M 
                timesLInPlace temp2 l1M l2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 r1M r2M 
                timesRInPlace temp2 l1M r2M 
                combineRInPlace rResM temp1 temp2
                assignMutable lResM temp3
            
 
            -- i1 consistent and containing zero, i2 positive
            ((_, Just True), (Just True, _), (Just True, _), (Just True, _)) -> 
--                (l1 `timesL` r2, r1 `timesR` r2)
                assignResEndpointsUsingTimesLR l1 l1M r2M  r1M r2M
            -- i1 anti-consistent and anti-containing zero, i2 positive
            ((Just True, _), (_, Just True), (Just True, _), (Just True, _)) -> 
--                (l1 `timesL` l2, r1 `timesR` l2)
                assignResEndpointsUsingTimesLR l1 l1M l2M  r1M l2M
            -- nothing known about i1, i2 positive
            (_, _, (Just True, _), (Just True, _)) -> 
--                ((l1 `timesL` r2) `combineL` (l1 `timesL` l2), 
--                 (r1 `timesR` r2) `combineR` (r1 `timesR` l2))
                do
                temp1 <- makeMutable (zero l1) 
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                timesLInPlace temp1 l1M r2M 
                timesLInPlace temp2 l1M l2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 r1M r2M 
                timesRInPlace temp2 r1M l2M 
                combineRInPlace rResM temp1 temp2
                assignMutable lResM temp3

            -- i1 consistent and containing zero, i2 negative
            ((_, Just True), (Just True, _), (_, Just True), (_, Just True)) -> 
--                (r1 `timesL` l2, l1 `timesR` l2)
                assignResEndpointsUsingTimesLR l1 r1M l2M  l1M l2M
            -- i1 anti-consistent and anti-containing zero, i2 negative
            ((Just True, _), (_, Just True), (_, Just True), (_, Just True)) -> 
--                (r1 `timesL` r2, l1 `timesR` r2)
                assignResEndpointsUsingTimesLR l1 r1M r2M  l1M r2M
            -- nothing known about i1, i2 negative
            (_, _, (_, Just True), (_, Just True)) -> 
--                ((r1 `timesL` r2) `combineL` (r1 `timesL` l2), 
--                 (l1 `timesR` r2) `combineR` (l1 `timesR` l2))
                do
                temp1 <- makeMutable (zero l1) 
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                timesLInPlace temp1 r1M r2M 
                timesLInPlace temp2 r1M l2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M r2M 
                timesRInPlace temp2 l1M l2M 
                combineRInPlace rResM temp1 temp2
                assignMutable lResM temp3

            -----------------------------------------------------------
            -- cases where both i1 or i2 are around zero
            -----------------------------------------------------------

            -- i1 consistent and containing zero, i2 consistent and containing zero
            ((_, Just True), (Just True, _), (_, Just True), (Just True, _)) ->
--                ((l1 `timesL` r2) `minL` (r1 `timesL` l2), 
--                 (l1 `timesR` l2) `maxR` (r1 `timesR` r2))
                do
                temp1 <- makeMutable (zero l1) 
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                timesLInPlace temp1 l1M r2M 
                timesLInPlace temp2 r1M l2M 
                minLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M l2M 
                timesRInPlace temp2 r1M r2M 
                maxRInPlace rResM temp1 temp2
                assignMutable lResM temp3
            -- i1 consistent and containing zero, i2 anti-consistent and anti-containing zero
            ((_, Just True), (Just True, _), (Just True, _), (_, Just True)) ->
--                (zero, zero)
                do
                let z = (zero l1)
                writeMutable lResM z
                writeMutable rResM z
            -- i1 consistent and containing zero, i2 unknown
            ((_, Just True), (Just True, _), _, _) ->
--                (((l1 `timesL` r2) `combineL` (r1 `timesL` l2)) `combineL` zero,
--                 ((l1 `timesR` l2) `combineR` (r1 `timesR` r2)) `combineR` zero)
                do
                temp1 <- makeMutable (zero l1)
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                timesLInPlace temp1 l1M r2M 
                timesLInPlace temp2 r1M l2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M l2M 
                timesRInPlace temp2 r1M r2M 
                combineRInPlace rResM temp1 temp2
                assignMutable lResM temp3
                let z = zero l1
                writeMutable temp1 z
                combineLInPlace lResM lResM temp1
                combineRInPlace rResM rResM temp1
                
            -- i1 anti-consistent and anti-containing zero, i2 consistent and containing zero
            ((Just True, _), (_, Just True), (_, Just True), (Just True, _)) ->
--                (zero, zero)
                do
                let z = zero l1
                writeMutable lResM z
                writeMutable rResM z
            -- i1 anti-consistent and anti-containing zero, i2 anti-consistent and anti-containing zero
            ((Just True, _), (_, Just True), (Just True, _), (_, Just True)) ->
--                ((l1 `timesL` l2) `maxL` (r1 `timesL` r2),
--                 (l1 `timesR` r2) `minR` (r1 `timesR` l2)) 
                do
                temp1 <- makeMutable (zero l1) 
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                timesLInPlace temp1 l1M l2M 
                timesLInPlace temp2 r1M r2M 
                maxLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M r2M 
                timesRInPlace temp2 r1M l2M 
                minRInPlace rResM temp1 temp2
                assignMutable lResM temp3
            -- i1 anti-consistent and anti-containing zero, i2 unknown
            ((Just True, _), (_, Just True), _, _) -> 
--                ((l1 `timesL` l2) `combineL` (r1 `timesL` r2) `combineL` zero,
--                 (l1 `timesR` r2) `combineR` (r1 `timesR` l2) `combineR` zero) 
                do
                temp1 <- makeMutable (zero l1)
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                timesLInPlace temp1 l1M l2M 
                timesLInPlace temp2 r1M r2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M r2M 
                timesRInPlace temp2 r1M l2M 
                combineRInPlace rResM temp1 temp2
                assignMutable lResM temp3
                let z = zero l1
                writeMutable temp1 z
                combineLInPlace lResM lResM temp1
                combineRInPlace rResM rResM temp1
                
            -- i1 unknown, i2 anti-consistent and anti-containing zero
            (_, _, (Just True, _), (_, Just True)) -> 
--                ((l1 `timesL` l2) `combineL` (r1 `timesL` r2) `combineL` zero,
--                 (l1 `timesR` r2) `combineR` (r1 `timesR` l2) `combineR` zero) 
                do
                temp1 <- makeMutable (zero l1)
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                timesLInPlace temp1 l1M l2M 
                timesLInPlace temp2 r1M r2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M r2M 
                timesRInPlace temp2 r1M l2M 
                combineRInPlace rResM temp1 temp2
                assignMutable lResM temp3
                let z = zero l1
                writeMutable temp1 z
                combineLInPlace lResM lResM temp1
                combineRInPlace rResM rResM temp1

            -- i1 unknown, i2 consistent and containing zero
            (_, _, (_, Just True), (Just True, _)) -> 
--                ((l1 `timesL` r2) `combineL` (r1 `timesL` l2) `combineL` zero, 
--                 (l1 `timesR` l2) `combineR` (r1 `timesR` r2) `combineR` zero)
                do
                temp1 <- makeMutable (zero l1)
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                
                timesLInPlace temp1 l1M r2M 
                timesLInPlace temp2 r1M l2M 
                combineLInPlace temp3 temp1 temp2
                
                timesRInPlace temp1 l1M l2M 
                timesRInPlace temp2 r1M r2M 
                combineRInPlace rResM temp1 temp2
                
                assignMutable lResM temp3
                
                let z = zero l1
                writeMutable temp1 z
                combineLInPlace lResM lResM temp1
                combineRInPlace rResM rResM temp1

            -- both i1 and i2 unknown sign
            _ ->
--                (foldl1 combineL [l1 `timesL` r2, r1 `timesL` l2, l1 `timesL` l2, r1 `timesL` r2], 
--                 foldl1 combineR [l1 `timesR` r2, r1 `timesR` l2, l1 `timesR` l2, r1 `timesR` r2])
                do
                temp1 <- makeMutable (zero l1)
                temp2 <- makeMutable (zero l1)
                temp3 <- makeMutable (zero l1)
                
                timesLInPlace temp1 l1M r2M 
                timesLInPlace temp2 r1M l2M
                combineLInPlace temp1 temp1 temp2
                timesLInPlace temp2 l1M l2M
                combineLInPlace temp1 temp1 temp2
                timesLInPlace temp2 r1M r2M
                combineLInPlace temp3 temp1 temp2
                
                timesRInPlace temp1 l1M r2M 
                timesRInPlace temp2 r1M l2M 
                combineRInPlace temp1 temp1 temp2
                timesRInPlace temp2 l1M l2M 
                combineRInPlace temp1 temp1 temp2
                timesRInPlace temp2 r1M r2M 
                combineRInPlace rResM temp1 temp2
                
                assignMutable lResM temp3
    where
    assignResEndpointsUsingTimesLR sampleE l1M l2M r1M r2M =
        do
        temp1 <- makeMutable (zero sampleE)
        timesLInPlace temp1 l1M l2M -- beware of aliasing between res and param
        timesRInPlace rResM r1M r2M
        assignMutable lResM temp1


instance 
    (RoundedSubtrInPlace (Interval e), 
     RoundedMultiplyInPlace (Interval e),
     RoundedPowerToNonnegIntInPlace (Interval e),
     RoundedRingEffort (Interval e)) => 
    RoundedRingInPlace (Interval e)

instance
    (ArithUpDn.RoundedPowerNonnegToNonnegIntInPlace e,
     RoundedPowerToNonnegInt (Interval e),
     RoundedMultiplyInPlace (Interval e),
     HasOne e,  HasZero e, Neg e, NegInPlace e,
     NumOrd.PartialComparison e, NumOrd.RoundedLatticeInPlace e,
     CanBeMutable e
     ) => 
    RoundedPowerToNonnegIntInPlace (Interval e)
    where
    powerToNonnegIntInInPlaceEff
            (effPowerEndpt, effComp, effPowerFromMult@(_,effMinMax,_)) 
            res@(MInterval resL resR) a@(MInterval aL aR) n =
        do
        l <- readMutable aL 
        r <- readMutable aR
        case (pNonnegNonposEff effComp l, pNonnegNonposEff effComp r) of
            ((Just True, _), (Just True, _)) -> -- both non-negative
                do
                ArithUpDn.powerNonnegToNonnegIntUpInPlaceEff 
                    effPowerEndpt resL aL n 
                ArithUpDn.powerNonnegToNonnegIntDnInPlaceEff 
                    effPowerEndpt resR aR n
            ((_, Just True), (_, Just True)) -> -- both non-positive
                do
                -- negate the parameters, use the result as a temp space (may alias!):
                negInPlace res a
                -- compute the power of the positive interval:
                ArithUpDn.powerNonnegToNonnegIntUpInPlaceEff 
                    effPowerEndpt resL resL n 
                ArithUpDn.powerNonnegToNonnegIntDnInPlaceEff 
                    effPowerEndpt resR resR n
                case even n of
                    True -> 
                        return () -- keep result positive
                    False ->
                        negInPlace res res -- back to the original sign
            _ ->
                do
                powerToNonnegIntInInPlaceEffFromMult effPowerFromMult res a n
                case even n of
                    True ->
                        do
                        let zeroI = zero $ Interval l r
                        zeroM <- unsafeMakeMutable zeroI 
                        NumOrd.maxInInPlaceEff effMinMax res res zeroM
                    False -> return ()
    powerToNonnegIntOutInPlaceEff
            (effPowerEndpt, effComp, effPowerFromMult@(_,effMinMax,_)) 
            res@(MInterval resL resR) a@(MInterval aL aR) n =
        do
        l <- readMutable aL 
        r <- readMutable aR
        case (pNonnegNonposEff effComp l, pNonnegNonposEff effComp r) of
            ((Just True, _), (Just True, _)) -> -- both non-negative
                do
                ArithUpDn.powerNonnegToNonnegIntDnInPlaceEff 
                    effPowerEndpt resL aL n 
                ArithUpDn.powerNonnegToNonnegIntUpInPlaceEff 
                    effPowerEndpt resR aR n
            ((_, Just True), (_, Just True)) -> -- both non-positive
                do
                -- negate the parameters, use the result as a temp space (may alias!):
                negInPlace res a
                -- compute the power of the positive interval:
                ArithUpDn.powerNonnegToNonnegIntDnInPlaceEff 
                    effPowerEndpt resL resL n 
                ArithUpDn.powerNonnegToNonnegIntUpInPlaceEff 
                    effPowerEndpt resR resR n
                case even n of
                    True -> 
                        return () -- keep result positive
                    False ->
                        negInPlace res res -- back to the original sign
            _ ->
                do
                powerToNonnegIntOutInPlaceEffFromMult effPowerFromMult res a n
                case even n of
                    True ->
                        do
                        let zeroI = zero $ Interval l r
                        zeroM <- unsafeMakeMutable zeroI 
                        NumOrd.maxOutInPlaceEff effMinMax res res zeroM
                    False -> return ()

instance 
    (ArithUpDn.RoundedDivideInPlace e,
     ArithUpDn.RoundedMultiplyInPlace e,
     NumOrd.RoundedLatticeInPlace e,
     HasZero e,  HasOne e, Neg e, 
     NumOrd.PartialComparison e,  NumOrd.HasExtrema e,
     CanBeMutable e) => 
    RoundedDivideInPlace (Interval e) 
    where
    divOutInPlaceEff 
            (effortComp, effortMinmax, (effortMult, effortDiv)) 
            res@(MInterval resL resR) a@(MInterval aL aR) b@(MInterval bL bR) =
        do
        temp <- makeMutable $ zero sampleI
        recipIntervalInPlace
            (pPosNonnegNegNonposEff effortComp) 
            divDn
            divUp
            bottom
            temp b
        multOutInPlaceEff (effortComp, effortMinmax, effortMult) res a temp
        where
        bottom = RefOrd.bottom
        sampleI@(Interval sampleE _) = getDummySample res
        _ = [bottom, sampleI]
        divUp = ArithUpDn.divUpInPlaceEff effortDiv
        divDn = ArithUpDn.divDnInPlaceEff effortDiv
    divInInPlaceEff 
            (effortComp, effortMinmax, (effortMult, effortDiv)) 
            res@(MInterval resL resR) a@(MInterval aL aR) b@(MInterval bL bR) =
        do
        temp <- makeMutable $ zero sampleI
        recipIntervalInPlace
            (pPosNonnegNegNonposEff effortComp) 
            divUp
            divDn
            top
            temp b
        multInInPlaceEff (effortComp, effortMinmax, effortMult) res a temp
        where
        top = RefOrd.top
        sampleI@(Interval sampleE _) = getDummySample res
        _ = [top, sampleI]
        divUp = ArithUpDn.divUpInPlaceEff effortDiv
        divDn = ArithUpDn.divDnInPlaceEff effortDiv

recipIntervalInPlace pPosNonnegNegNonpos divL divR fallback 
        res@(MInterval resL resR) a@(MInterval aL aR) =
    do
    l <- readMutable aL
    r <- readMutable aR
    let oneP = one l
    let top = RefOrd.top 
    let bottom = RefOrd.bottom
    let _ = [top, bottom, fallback]
    oneM <- unsafeMakeMutable oneP  
    case (pPosNonnegNegNonpos l, pPosNonnegNegNonpos r) of
        -- positive:
        ((Just True, _, _, _), (Just True, _, _, _)) ->
            do
            divL resL oneM aR
            divR resR oneM aL
        -- negative:
        ((_, _, Just True, _), (_, _, Just True, _)) ->  
            do
            divL resL oneM aR
            divR resR oneM aL
        -- consistent around zero:
        ((_, _, _, Just True), (_, Just True, _, _)) ->
            writeMutable res bottom
        -- anti-consistent around zero:
        ((_, Just True, _, _), (_,_,_, Just True)) ->  
            writeMutable res top
        -- unknown:
        _ ->  
            writeMutable res fallback

instance 
    (ArithUpDn.RoundedFieldInPlace e,
     ArithUpDn.RoundedMultiply e,
     ArithUpDn.RoundedPowerNonnegToNonnegInt e,
     HasZero e, Neg e, NegInPlace e, HasOne e, 
     NumOrd.HasExtrema e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e, 
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedFieldInPlace (Interval e)
    