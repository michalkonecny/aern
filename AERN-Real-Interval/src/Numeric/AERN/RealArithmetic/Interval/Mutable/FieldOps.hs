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
import Numeric.AERN.Basics.Interval.Mutable

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.Interval.FieldOps

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Control.Monad.ST (ST)


instance (ArithUpDn.RoundedAddInPlace e, CanBeMutable e) => 
    RoundedAddInPlace (Interval e) 
    where
    addInInPlaceEff eff (MInterval resL resH) (MInterval aL aH) (MInterval bL bH) =
        do
        ArithUpDn.addUpInPlaceEff eff resL aL bL
        ArithUpDn.addDnInPlaceEff eff resH aH bH
    addOutInPlaceEff eff (MInterval resL resH) (MInterval aL aH) (MInterval bL bH) =
        do
        ArithUpDn.addDnInPlaceEff eff resL aL bL
        ArithUpDn.addUpInPlaceEff eff resH aH bH
    
instance 
    (ArithUpDn.RoundedAddInPlace e,
     CanBeMutable e,
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
        (MInterval lResM hResM) (MInterval l1M h1M) (MInterval l2M h2M) =
    do
    let _ = [minLInPlace, maxRInPlace, combineLInPlace, combineRInPlace]
    l1 <- readMutable l1M
    h1 <- readMutable h1M
    l2 <- readMutable l2M
    h2 <- readMutable h2M
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
--                (l1 `timesL` h2, h1 `timesR` l2)
                assignResEndpointsUsingTimesLR  l1M h2M  h1M l2M
            -- i1 negative, i2 negative
            ((_, Just True), (_, Just True), (_, Just True), (_, Just True)) -> 
--                (h1 `timesL` h2, l1 `timesR` l2)
                assignResEndpointsUsingTimesLR  h1M h2M  l1M l2M
            -- i1 negative, i2 consistent and containing zero
            ((_, Just True), (_, Just True), (_, Just True), (Just True, _)) -> 
--                (l1 `timesL` h2, l1 `timesR` l2)
                assignResEndpointsUsingTimesLR  l1M h2M  l1M l2M
            -- i1 negative, i2 anti-consistent and anti-containing zero
            ((_, Just True), (_, Just True), (Just True, _), (_, Just True)) -> 
--                (h1 `timesL` h2, h1 `timesR` l2)
                assignResEndpointsUsingTimesLR  h1M h2M  h1M l2M
            -- i1 negative, nothing known about i2:
            ((_, Just True), (_, Just True), _, _) -> 
--                ((h1 `timesL` h2) `combineL` (l1 `timesL` h2), 
--                 (h1 `timesR` l2) `combineR` (l1 `timesR` l2))
                do
                temp1 <- makeMutable zero 
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                timesLInPlace temp1 h1M h2M 
                timesLInPlace temp2 l1M h2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 h1M l2M 
                timesRInPlace temp2 l1M l2M 
                combineRInPlace hResM temp1 temp2
                assignMutable lResM temp3

            -- i1 positive, i2 positive
            ((Just True, _), (Just True, _), (Just True, _), (Just True, _)) -> 
--                (l1 `timesL` l2, h1 `timesR` h2)
                do
                timesLInPlace lResM l1M l2M 
                timesRInPlace hResM h1M h2M 
            -- i1 positive, i2 negative
            ((Just True, _), (Just True, _), (_, Just True), (_, Just True)) -> 
--                (h1 `timesL` l2, l1 `timesR` h2)
                assignResEndpointsUsingTimesLR  h1M l2M  l1M h2M
            -- i1 positive, i2 consistent and containing zero
            ((Just True, _), (Just True, _), (_, Just True), (Just True, _)) -> 
--                (h1 `timesL` l2, h1 `timesR` h2)
                assignResEndpointsUsingTimesLR  h1M l2M  h1M h2M
            -- i1 positive, i2 anti-consistent and anti-containing zero
            ((Just True, _), (Just True, _), (Just True, _), (_, Just True)) -> 
--                (l1 `timesL` l2, l1 `timesR` h2)
                assignResEndpointsUsingTimesLR  l1M l2M  l1M h2M
            -- i1 positive, nothing known about i2:
            ((Just True, _), (Just True, _), _, _) -> 
--                ((h1 `timesL` l2) `combineL` (l1 `timesL` l2), 
--                 (h1 `timesR` h2) `combineR` (l1 `timesR` h2))
                do
                temp1 <- makeMutable zero 
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                timesLInPlace temp1 h1M l2M 
                timesLInPlace temp2 l1M l2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 h1M h2M 
                timesRInPlace temp2 l1M h2M 
                combineRInPlace hResM temp1 temp2
                assignMutable lResM temp3
            
 
            -- i1 consistent and containing zero, i2 positive
            ((_, Just True), (Just True, _), (Just True, _), (Just True, _)) -> 
--                (l1 `timesL` h2, h1 `timesR` h2)
                assignResEndpointsUsingTimesLR  l1M h2M  h1M h2M
            -- i1 anti-consistent and anti-containing zero, i2 positive
            ((Just True, _), (_, Just True), (Just True, _), (Just True, _)) -> 
--                (l1 `timesL` l2, h1 `timesR` l2)
                assignResEndpointsUsingTimesLR  l1M l2M  h1M l2M
            -- nothing known about i1, i2 positive
            (_, _, (Just True, _), (Just True, _)) -> 
--                ((l1 `timesL` h2) `combineL` (l1 `timesL` l2), 
--                 (h1 `timesR` h2) `combineR` (h1 `timesR` l2))
                do
                temp1 <- makeMutable zero 
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                timesLInPlace temp1 l1M h2M 
                timesLInPlace temp2 l1M l2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 h1M h2M 
                timesRInPlace temp2 h1M l2M 
                combineRInPlace hResM temp1 temp2
                assignMutable lResM temp3

            -- i1 consistent and containing zero, i2 negative
            ((_, Just True), (Just True, _), (_, Just True), (_, Just True)) -> 
--                (h1 `timesL` l2, l1 `timesR` l2)
                assignResEndpointsUsingTimesLR  h1M l2M  l1M l2M
            -- i1 anti-consistent and anti-containing zero, i2 negative
            ((Just True, _), (_, Just True), (_, Just True), (_, Just True)) -> 
--                (h1 `timesL` h2, l1 `timesR` h2)
                assignResEndpointsUsingTimesLR  h1M h2M  l1M h2M
            -- nothing known about i1, i2 negative
            (_, _, (_, Just True), (_, Just True)) -> 
--                ((h1 `timesL` h2) `combineL` (h1 `timesL` l2), 
--                 (l1 `timesR` h2) `combineR` (l1 `timesR` l2))
                do
                temp1 <- makeMutable zero 
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                timesLInPlace temp1 h1M h2M 
                timesLInPlace temp2 h1M l2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M h2M 
                timesRInPlace temp2 l1M l2M 
                combineRInPlace hResM temp1 temp2
                assignMutable lResM temp3

            -----------------------------------------------------------
            -- cases where both i1 or i2 are around zero
            -----------------------------------------------------------

            -- i1 consistent and containing zero, i2 consistent and containing zero
            ((_, Just True), (Just True, _), (_, Just True), (Just True, _)) ->
--                ((l1 `timesL` h2) `minL` (h1 `timesL` l2), 
--                 (l1 `timesR` l2) `maxR` (h1 `timesR` h2))
                do
                temp1 <- makeMutable zero 
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                timesLInPlace temp1 l1M h2M 
                timesLInPlace temp2 h1M l2M 
                minLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M l2M 
                timesRInPlace temp2 h1M h2M 
                maxRInPlace hResM temp1 temp2
                assignMutable lResM temp3
            -- i1 consistent and containing zero, i2 anti-consistent and anti-containing zero
            ((_, Just True), (Just True, _), (Just True, _), (_, Just True)) ->
--                (zero, zero)
                do
                let z = zero
                let _ = [z,l1]
                writeMutable lResM z
                writeMutable hResM z
            -- i1 consistent and containing zero, i2 unknown
            ((_, Just True), (Just True, _), _, _) ->
--                (((l1 `timesL` h2) `combineL` (h1 `timesL` l2)) `combineL` zero,
--                 ((l1 `timesR` l2) `combineR` (h1 `timesR` h2)) `combineR` zero)
                do
                temp1 <- makeMutable zero
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                timesLInPlace temp1 l1M h2M 
                timesLInPlace temp2 h1M l2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M l2M 
                timesRInPlace temp2 h1M h2M 
                combineRInPlace hResM temp1 temp2
                assignMutable lResM temp3
                let z = zero
                let _ = [z,l1]
                writeMutable temp1 z
                combineLInPlace lResM lResM temp1
                combineRInPlace hResM hResM temp1
                
            -- i1 anti-consistent and anti-containing zero, i2 consistent and containing zero
            ((Just True, _), (_, Just True), (_, Just True), (Just True, _)) ->
--                (zero, zero)
                do
                let z = zero
                let _ = [z,l1]
                writeMutable lResM z
                writeMutable hResM z
            -- i1 anti-consistent and anti-containing zero, i2 anti-consistent and anti-containing zero
            ((Just True, _), (_, Just True), (Just True, _), (_, Just True)) ->
--                ((l1 `timesL` l2) `maxL` (h1 `timesL` h2),
--                 (l1 `timesR` h2) `minR` (h1 `timesR` l2)) 
                do
                temp1 <- makeMutable zero 
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                timesLInPlace temp1 l1M l2M 
                timesLInPlace temp2 h1M h2M 
                maxLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M h2M 
                timesRInPlace temp2 h1M l2M 
                minRInPlace hResM temp1 temp2
                assignMutable lResM temp3
            -- i1 anti-consistent and anti-containing zero, i2 unknown
            ((Just True, _), (_, Just True), _, _) -> 
--                ((l1 `timesL` l2) `combineL` (h1 `timesL` h2) `combineL` zero,
--                 (l1 `timesR` h2) `combineR` (h1 `timesR` l2) `combineR` zero) 
                do
                temp1 <- makeMutable zero
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                timesLInPlace temp1 l1M l2M 
                timesLInPlace temp2 h1M h2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M h2M 
                timesRInPlace temp2 h1M l2M 
                combineRInPlace hResM temp1 temp2
                assignMutable lResM temp3
                let z = zero
                let _ = [z,l1]
                writeMutable temp1 z
                combineLInPlace lResM lResM temp1
                combineRInPlace hResM hResM temp1
                
            -- i1 unknown, i2 anti-consistent and anti-containing zero
            (_, _, (Just True, _), (_, Just True)) -> 
--                ((l1 `timesL` l2) `combineL` (h1 `timesL` h2) `combineL` zero,
--                 (l1 `timesR` h2) `combineR` (h1 `timesR` l2) `combineR` zero) 
                do
                temp1 <- makeMutable zero
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                timesLInPlace temp1 l1M l2M 
                timesLInPlace temp2 h1M h2M 
                combineLInPlace temp3 temp1 temp2
                timesRInPlace temp1 l1M h2M 
                timesRInPlace temp2 h1M l2M 
                combineRInPlace hResM temp1 temp2
                assignMutable lResM temp3
                let z = zero
                let _ = [z,l1]
                writeMutable temp1 z
                combineLInPlace lResM lResM temp1
                combineRInPlace hResM hResM temp1

            -- i1 unknown, i2 consistent and containing zero
            (_, _, (_, Just True), (Just True, _)) -> 
--                ((l1 `timesL` h2) `combineL` (h1 `timesL` l2) `combineL` zero, 
--                 (l1 `timesR` l2) `combineR` (h1 `timesR` h2) `combineR` zero)
                do
                temp1 <- makeMutable zero
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                
                timesLInPlace temp1 l1M h2M 
                timesLInPlace temp2 h1M l2M 
                combineLInPlace temp3 temp1 temp2
                
                timesRInPlace temp1 l1M l2M 
                timesRInPlace temp2 h1M h2M 
                combineRInPlace hResM temp1 temp2
                
                assignMutable lResM temp3
                
                let z = zero
                let _ = [z,l1]
                writeMutable temp1 z
                combineLInPlace lResM lResM temp1
                combineRInPlace hResM hResM temp1

            -- both i1 and i2 unknown sign
            _ ->
--                (foldl1 combineL [l1 `timesL` h2, h1 `timesL` l2, l1 `timesL` l2, h1 `timesL` h2], 
--                 foldl1 combineR [l1 `timesR` h2, h1 `timesR` l2, l1 `timesR` l2, h1 `timesR` h2])
                do
                temp1 <- makeMutable zero
                temp2 <- makeMutable zero
                temp3 <- makeMutable zero
                
                timesLInPlace temp1 l1M h2M 
                timesLInPlace temp2 h1M l2M
                combineLInPlace temp1 temp1 temp2
                timesLInPlace temp2 l1M l2M
                combineLInPlace temp1 temp1 temp2
                timesLInPlace temp2 h1M h2M
                combineLInPlace temp3 temp1 temp2
                
                timesRInPlace temp1 l1M h2M 
                timesRInPlace temp2 h1M l2M 
                combineRInPlace temp1 temp1 temp2
                timesRInPlace temp2 l1M l2M 
                combineRInPlace temp1 temp1 temp2
                timesRInPlace temp2 h1M h2M 
                combineRInPlace hResM temp1 temp2
                
                assignMutable lResM temp3
    where
    assignResEndpointsUsingTimesLR l1M l2M h1M h2M =
        do
        temp1 <- makeMutable zero
        timesLInPlace temp1 l1M l2M -- beware of aliasing between res and param
        timesRInPlace hResM h1M h2M
        assignMutable lResM temp1


instance 
    (RoundedSubtrInPlace (Interval e), 
     RoundedMultiplyInPlace (Interval e),
     RoundedRingEffort (Interval e)) => 
    RoundedRingInPlace (Interval e)

instance
    (ArithUpDn.RoundedPowerNonnegToNonnegIntInPlace e,
     RoundedPowerToNonnegInt (Interval e),
     RoundedMultiplyInPlace (Interval e),
     HasOne e,  HasZero e, NegInPlace e,
     NumOrd.PartialComparison e, NumOrd.RoundedLatticeInPlace e,
     CanBeMutable e
     ) => 
    RoundedPowerToNonnegIntInPlace (Interval e)
    where
    powerToNonnegIntInInPlaceEff
            (effPowerEndpt, effComp, effPowerFromMult@(_,effMinMax,_)) 
            res@(MInterval resL resH) a@(MInterval aL aH) n =
        do
        l <- readMutable aL 
        h <- readMutable aH
        case (pNonnegNonposEff effComp l, pNonnegNonposEff effComp h) of
            ((Just True, _), (Just True, _)) -> -- both non-negative
                do
                ArithUpDn.powerNonnegToNonnegIntUpInPlaceEff 
                    effPowerEndpt resL aL n 
                ArithUpDn.powerNonnegToNonnegIntDnInPlaceEff 
                    effPowerEndpt resH aH n
            ((_, Just True), (_, Just True)) -> -- both non-positive
                do
                -- negate the parameters, use the result as a temp space (may alias!):
                negInPlace res a
                -- compute the power of the positive interval:
                ArithUpDn.powerNonnegToNonnegIntUpInPlaceEff 
                    effPowerEndpt resL resL n 
                ArithUpDn.powerNonnegToNonnegIntDnInPlaceEff 
                    effPowerEndpt resH resH n
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
                        let zeroI = zero 
                        let _ = [zeroI, Interval l h]
                        zeroM <- unsafeMakeMutable zeroI 
                        NumOrd.maxInnerInPlaceEff effMinMax res res zeroM
                    False -> return ()
    powerToNonnegIntOutInPlaceEff
            (effPowerEndpt, effComp, effPowerFromMult@(_,effMinMax,_)) 
            res@(MInterval resL resH) a@(MInterval aL aH) n =
        do
        l <- readMutable aL 
        h <- readMutable aH
        case (pNonnegNonposEff effComp l, pNonnegNonposEff effComp h) of
            ((Just True, _), (Just True, _)) -> -- both non-negative
                do
                ArithUpDn.powerNonnegToNonnegIntDnInPlaceEff 
                    effPowerEndpt resL aL n 
                ArithUpDn.powerNonnegToNonnegIntUpInPlaceEff 
                    effPowerEndpt resH aH n
            ((_, Just True), (_, Just True)) -> -- both non-positive
                do
                -- negate the parameters, use the result as a temp space (may alias!):
                negInPlace res a
                -- compute the power of the positive interval:
                ArithUpDn.powerNonnegToNonnegIntDnInPlaceEff 
                    effPowerEndpt resL resL n 
                ArithUpDn.powerNonnegToNonnegIntUpInPlaceEff 
                    effPowerEndpt resH resH n
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
                        let zeroI = zero 
                        let _ = [zeroI, Interval l h]
                        zeroM <- unsafeMakeMutable zeroI 
                        NumOrd.maxOuterInPlaceEff effMinMax res res zeroM
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
            res@(MInterval resL resH) a@(MInterval aL aH) b@(MInterval bL bH) =
        do
        temp <- makeMutable zero
        recipIntervalInPlace
            (pPosNonnegNegNonposEff effortComp) 
            divDn
            divUp
            bottom
            temp b
        multOutInPlaceEff (effortComp, effortMinmax, effortMult) res a temp
        where
        bottom = RefOrd.bottom
        sampleI = getDummySample res
        _ = [bottom, sampleI]
        divUp = ArithUpDn.divUpInPlaceEff effortDiv
        divDn = ArithUpDn.divDnInPlaceEff effortDiv
    divInInPlaceEff 
            (effortComp, effortMinmax, (effortMult, effortDiv)) 
            res@(MInterval resL resH) a@(MInterval aL aH) b@(MInterval bL bH) =
        do
        temp <- makeMutable zero
        recipIntervalInPlace
            (pPosNonnegNegNonposEff effortComp) 
            divUp
            divDn
            top
            temp b
        multInInPlaceEff (effortComp, effortMinmax, effortMult) res a temp
        where
        top = RefOrd.top
        sampleI = getDummySample res
        _ = [top, sampleI]
        divUp = ArithUpDn.divUpInPlaceEff effortDiv
        divDn = ArithUpDn.divDnInPlaceEff effortDiv

recipIntervalInPlace pPosNonnegNegNonpos divL divR fallback 
        res@(MInterval resL resH) a@(MInterval aL aH) =
    do
    let oneP = one
    let top = RefOrd.top 
    let bottom = RefOrd.bottom
    let _ = [top, bottom, fallback] 
    oneM <- unsafeMakeMutable oneP  
    l <- readMutable aL
    h <- readMutable aH
    let _ = [l, h, oneP]
    case (pPosNonnegNegNonpos l, pPosNonnegNegNonpos h) of
        -- positive:
        ((Just True, _, _, _), (Just True, _, _, _)) ->
            do
            divL resL oneM aH
            divR resH oneM aL
        -- negative:
        ((_, _, Just True, _), (_, _, Just True, _)) ->  
            do
            divL resL oneM aH
            divR resH oneM aL
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
     ArithUpDn.RoundedPowerNonnegToNonnegIntInPlace e,
     HasZero e, NegInPlace e, HasOne e, 
     NumOrd.HasExtrema e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeInPlace e) => 
    RoundedFieldInPlace (Interval e)
    