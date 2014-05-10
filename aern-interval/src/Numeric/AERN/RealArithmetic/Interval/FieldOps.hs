{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.FieldOps
    Description :  refinement rounded basic operations for intervals
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Refinement rounded basic operations for intervals.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.FieldOps
(multiplyIntervals, recipInterval)
where

import Numeric.AERN.Basics.Interval

import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.ExactOps ()

import Numeric.AERN.RealArithmetic.Interval.Effort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd


instance (ArithUpDn.RoundedAddEffort e) => RoundedAddEffort (Interval e) where
    type AddEffortIndicator (Interval e) = ArithUpDn.AddEffortIndicator e
    addDefaultEffort (Interval sampleE _) = ArithUpDn.addDefaultEffort sampleE

instance (ArithUpDn.RoundedAdd e) => RoundedAdd (Interval e) where
    addInEff effort (Interval l1 r1) (Interval l2 r2) =
        Interval 
            (ArithUpDn.addUpEff effort l1 l2)
            (ArithUpDn.addDnEff effort r1 r2)
    addOutEff effort (Interval l1 r1) (Interval l2 r2) =
        Interval 
            (ArithUpDn.addDnEff effort l1 l2)
            (ArithUpDn.addUpEff effort r1 r2)

instance (ArithUpDn.RoundedAdd e, Neg e) => RoundedSubtr (Interval e)

instance 
    (NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeEffort e) => 
    RoundedAbsEffort (Interval e)
    where
    type AbsEffortIndicator (Interval e) = 
        IntervalOrderEffort e
    absDefaultEffort i = 
        defaultIntervalOrderEffort i 

instance 
    (ArithUpDn.RoundedAbs e,  
     HasZero e, Neg e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedAbs (Interval e)
    where
    absOutEff eff = absOutUsingCompMax (eff, eff)
    absInEff eff = absInUsingCompMax (eff, eff)


instance 
    (ArithUpDn.RoundedReal e, 
     RoundedFieldEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e)) 
    => 
    RoundedMultiplyEffort (Interval e)
    where
    type MultEffortIndicator (Interval e) =
        IntervalRealEffort e
    multDefaultEffort i =
        defaultIntervalRealEffort i 

instance 
    (ArithUpDn.RoundedReal e, 
     RoundedFieldEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e)) 
    => 
    RoundedMultiply (Interval e)
    where
    multOutEff effort i1@(Interval sampleE _) i2 =
        fromEndpoints $
        multiplyIntervals 
            (pNonnegNonposEff effortComp)
            (ArithUpDn.multDnEff effortMult) (ArithUpDn.multUpEff effortMult)
            (NumOrd.minDnEff effortMinmax) -- minL
            (NumOrd.minUpEff effortMinmax) -- minR
            (NumOrd.maxDnEff effortMinmax) -- maxL
            (NumOrd.maxUpEff effortMinmax) -- maxR
            (NumOrd.minDnEff effortMinmax)
            (NumOrd.maxUpEff effortMinmax) 
            (getEndpoints i1) (getEndpoints i2)
        where
        effortComp = intrealeff_eComp sampleE effort
        effortMinmax = intrealeff_eMinmax sampleE effort
        effortMult = ArithUpDn.fldEffortMult sampleE effField 
        effField = ArithUpDn.rrEffortField sampleE effE 
        effE = intrealeff_eRoundedReal effort
        
    multInEff effort i1@(Interval sampleE _) i2 =
        fromEndpoints $
        multiplyIntervals 
            (pNonnegNonposEff effortComp)
            (ArithUpDn.multUpEff effortMult) (ArithUpDn.multDnEff effortMult)
            (NumOrd.minUpEff effortMinmax) -- minL
            (NumOrd.minDnEff effortMinmax) -- minR
            (NumOrd.maxUpEff effortMinmax) -- maxL
            (NumOrd.maxDnEff effortMinmax) -- maxR
            (NumOrd.maxUpEff effortMinmax)
            (NumOrd.minDnEff effortMinmax) 
            (getEndpoints i1) (getEndpoints i2)
        where
        effortComp = intrealeff_eComp sampleE effort
        effortMinmax = intrealeff_eMinmax sampleE effort
        effortMult = ArithUpDn.fldEffortMult sampleE effField 
        effField = ArithUpDn.rrEffortField sampleE effE 
        effE = intrealeff_eRoundedReal effort
    
multiplyIntervals :: 
   HasZero t 
   =>
   (t -> (Maybe Bool, Maybe Bool))
   -> (t -> t -> t)
   -> (t -> t -> t)
   -> (t -> t -> t)
   -> (t -> t -> t)
   -> (t -> t -> t)
   -> (t -> t -> t)
   -> (t -> t -> t)
   -> (t -> t -> t)
   -> (t, t)
   -> (t, t)
   -> (t, t)
multiplyIntervals
        pNonnegNonpos timesL timesR minL minR maxL maxR 
        combineL combineR 
        (l1, r1) (l2, r2) =
    let _ = [minL, maxR, combineL, combineR] in
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
                (l1 `timesL` r2, r1 `timesR` l2)
            -- i1 negative, i2 negative
            ((_, Just True), (_, Just True), (_, Just True), (_, Just True)) -> 
                (r1 `timesL` r2, l1 `timesR` l2)
            -- i1 negative, i2 consistent and containing zero
            ((_, Just True), (_, Just True), (_, Just True), (Just True, _)) -> 
                (l1 `timesL` r2, l1 `timesR` l2)
            -- i1 negative, i2 anti-consistent and anti-containing zero
            ((_, Just True), (_, Just True), (Just True, _), (_, Just True)) -> 
                (r1 `timesL` r2, r1 `timesR` l2)
            -- i1 negative, nothing known about i2:
            ((_, Just True), (_, Just True), _, _) -> 
                ((r1 `timesL` r2) `combineL` (l1 `timesL` r2), 
                 (r1 `timesR` l2) `combineR` (l1 `timesR` l2))

            -- i1 positive, i2 positive
            ((Just True, _), (Just True, _), (Just True, _), (Just True, _)) -> 
                (l1 `timesL` l2, r1 `timesR` r2)
            -- i1 positive, i2 negative
            ((Just True, _), (Just True, _), (_, Just True), (_, Just True)) -> 
                (r1 `timesL` l2, l1 `timesR` r2)
            -- i1 positive, i2 consistent and containing zero
            ((Just True, _), (Just True, _), (_, Just True), (Just True, _)) -> 
                (r1 `timesL` l2, r1 `timesR` r2)
            -- i1 positive, i2 anti-consistent and anti-containing zero
            ((Just True, _), (Just True, _), (Just True, _), (_, Just True)) -> 
                (l1 `timesL` l2, l1 `timesR` r2)

            -- i1 positive, nothing known about i2:
            ((Just True, _), (Just True, _), _, _) -> 
                ((r1 `timesL` l2) `combineL` (l1 `timesL` l2), 
                 (r1 `timesR` r2) `combineR` (l1 `timesR` r2))
            
 
            -- i1 consistent and containing zero, i2 positive
            ((_, Just True), (Just True, _), (Just True, _), (Just True, _)) -> 
                (l1 `timesL` r2, r1 `timesR` r2)
            -- i1 anti-consistent and anti-containing zero, i2 positive
            ((Just True, _), (_, Just True), (Just True, _), (Just True, _)) -> 
                (l1 `timesL` l2, r1 `timesR` l2)
            -- nothing known about i1, i2 positive
            (_, _, (Just True, _), (Just True, _)) -> 
                ((l1 `timesL` r2) `combineL` (l1 `timesL` l2), 
                 (r1 `timesR` r2) `combineR` (r1 `timesR` l2))

            -- i1 consistent and containing zero, i2 negative
            ((_, Just True), (Just True, _), (_, Just True), (_, Just True)) -> 
                (r1 `timesL` l2, l1 `timesR` l2)
            -- i1 anti-consistent and anti-containing zero, i2 negative
            ((Just True, _), (_, Just True), (_, Just True), (_, Just True)) -> 
                (r1 `timesL` r2, l1 `timesR` r2)
            -- nothing known about i1, i2 negative
            (_, _, (_, Just True), (_, Just True)) -> 
                ((r1 `timesL` r2) `combineL` (r1 `timesL` l2), 
                 (l1 `timesR` r2) `combineR` (l1 `timesR` l2))

            -----------------------------------------------------------
            -- cases where both i1 or i2 are around zero
            -----------------------------------------------------------

            -- i1 consistent and containing zero, i2 consistent and containing zero
            ((_, Just True), (Just True, _), (_, Just True), (Just True, _)) ->
                ((l1 `timesL` r2) `minL` (r1 `timesL` l2), 
                 (l1 `timesR` l2) `maxR` (r1 `timesR` r2))
            -- i1 consistent and containing zero, i2 anti-consistent and anti-containing zero
            ((_, Just True), (Just True, _), (Just True, _), (_, Just True)) ->
                (z, z)
            -- i1 consistent and containing zero, i2 unknown
            ((_, Just True), (Just True, _), _, _) ->
                (((l1 `timesL` r2) `combineL` (r1 `timesL` l2)) `combineL` z,
                 ((l1 `timesR` l2) `combineR` (r1 `timesR` r2)) `combineR` z)
                
            -- i1 anti-consistent and anti-containing zero, i2 consistent and containing zero
            ((Just True, _), (_, Just True), (_, Just True), (Just True, _)) ->
                (z, z)
            -- i1 anti-consistent and anti-containing zero, i2 anti-consistent and anti-containing zero
            ((Just True, _), (_, Just True), (Just True, _), (_, Just True)) ->
                ((l1 `timesL` l2) `maxL` (r1 `timesL` r2),
                 (l1 `timesR` r2) `minR` (r1 `timesR` l2)) 
            -- i1 anti-consistent and anti-containing zero, i2 unknown
            ((Just True, _), (_, Just True), _, _) -> 
                ((l1 `timesL` l2) `combineL` (r1 `timesL` r2) `combineL` z,
                 (l1 `timesR` r2) `combineR` (r1 `timesR` l2) `combineR` z) 
                
            -- i1 unknown, i2 anti-consistent and anti-containing zero
            (_, _, (Just True, _), (_, Just True)) -> 
                ((l1 `timesL` l2) `combineL` (r1 `timesL` r2) `combineL` z,
                 (l1 `timesR` r2) `combineR` (r1 `timesR` l2) `combineR` z) 

            -- i1 unknown, i2 consistent and containing zero
            (_, _, (_, Just True), (Just True, _)) -> 
                ((l1 `timesL` r2) `combineL` (r1 `timesL` l2) `combineL` z, 
                 (l1 `timesR` l2) `combineR` (r1 `timesR` r2) `combineR` z)

            -- both i1 and i2 unknown sign
            _ ->
                (foldl1 combineL [l1 `timesL` r2, r1 `timesL` l2, l1 `timesL` l2, r1 `timesL` r2], 
                 foldl1 combineR [l1 `timesR` r2, r1 `timesR` l2, l1 `timesR` l2, r1 `timesR` r2])
        where
        z = zero sampleE
        sampleE = l1  

instance
    (ArithUpDn.RoundedReal e, 
     RoundedFieldEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e)) 
    => 
    RoundedPowerToNonnegIntEffort (Interval e)
    where
    type PowerToNonnegIntEffortIndicator (Interval e) =
        IntervalRealEffort e
    powerToNonnegIntDefaultEffort i =
        defaultIntervalRealEffort i

instance
    (ArithUpDn.RoundedReal e, 
     RoundedFieldEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e)) 
    => 
    RoundedPowerToNonnegInt (Interval e)
    where
    powerToNonnegIntInEff effort i@(Interval l r) n =
        case (pNonnegNonposEff effComp l, pNonnegNonposEff effComp r) of
            ((Just True, _), (Just True, _)) -> -- both non-negative
                Interval lPowerUp hPowerDn
            ((_, Just True), (_, Just True)) -> -- both non-positive
                case even n of
                    True -> Interval hNegPowerUp lNegPowerDn -- switching sign!
                    False -> Interval lNegNegPowerUp hNegNegPowerDn
            _ -> -- may involve crossing zero, revert to the default:
                case even n of
                    True -> 
                        NumOrd.maxInEff effOrd z iPowerFromMult 
                        -- take advantage of the fact that the result is non-negative 
                    False -> iPowerFromMult 
        where
        lPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt l n
        hPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt r n
        lNegPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt (neg l) n
        hNegPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt (neg r) n
        lNegNegPowerUp = neg lNegPowerDn
        hNegNegPowerDn = neg hNegPowerUp
        iPowerFromMult = powerToNonnegIntInEffFromMult effort i n 
        z = zero i
        
        effComp = intrealeff_eComp sampleE effort
        effOrd = intrealeff_intordeff sampleE effort
        
        effPowerEndpt = ArithUpDn.fldEffortPow sampleE effField
        effField = ArithUpDn.rrEffortField sampleE effE
        effE = intrealeff_eRoundedReal effort 
        sampleE = l
        
    powerToNonnegIntOutEff effort i@(Interval l r) n =
        case (pNonnegNonposEff effComp l, pNonnegNonposEff effComp r) of
            ((Just True, _), (Just True, _)) -> -- both non-negative
                Interval lPowerDn hPowerUp
            ((_, Just True), (_, Just True)) -> -- both non-positive
                case even n of
                    True -> Interval hNegPowerDn lNegPowerUp -- switching sign!
                    False -> Interval lNegNegPowerDn hNegNegPowerUp
            _ -> -- may involve crossing zero, revert to the default:
                case even n of
                    True -> 
                        NumOrd.maxOutEff effOrd z iPowerFromMult 
                        -- take advantage of the fact that the result is non-negative 
                    False -> iPowerFromMult 
        where
        lPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt l n
        hPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt r n
        lNegPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt (neg l) n
        hNegPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt (neg r) n
        lNegNegPowerDn = neg lNegPowerUp
        hNegNegPowerUp = neg hNegPowerDn
        iPowerFromMult = powerToNonnegIntOutEffFromMult effort i n 
        z = zero i

        effComp = intrealeff_eComp sampleE effort
        effOrd = intrealeff_intordeff sampleE effort
        
        effPowerEndpt = ArithUpDn.fldEffortPow sampleE effField
        effField = ArithUpDn.rrEffortField sampleE effE
        effE = intrealeff_eRoundedReal effort 
        sampleE = l

instance 
    (ArithUpDn.RoundedReal e, 
     RoundedFieldEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e)) 
    =>
    RoundedDivideEffort (Interval e)
    where
    type DivEffortIndicator (Interval e) =
        IntervalRealEffort e
    divDefaultEffort i =
        defaultIntervalRealEffort i 

instance 
    (ArithUpDn.RoundedReal e, 
     RoundedFieldEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e),
     NumOrd.HasExtrema e)
    => 
    RoundedDivide (Interval e)
    where
    divOutEff effort i1@(Interval sampleE _) i2 =
        multOutEff effort i1 $ 
            recipInterval 
                (pPosNonnegNegNonposEff effComp) 
                (ArithUpDn.divDnEff effDiv)
                (ArithUpDn.divUpEff effDiv)
                (RefOrd.bottom i1)
                i2
        where
        effComp = intrealeff_eComp sampleE effort
        
        effDiv = ArithUpDn.fldEffortDiv sampleE effField
        effField = ArithUpDn.rrEffortField sampleE effE
        effE = intrealeff_eRoundedReal effort 
        
    divInEff effort i1@(Interval sampleE _) i2 =
        multInEff effort i1 $ 
            recipInterval 
                (pPosNonnegNegNonposEff effComp) 
                (ArithUpDn.divUpEff effDiv)
                (ArithUpDn.divDnEff effDiv)
                (RefOrd.top i1)
                i2
        where
        effComp = intrealeff_eComp sampleE effort
        
        effDiv = ArithUpDn.fldEffortDiv sampleE effField
        effField = ArithUpDn.rrEffortField sampleE effE
        effE = intrealeff_eRoundedReal effort 



recipInterval :: 
   (NumOrd.HasExtrema e, HasOne e) 
   =>
   (e -> (Maybe Bool, Maybe Bool, Maybe Bool, Maybe Bool))
   -> (e -> e -> e)
   -> (e -> e -> e)
   -> Interval e
   -> Interval e
   -> Interval e
recipInterval pPosNonnegNegNonpos divL divR fallback (Interval l r) =
    case (pPosNonnegNegNonpos l, pPosNonnegNegNonpos r) of
        -- positive:
        ((Just True, _, _, _), (Just True, _, _, _)) ->  
             Interval (divL o r) (divR o l)
        -- negative:
        ((_, _, Just True, _), (_, _, Just True, _)) ->  
             Interval (divL o r) (divR o l)
        -- consistent around zero:
        ((_, _, _, Just True), (_, Just True, _, _)) ->
             (RefOrd.bottom fallback)
        -- anti-consistent around zero:
        ((_, Just True, _, _), (_,_,_, Just True)) ->  
             (RefOrd.top fallback)
        -- unknown:
        _ ->  
             fallback
    where
    o = one sampleE
    sampleE = l 

instance 
    (ArithUpDn.RoundedReal e, 
     RoundedFieldEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e),
     NumOrd.HasExtrema e)
    =>
    RoundedRingEffort (Interval e)
    where
    type RingOpsEffortIndicator (Interval e) =
        IntervalRealEffort e
    ringOpsDefaultEffort i =
        defaultIntervalRealEffort i
    ringEffortAdd (Interval sampleE _) effort =
        ArithUpDn.fldEffortAdd sampleE effortField
        where
        effortField = ArithUpDn.rrEffortField sampleE $ intrealeff_eRoundedReal effort
    ringEffortMult _ effort = effort
    ringEffortPow _ effort = effort

instance 
    (ArithUpDn.RoundedReal e, 
     RoundedFieldEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e),
     NumOrd.HasExtrema e)
    =>
    RoundedRing (Interval e)


instance 
    (ArithUpDn.RoundedReal e, 
     RoundedFieldEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e),
     NumOrd.HasExtrema e)
    =>
    RoundedFieldEffort (Interval e)
    where
    type FieldOpsEffortIndicator (Interval e) =
        IntervalRealEffort e
    fieldOpsDefaultEffort i =
        defaultIntervalRealEffort i
    fldEffortAdd (Interval sampleE _) effort =
        ArithUpDn.fldEffortAdd sampleE effortField
        where
        effortField = ArithUpDn.rrEffortField sampleE $ intrealeff_eRoundedReal effort
    fldEffortMult _ effort = effort
    fldEffortPow _ effort = effort
    fldEffortDiv _  effort = effort
         
        
instance 
    (ArithUpDn.RoundedReal e, 
     RoundedFieldEffort (Distance e),
     RefOrd.RoundedLatticeEffort (Distance e),
     NumOrd.HasExtrema e)
    =>
    RoundedField (Interval e)
        