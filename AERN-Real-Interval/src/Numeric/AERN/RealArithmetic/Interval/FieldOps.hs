{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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
()
where

import Numeric.AERN.Basics.Interval

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd


instance (ArithUpDn.RoundedAddEffort e) => RoundedAddEffort (Interval e) where
    type AddEffortIndicator (Interval e) = ArithUpDn.AddEffortIndicator e
    addDefaultEffort (Interval l r) = ArithUpDn.addDefaultEffort l

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
        (NumOrd.PartialCompareEffortIndicator e, NumOrd.MinmaxEffortIndicator e)
    absDefaultEffort (Interval l r) = 
        (NumOrd.pCompareDefaultEffort l, NumOrd.minmaxDefaultEffort l) 

instance 
    (ArithUpDn.RoundedAbs e,  
     HasZero e, Neg e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedAbs (Interval e)
    where
    absOutEff = absOutUsingCompMax
    absInEff = absInUsingCompMax


instance 
    (ArithUpDn.RoundedMultiplyEffort e,  
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeEffort e) => 
    RoundedMultiplyEffort (Interval e)
    where
    type MultEffortIndicator (Interval e) = 
        (NumOrd.PartialCompareEffortIndicator e, 
         NumOrd.MinmaxEffortIndicator e,
         ArithUpDn.MultEffortIndicator e)
    multDefaultEffort (Interval l r) = 
        (NumOrd.pCompareDefaultEffort l, 
         NumOrd.minmaxDefaultEffort l,
         ArithUpDn.multDefaultEffort l) 

instance 
    (ArithUpDn.RoundedMultiply e,  
     HasZero e, Neg e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedMultiply (Interval e)
    where
    multOutEff (effortComp, effortMinmax, effortMult) i1 i2 =
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
            i1 i2
    multInEff (effortComp, effortMinmax, effortMult) i1 i2 =
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
            i1 i2
    
multiplyIntervals
        pNonnegNonpos timesL timesR minL minR maxL maxR 
        combineL combineR 
        (Interval l1 r1) (Interval l2 r2) =
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
    (ArithUpDn.RoundedPowerNonnegToNonnegIntEffort e,
     ArithUpDn.RoundedMultiplyEffort e,
     NumOrd.PartialComparison e, NumOrd.RoundedLatticeEffort e
     ) => 
    RoundedPowerToNonnegIntEffort (Interval e)
    where
    type PowerToNonnegIntEffortIndicator (Interval e) =
        (ArithUpDn.PowerNonnegToNonnegIntEffortIndicator e,
         NumOrd.PartialCompareEffortIndicator e,
         PowerToNonnegIntEffortIndicatorFromMult (Interval e))
    powerToNonnegIntDefaultEffort i@(Interval l r) =
        (ArithUpDn.powerNonnegToNonnegIntDefaultEffort l,
         NumOrd.pCompareDefaultEffort l,
         powerToNonnegIntDefaultEffortFromMult i) 

instance
    (ArithUpDn.RoundedPowerNonnegToNonnegInt e,
     ArithUpDn.RoundedMultiply e,
     HasZero e, HasOne e, Neg e,
     NumOrd.PartialComparison e, NumOrd.RoundedLattice e
     ) => 
    RoundedPowerToNonnegInt (Interval e)
    where
    powerToNonnegIntInEff 
            (effPowerEndpt, effComp, effPowerFromMult@(_,effMinMax,_)) 
            i@(Interval l r) n =
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
                        NumOrd.maxInEff effMinMax z iPowerFromMult 
                        -- take advantage of the fact that the result is non-negative 
                    False -> iPowerFromMult 
        where
        lPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt l n
        hPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt r n
        lNegPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt (neg l) n
        hNegPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt (neg r) n
        lNegNegPowerUp = neg lNegPowerDn
        hNegNegPowerDn = neg hNegPowerUp
        iPowerFromMult = powerToNonnegIntInEffFromMult effPowerFromMult i n 
        z = zero i
    powerToNonnegIntOutEff 
            (effPowerEndpt, effComp, effPowerFromMult@(_,effMinMax,_)) 
            i@(Interval l r) n =
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
                        NumOrd.maxOutEff effMinMax z iPowerFromMult 
                        -- take advantage of the fact that the result is non-negative 
                    False -> iPowerFromMult 
        where
        lPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt l n
        hPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt r n
        lNegPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt (neg l) n
        hNegPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt (neg r) n
        lNegNegPowerDn = neg lNegPowerUp
        hNegNegPowerUp = neg hNegPowerDn
        iPowerFromMult = powerToNonnegIntOutEffFromMult effPowerFromMult i n 
        z = zero i

instance 
    (ArithUpDn.RoundedMultiplyEffort e, ArithUpDn.RoundedDivideEffort e,  
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeEffort e) => 
    RoundedDivideEffort (Interval e)
    where
    type DivEffortIndicator (Interval e) = 
        (NumOrd.PartialCompareEffortIndicator e, 
         NumOrd.MinmaxEffortIndicator e,
         (ArithUpDn.MultEffortIndicator e,
          ArithUpDn.DivEffortIndicator e))
    divDefaultEffort (Interval l r) = 
        (NumOrd.pCompareDefaultEffort l, 
         NumOrd.minmaxDefaultEffort l,
         (ArithUpDn.multDefaultEffort l,
          ArithUpDn.divDefaultEffort l)) 

instance 
    (ArithUpDn.RoundedMultiply e, ArithUpDn.RoundedDivide e,  
     HasZero e, Neg e, HasOne e, NumOrd.HasExtrema e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedDivide (Interval e)
    where
    divOutEff (effortComp, effortMinmax, (effortMult, effortDiv)) i1 i2 =
        multOutEff (effortComp, effortMinmax, effortMult) i1 $ 
            recipInterval 
                (pPosNonnegNegNonposEff effortComp) 
                (ArithUpDn.divDnEff effortDiv)
                (ArithUpDn.divUpEff effortDiv)
                RefOrd.bottom
                i2
    divInEff (effortComp, effortMinmax, (effortMult, effortDiv)) i1 i2 =
        multInEff (effortComp, effortMinmax, effortMult) i1 $ 
            recipInterval 
                (pPosNonnegNegNonposEff effortComp) 
                (ArithUpDn.divUpEff effortDiv)
                (ArithUpDn.divDnEff effortDiv)
                RefOrd.top
                i2


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
             RefOrd.bottom
        -- anti-consistent around zero:
        ((_, Just True, _, _), (_,_,_, Just True)) ->  
             RefOrd.top
        -- unknown:
        _ ->  
             fallback
    where
    o = one sampleE
    sampleE = l 

instance 
    (ArithUpDn.RoundedRingEffort e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeEffort e) => 
    RoundedRingEffort (Interval e)
    where
    type RingOpsEffortIndicator (Interval e) =
        (ArithUpDn.RingOpsEffortIndicator e,
         NumOrd.PartialCompareEffortIndicator e,
         NumOrd.MinmaxEffortIndicator e)
    ringOpsDefaultEffort (Interval l r) =
        (ArithUpDn.ringOpsDefaultEffort l,
         NumOrd.pCompareDefaultEffort l,
         NumOrd.minmaxDefaultEffort l)
    ringEffortAdd (Interval l r) (effortRing, effortComp, effortMinmax) =
        ArithUpDn.ringEffortAdd l effortRing
    ringEffortMult (Interval l r) (effortRing, effortComp, effortMinmax) =
        (effortComp, effortMinmax, 
         ArithUpDn.ringEffortMult l effortRing)
    ringEffortPow i@(Interval l r) e@(effortRing, effortComp, effortMinmax) =
        (ArithUpDn.ringEffortPow l effortRing,
         effortComp,
         ringEffortMult i e) 

instance 
    (ArithUpDn.RoundedRing e,
     ArithUpDn.RoundedPowerNonnegToNonnegInt e,
     HasOne e, HasZero e, Neg e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e) => 
    RoundedRing (Interval e)


instance 
    (ArithUpDn.RoundedFieldEffort e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLatticeEffort e) => 
    RoundedFieldEffort (Interval e)
    where
    type FieldOpsEffortIndicator (Interval e) =
        (ArithUpDn.FieldOpsEffortIndicator e,
         NumOrd.PartialCompareEffortIndicator e,
         NumOrd.MinmaxEffortIndicator e)
    fieldOpsDefaultEffort (Interval l r) =
        (ArithUpDn.fieldOpsDefaultEffort l,
         NumOrd.pCompareDefaultEffort l,
         NumOrd.minmaxDefaultEffort l)
    fldEffortAdd (Interval l r) (effortField, effortComp, effortMinmax) =
        ArithUpDn.fldEffortAdd l effortField
    fldEffortMult (Interval l r) (effortField, effortComp, effortMinmax) =
        (effortComp, effortMinmax, 
         ArithUpDn.fldEffortMult l effortField)
    fldEffortPow i@(Interval l r) e@(effortField, effortComp, effortMinmax) =
        (ArithUpDn.fldEffortPow l effortField,
         effortComp,
         fldEffortMult i e) 
    fldEffortDiv (Interval l r) (effortField, effortComp, effortMinmax) =
        (effortComp, effortMinmax, 
         (ArithUpDn.fldEffortMult l effortField,
          ArithUpDn.fldEffortDiv l effortField))
         
        
instance 
    (ArithUpDn.RoundedField e,
     ArithUpDn.RoundedPowerNonnegToNonnegInt e,
     HasZero e, Neg e, HasOne e, 
     NumOrd.HasExtrema e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedField (Interval e)
        