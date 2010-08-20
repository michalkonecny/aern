{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.FieldOps
    Description :  refinement rounded basic operations for intervals
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Refinement rounded basic operations for intervals.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.FieldOps where

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.CInterval

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd


instance (ArithUpDn.RoundedAdd e) => RoundedAdd (Interval e) where
    type AddEffortIndicator (Interval e) = ArithUpDn.AddEffortIndicator e
    addDefaultEffort (Interval l h) = ArithUpDn.addDefaultEffort l
    addInEff effort (Interval l1 h1) (Interval l2 h2) =
        Interval 
            (ArithUpDn.addUpEff effort l1 l2)
            (ArithUpDn.addDnEff effort h1 h2)
    addOutEff effort (Interval l1 h1) (Interval l2 h2) =
        Interval 
            (ArithUpDn.addDnEff effort l1 l2)
            (ArithUpDn.addUpEff effort h1 h2)

instance (ArithUpDn.RoundedAdd e, Neg e) => RoundedSubtr (Interval e)

instance 
    (ArithUpDn.RoundedAbs e,  
     HasZero e, Neg e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedAbs (Interval e)
    where
    type AbsEffortIndicator (Interval e) = 
        (NumOrd.PartialCompareEffortIndicator e, NumOrd.MinmaxEffortIndicator e)
    absDefaultEffort (Interval l h) = 
        (NumOrd.pCompareDefaultEffort l, NumOrd.minmaxDefaultEffort l) 
    absOutEff = absOutUsingCompMax
    absInEff = absInUsingCompMax


instance 
    (ArithUpDn.RoundedMultiply e,  
     HasZero e, Neg e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedMultiply (Interval e)
    where
    type MultEffortIndicator (Interval e) = 
        (NumOrd.PartialCompareEffortIndicator e, 
         NumOrd.MinmaxEffortIndicator e,
         ArithUpDn.MultEffortIndicator e)
    multDefaultEffort (Interval l h) = 
        (NumOrd.pCompareDefaultEffort l, 
         NumOrd.minmaxDefaultEffort l,
         ArithUpDn.multDefaultEffort l) 
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
        (Interval l1 h1) (Interval l2 h2) =
    let _ = [minL, maxR, combineL, combineR] in
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
                (l1 `timesL` h2, h1 `timesR` l2)
            -- i1 negative, i2 negative
            ((_, Just True), (_, Just True), (_, Just True), (_, Just True)) -> 
                (h1 `timesL` h2, l1 `timesR` l2)
            -- i1 negative, i2 consistent and containing zero
            ((_, Just True), (_, Just True), (_, Just True), (Just True, _)) -> 
                (l1 `timesL` h2, l1 `timesR` l2)
            -- i1 negative, i2 anti-consistent and anti-containing zero
            ((_, Just True), (_, Just True), (Just True, _), (_, Just True)) -> 
                (h1 `timesL` h2, h1 `timesR` l2)
            -- i1 negative, nothing known about i2:
            ((_, Just True), (_, Just True), _, _) -> 
                ((h1 `timesL` h2) `combineL` (l1 `timesL` h2), 
                 (h1 `timesR` l2) `combineR` (l1 `timesR` l2))

            -- i1 positive, i2 positive
            ((Just True, _), (Just True, _), (Just True, _), (Just True, _)) -> 
                (l1 `timesL` l2, h1 `timesR` h2)
            -- i1 positive, i2 negative
            ((Just True, _), (Just True, _), (_, Just True), (_, Just True)) -> 
                (h1 `timesL` l2, l1 `timesR` h2)
            -- i1 positive, i2 consistent and containing zero
            ((Just True, _), (Just True, _), (_, Just True), (Just True, _)) -> 
                (h1 `timesL` l2, h1 `timesR` h2)
            -- i1 positive, i2 anti-consistent and anti-containing zero
            ((Just True, _), (Just True, _), (Just True, _), (_, Just True)) -> 
                (l1 `timesL` l2, l1 `timesR` h2)

            -- i1 positive, nothing known about i2:
            ((Just True, _), (Just True, _), _, _) -> 
                ((h1 `timesL` l2) `combineL` (l1 `timesL` l2), 
                 (h1 `timesR` h2) `combineR` (l1 `timesR` h2))
            
 
            -- i1 consistent and containing zero, i2 positive
            ((_, Just True), (Just True, _), (Just True, _), (Just True, _)) -> 
                (l1 `timesL` h2, h1 `timesR` h2)
            -- i1 anti-consistent and anti-containing zero, i2 positive
            ((Just True, _), (_, Just True), (Just True, _), (Just True, _)) -> 
                (l1 `timesL` l2, h1 `timesR` l2)
            -- nothing known about i1, i2 positive
            (_, _, (Just True, _), (Just True, _)) -> 
                ((l1 `timesL` h2) `combineL` (l1 `timesL` l2), 
                 (h1 `timesR` h2) `combineR` (h1 `timesR` l2))

            -- i1 consistent and containing zero, i2 negative
            ((_, Just True), (Just True, _), (_, Just True), (_, Just True)) -> 
                (h1 `timesL` l2, l1 `timesR` l2)
            -- i1 anti-consistent and anti-containing zero, i2 negative
            ((Just True, _), (_, Just True), (_, Just True), (_, Just True)) -> 
                (h1 `timesL` h2, l1 `timesR` h2)
            -- nothing known about i1, i2 negative
            (_, _, (_, Just True), (_, Just True)) -> 
                ((h1 `timesL` h2) `combineL` (h1 `timesL` l2), 
                 (l1 `timesR` h2) `combineR` (l1 `timesR` l2))

            -----------------------------------------------------------
            -- cases where both i1 or i2 are around zero
            -----------------------------------------------------------

            -- i1 consistent and containing zero, i2 consistent and containing zero
            ((_, Just True), (Just True, _), (_, Just True), (Just True, _)) ->
                ((l1 `timesL` h2) `minL` (h1 `timesL` l2), 
                 (l1 `timesR` l2) `maxR` (h1 `timesR` h2))
            -- i1 consistent and containing zero, i2 anti-consistent and anti-containing zero
            ((_, Just True), (Just True, _), (Just True, _), (_, Just True)) ->
                (zero, zero)
            -- i1 consistent and containing zero, i2 unknown
            ((_, Just True), (Just True, _), _, _) ->
                (((l1 `timesL` h2) `combineL` (h1 `timesL` l2)) `combineL` zero,
                 ((l1 `timesR` l2) `combineR` (h1 `timesR` h2)) `combineR` zero)
                
            -- i1 anti-consistent and anti-containing zero, i2 consistent and containing zero
            ((Just True, _), (_, Just True), (_, Just True), (Just True, _)) ->
                (zero, zero)
            -- i1 anti-consistent and anti-containing zero, i2 anti-consistent and anti-containing zero
            ((Just True, _), (_, Just True), (Just True, _), (_, Just True)) ->
                ((l1 `timesL` l2) `maxL` (h1 `timesL` h2),
                 (l1 `timesR` h2) `minR` (h1 `timesR` l2)) 
            -- i1 anti-consistent and anti-containing zero, i2 unknown
            ((Just True, _), (_, Just True), _, _) -> 
                ((l1 `timesL` l2) `combineL` (h1 `timesL` h2) `combineL` zero,
                 (l1 `timesR` h2) `combineR` (h1 `timesR` l2) `combineR` zero) 
                
            -- i1 unknown, i2 anti-consistent and anti-containing zero
            (_, _, (Just True, _), (_, Just True)) -> 
                ((l1 `timesL` l2) `combineL` (h1 `timesL` h2) `combineL` zero,
                 (l1 `timesR` h2) `combineR` (h1 `timesR` l2) `combineR` zero) 

            -- i1 unknown, i2 consistent and containing zero
            (_, _, (_, Just True), (Just True, _)) -> 
                ((l1 `timesL` h2) `combineL` (h1 `timesL` l2) `combineL` zero, 
                 (l1 `timesR` l2) `combineR` (h1 `timesR` h2) `combineR` zero)

            -- both i1 and i2 unknown sign
            _ ->
                (foldl1 combineL [l1 `timesL` h2, h1 `timesL` l2, l1 `timesL` l2, h1 `timesL` h2], 
                 foldl1 combineR [l1 `timesR` h2, h1 `timesR` l2, l1 `timesR` l2, h1 `timesR` h2])

instance 
    (ArithUpDn.RoundedRing e,
     ArithUpDn.RoundedPowerNonnegToNonnegInt e,
     HasOne e, HasZero e, Neg e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e) => 
    RoundedRing (Interval e)

instance
    (ArithUpDn.RoundedPowerNonnegToNonnegInt e,
     ArithUpDn.RoundedMultiply e,
     HasZero e, HasOne e, Neg e,
     NumOrd.PartialComparison e, NumOrd.RoundedLattice e
     ) => 
    RoundedPowerToNonnegInt (Interval e)
    where
    type PowerToNonnegIntEffortIndicator (Interval e) =
        (ArithUpDn.PowerNonnegToNonnegIntEffortIndicator e,
         NumOrd.PartialCompareEffortIndicator e,
         PowerToNonnegIntEffortIndicatorFromMult (Interval e))
    powerToNonnegIntDefaultEffort i@(Interval l h) =
        (ArithUpDn.powerNonnegToNonnegIntDefaultEffort l,
         NumOrd.pCompareDefaultEffort l,
         powerToNonnegIntDefaultEffortFromMult i) 
    powerToNonnegIntInEff 
            (effPowerEndpt, effComp, effPowerFromMult@(_,effMinMax,_)) 
            i@(Interval l h) n =
        case (pNonnegNonposEff effComp l, pNonnegNonposEff effComp h) of
            ((Just True, _), (Just True, _)) -> -- both non-negative
                Interval lPowerUp hPowerDn
            ((_, Just True), (_, Just True)) -> -- both non-positive
                case even n of
                    True -> Interval hNegPowerUp lNegPowerDn -- switching sign!
                    False -> Interval lNegNegPowerUp hNegNegPowerDn
            _ -> -- may involve crossing zero, revert to the default:
                case even n of
                    True -> 
                        NumOrd.maxInnerEff effMinMax zero iPowerFromMult 
                        -- take advantage of the fact that the result is non-negative 
                    False -> iPowerFromMult 
        where
        lPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt l n
        hPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt h n
        lNegPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt (neg l) n
        hNegPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt (neg h) n
        lNegNegPowerUp = neg lNegPowerDn
        hNegNegPowerDn = neg hNegPowerUp
        iPowerFromMult = powerToNonnegIntInEffFromMult effPowerFromMult i n 
    powerToNonnegIntOutEff 
            (effPowerEndpt, effComp, effPowerFromMult@(_,effMinMax,_)) 
            i@(Interval l h) n =
        case (pNonnegNonposEff effComp l, pNonnegNonposEff effComp h) of
            ((Just True, _), (Just True, _)) -> -- both non-negative
                Interval lPowerDn hPowerUp
            ((_, Just True), (_, Just True)) -> -- both non-positive
                case even n of
                    True -> Interval hNegPowerDn lNegPowerUp -- switching sign!
                    False -> Interval lNegNegPowerDn hNegNegPowerUp
            _ -> -- may involve crossing zero, revert to the default:
                case even n of
                    True -> 
                        NumOrd.maxOuterEff effMinMax zero iPowerFromMult 
                        -- take advantage of the fact that the result is non-negative 
                    False -> iPowerFromMult 
        where
        lPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt l n
        hPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt h n
        lNegPowerUp = ArithUpDn.powerNonnegToNonnegIntUpEff effPowerEndpt (neg l) n
        hNegPowerDn = ArithUpDn.powerNonnegToNonnegIntDnEff effPowerEndpt (neg h) n
        lNegNegPowerDn = neg lNegPowerUp
        hNegNegPowerUp = neg hNegPowerDn
        iPowerFromMult = powerToNonnegIntOutEffFromMult effPowerFromMult i n 

instance 
    (ArithUpDn.RoundedMultiply e, ArithUpDn.RoundedDivide e,  
     HasZero e, Neg e, HasOne e, NumOrd.HasExtrema e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedDivide (Interval e)
    where
    type DivEffortIndicator (Interval e) = 
        (NumOrd.PartialCompareEffortIndicator e, 
         NumOrd.MinmaxEffortIndicator e,
         (ArithUpDn.MultEffortIndicator e,
          ArithUpDn.DivEffortIndicator e))
    divDefaultEffort (Interval l h) = 
        (NumOrd.pCompareDefaultEffort l, 
         NumOrd.minmaxDefaultEffort l,
         (ArithUpDn.multDefaultEffort l,
          ArithUpDn.divDefaultEffort l)) 
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


recipInterval pPosNonnegNegNonpos divL divR fallback (Interval l h) =
    case (pPosNonnegNegNonpos l, pPosNonnegNegNonpos h) of
        -- positive:
        ((Just True, _, _, _), (Just True, _, _, _)) ->  
             Interval (divL one h) (divR one l)
        -- negative:
        ((_, _, Just True, _), (_, _, Just True, _)) ->  
             Interval (divL one h) (divR one l)
        -- consistent around zero:
        ((_, _, _, Just True), (_, Just True, _, _)) ->
             RefOrd.bottom
        -- anti-consistent around zero:
        ((_, Just True, _, _), (_,_,_, Just True)) ->  
             RefOrd.top
        -- unknown:
        _ ->  
             fallback

instance 
    (ArithUpDn.RoundedField e,
     ArithUpDn.RoundedPowerNonnegToNonnegInt e,
     HasZero e, Neg e, HasOne e, 
     NumOrd.HasExtrema e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedField (Interval e)
    where
    type FieldOpsEffortIndicator (Interval e) =
        (ArithUpDn.FieldOpsEffortIndicator e,
         NumOrd.PartialCompareEffortIndicator e,
         NumOrd.MinmaxEffortIndicator e)
    fieldOpsDefaultEffort (Interval l h) =
        (ArithUpDn.fieldOpsDefaultEffort l,
         NumOrd.pCompareDefaultEffort l,
         NumOrd.minmaxDefaultEffort l)
    fldEffortAdd (Interval l h) (effortField, effortComp, effortMinmax) =
        ArithUpDn.fldEffortAdd l effortField
    fldEffortMult (Interval l h) (effortField, effortComp, effortMinmax) =
        (effortComp, effortMinmax, 
         ArithUpDn.fldEffortMult l effortField)
    fldEffortPow i@(Interval l h) e@(effortField, effortComp, effortMinmax) =
        (ArithUpDn.fldEffortPow l effortField,
         effortComp,
         fldEffortMult i e) 
    fldEffortDiv (Interval l h) (effortField, effortComp, effortMinmax) =
        (effortComp, effortMinmax, 
         (ArithUpDn.fldEffortMult l effortField,
          ArithUpDn.fldEffortDiv l effortField))
         
        