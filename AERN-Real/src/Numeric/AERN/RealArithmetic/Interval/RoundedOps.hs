{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.RoundedOps
    Description :  refinement rounded basic operations for intervals
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Refinement rounded basic operations for intervals.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.RoundedOps where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.CInterval

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.RoundedOps

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
    
pNonnegNonposEff effort a =
    case NumOrd.pCompareEff effort a zero of
       Just EQ -> Just (True, True) 
       Just LT -> Just (False, True) 
       Just GT -> Just (True, False)
       Just LEE -> Just (False, True) 
       Just GEE -> Just (True, False)
       _ -> Nothing
       
    
multiplyIntervals pNonnegNonpos timesL timesR minL minR maxL maxR combineL combineR (Interval l1 h1) (Interval l2 h2) =
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
            (Just (_, True), Just (_, True), Just (True, _), Just (True, _)) -> 
                (l1 `timesL` h2, h1 `timesR` l2)
            -- i1 negative, i2 negative
            (Just (_, True), Just (_, True), Just (_, True), Just (_, True)) -> 
                (h1 `timesL` h2, l1 `timesR` l2)
            -- i1 negative, i2 consistent and containing zero
            (Just (_, True), Just (_, True), Just (_, True), Just (True, _)) -> 
                (l1 `timesL` h2, l1 `timesR` l2)
            -- i1 negative, i2 anti-consistent and anti-containing zero
            (Just (_, True), Just (_, True), Just (True, _), Just (_, True)) -> 
                (h1 `timesL` h2, h1 `timesR` l2)
            -- i1 negative, nothing known about i2:
            (Just (_, True), Just (_, True), _, _) -> 
                ((h1 `timesL` h2) `combineL` (l1 `timesL` h2), 
                 (h1 `timesR` l2) `combineR` (l1 `timesR` l2))

            -- i1 positive, i2 positive
            (Just (True, _), Just (True, _), Just (True, _), Just (True, _)) -> 
                (l1 `timesL` l2, h1 `timesR` h2)
            -- i1 positive, i2 negative
            (Just (True, _), Just (True, _), Just (_, True), Just (_, True)) -> 
                (h1 `timesL` l2, l1 `timesR` h2)
            -- i1 positive, i2 consistent and containing zero
            (Just (True, _), Just (True, _), Just (_, True), Just (True, _)) -> 
                (h1 `timesL` l2, h1 `timesR` h2)
            -- i1 positive, i2 anti-consistent and anti-containing zero
            (Just (True, _), Just (True, _), Just (True, _), Just (_, True)) -> 
                (l1 `timesL` l2, l1 `timesR` h2)

            -- i1 positive, nothing known about i2:
            (Just (True, _), Just (True, _), _, _) -> 
                ((h1 `timesL` l2) `combineL` (l1 `timesL` l2), 
                 (h1 `timesR` h2) `combineR` (l1 `timesR` h2))
            
 
            -- i1 consistent and containing zero, i2 positive
            (Just (_, True), Just (True, _), Just (True, _), Just (True, _)) -> 
                (l1 `timesL` h2, h1 `timesR` h2)
            -- i1 anti-consistent and anti-containing zero, i2 positive
            (Just (True, _), Just (_, True), Just (True, _), Just (True, _)) -> 
                (l1 `timesL` l2, h1 `timesR` l2)
            -- nothing known about i1, i2 positive
            (_, _, Just (True, _), Just (True, _)) -> 
                ((l1 `timesL` h2) `combineL` (l1 `timesL` l2), 
                 (h1 `timesR` h2) `combineR` (h1 `timesR` l2))

            -- i1 consistent and containing zero, i2 negative
            (Just (_, True), Just (True, _), Just (_, True), Just (_, True)) -> 
                (h1 `timesL` l2, l1 `timesR` l2)
            -- i1 anti-consistent and anti-containing zero, i2 negative
            (Just (True, _), Just (_, True), Just (_, True), Just (_, True)) -> 
                (h1 `timesL` h2, l1 `timesR` h2)
            -- nothing known about i1, i2 negative
            (_, _, Just (_, True), Just (_, True)) -> 
                ((h1 `timesL` h2) `combineL` (h1 `timesL` l2), 
                 (l1 `timesR` h2) `combineR` (l1 `timesR` l2))

            -----------------------------------------------------------
            -- cases where both i1 or i2 are around zero
            -----------------------------------------------------------

            -- i1 consistent and containing zero, i2 consistent and containing zero
            (Just (_, True), Just (True, _), Just (_, True), Just (True, _)) ->
                ((l1 `timesL` h2) `minL` (h1 `timesL` l2), 
                 (l1 `timesR` l2) `maxR` (h1 `timesR` h2))
            -- i1 consistent and containing zero, i2 anti-consistent and anti-containing zero
            (Just (_, True), Just (True, _), Just (True, _), Just (_, True)) ->
                (zero, zero)
            -- i1 consistent and containing zero, i2 unknown
            (Just (_, True), Just (True, _), _, _) ->
                (((l1 `timesL` h2) `combineL` (h1 `timesL` l2)) `combineL` zero,
                 ((l1 `timesR` l2) `combineR` (h1 `timesR` h2)) `combineR` zero)
                
            -- i1 anti-consistent and anti-containing zero, i2 consistent and containing zero
            (Just (True, _), Just (_, True), Just (_, True), Just (True, _)) ->
                (zero, zero)
            -- i1 anti-consistent and anti-containing zero, i2 anti-consistent and anti-containing zero
            (Just (True, _), Just (_, True), Just (True, _), Just (_, True)) ->
                ((l1 `timesL` l2) `maxL` (h1 `timesL` h2),
                 (l1 `timesR` h2) `minR` (h1 `timesR` l2)) 
            -- i1 anti-consistent and anti-containing zero, i2 unknown
            (Just (True, _), Just (_, True), _, _) -> 
                ((l1 `timesL` l2) `combineL` (h1 `timesL` h2) `combineL` zero,
                 (l1 `timesR` h2) `combineR` (h1 `timesR` l2) `combineR` zero) 
                
            -- i1 unknown, i2 anti-consistent and anti-containing zero
            (_, _, Just (True, _), Just (_, True)) -> 
                ((l1 `timesL` l2) `combineL` (h1 `timesL` h2) `combineL` zero,
                 (l1 `timesR` h2) `combineR` (h1 `timesR` l2) `combineR` zero) 

            -- i1 unknown, i2 consistent and containing zero
            (_, _, Just (_, True), Just (True, _)) -> 
                ((l1 `timesL` h2) `combineL` (h1 `timesL` l2) `combineL` zero, 
                 (l1 `timesR` l2) `combineR` (h1 `timesR` h2) `combineR` zero)

            -- both i1 and i2 unknown sign
            _ ->
                (foldl1 combineL [l1 `timesL` h2, h1 `timesL` l2, l1 `timesL` l2, h1 `timesL` h2], 
                 foldl1 combineR [l1 `timesR` h2, h1 `timesR` l2, l1 `timesR` l2, h1 `timesR` h2])

instance 
    (ArithUpDn.RoundedMultiply e, ArithUpDn.RoundedSubtr e,  
     HasZero e, Neg e,
     NumOrd.PartialComparison e, 
     NumOrd.RoundedLattice e) => 
    RoundedRing (Interval e)


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
                (pNonnegNonposEff effortComp) 
                (ArithUpDn.divDnEff effortDiv)
                (ArithUpDn.divUpEff effortDiv)
                RefOrd.bottom
                i2
    divInEff (effortComp, effortMinmax, (effortMult, effortDiv)) i1 i2 =
        multInEff (effortComp, effortMinmax, effortMult) i1 $ 
            recipInterval 
                (pNonnegNonposEff effortComp) 
                (ArithUpDn.divUpEff effortDiv)
                (ArithUpDn.divDnEff effortDiv)
                RefOrd.top
                i2


recipInterval pNonnegNonpos divL divR fallback (Interval l h) =
    case (pNonnegNonpos l, pNonnegNonpos h) of
        -- non-negative:
        (Just (True, _), Just (True, _)) ->  
             Interval (divL one h) (divR one l)
        -- non-positive:
        (Just (_, True), Just (_, True)) ->  
             Interval (divL one h) (divR one l)
        -- consistent around zero:
        (Just (_, True), Just (True, _)) ->
             RefOrd.bottom
        -- anti-consistent around zero:
        (Just (True, _), Just (_, True)) ->  
             RefOrd.top
        -- unknown:
        _ ->  
             fallback

            