{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Basics.Laws.Relation
    Description :  common properties of arithmetic operations arbitrarily-little rounded  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Common properties of arithmetic operations when these operations
    are rounded but the rounding can be diminished arbitrarily 
    by increasing an effort indicator.
-}

module Numeric.AERN.RealArithmetic.Laws where

import Numeric.AERN.RealArithmetic.Measures

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Laws.Utilities

import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Misc.Bool
import Data.Maybe

import Numeric.AERN.RealArithmetic.ExactOperations
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Numerals
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd


roundedImprovingUnit ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     NumOrd.PartialComparison gap, HasZero gap) =>
    t -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> t -> Bool
roundedImprovingUnit unit =
    equalRoundingUpDnImprovement11 (\_ _ e -> e) expr2
    where
    expr2 opEff effort e = 
        unit * e
        where
        (*) = opEff effort

equalRoundingUpDnImprovement11 :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     NumOrd.PartialComparison gap, HasZero gap) =>
    (Expr1Op1Eff eiOp t) -> (Expr1Op1Eff eiOp t) -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> t -> Bool
equalRoundingUpDnImprovement11 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort@(initEffortRel, initEffortOp) e =
    and successes && isImprovement
    where
    imprecision0Zero = 
        (imprecision0 NumOrd.==? zero) == Just True
        where
        ?pCompareEffort = effortImprComp
    isImprovement = 
        or $ null efforts : imprecision0Zero : -- either perfect or can be improved: 
                (catMaybes $ map (imprecision0 NumOrd.>?) imprecisions)
        where
        ?pCompareEffort = effortImprComp
    (successes, imprecision0 : imprecisions) = unzip $ map check efforts
    efforts =
        map (\i -> (initEffortRel, i)) $
            (initEffortOp : ) $ 
                concat $
                    map (take 5 . effortIncrementSequence) $ 
                        effortIncrementVariants initEffortOp 
    check (effortRel, effortOp) =
        (success, imprecision)
        where
        success =
            (defined (val1Dn <=? val2Up) ===> (val1Dn <= val2Up))
            &&
            (defined (val2Dn <=? val1Up) ===> (val2Dn <= val1Up))
        imprecision =
            measureGap val1Dn val2Up
        val1Dn = expr1 opDnEff effortOp e
        val1Up = expr1 opUpEff effortOp e
        val2Dn = expr2 opDnEff effortOp e
        val2Up = expr2 opUpEff effortOp e
        (<=) = assumeTotal2 (<=?)
        (<=?) = pCompareEff effortRel

