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
import Numeric.AERN.Misc.Debug
import Data.Maybe

import Numeric.AERN.RealArithmetic.ExactOperations
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Numerals
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd


roundedImprovingUnit ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
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

roundedImprovingCommutative ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap) =>
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> t -> t -> Bool
roundedImprovingCommutative =
    equalRoundingUpDnImprovement12 expr1 expr2
    where
    expr1 opEff effort e1 e2 = 
        e1 * e2
        where
        (*) = opEff effort
    expr2 opEff effort e1 e2 = 
        e2 * e1
        where
        (*) = opEff effort

equalRoundingUpDnImprovement11 :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap) =>
    (Expr1Op1Eff eiOp t) -> 
    (Expr1Op1Eff eiOp t) -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> t -> Bool
equalRoundingUpDnImprovement11 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort@(initEffortRel, initEffortOp) e =
    equalRoundingUpDnImprovement1N expr11 expr21 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp (initEffortRel, initEffortOp)
    where
    expr11 op eff = expr1 op eff e
    expr21 op eff = expr2 op eff e

equalRoundingUpDnImprovement12 :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap) =>
    (Expr1Op2Eff eiOp t) -> 
    (Expr1Op2Eff eiOp t) -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> t -> t -> Bool
equalRoundingUpDnImprovement12 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort@(initEffortRel, initEffortOp) e1 e2 =
    equalRoundingUpDnImprovement1N expr12 expr22 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp (initEffortRel, initEffortOp)
    where
    expr12 op eff = expr1 op eff e1 e2
    expr22 op eff = expr2 op eff e1 e2

equalRoundingUpDnImprovement13 :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap) =>
    (Expr1Op3Eff eiOp t) -> 
    (Expr1Op3Eff eiOp t) -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> 
    t -> t -> t -> Bool
equalRoundingUpDnImprovement13 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort@(initEffortRel, initEffortOp) e1 e2 e3 =
    equalRoundingUpDnImprovement1N expr13 expr23 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp (initEffortRel, initEffortOp)
    where
    expr13 op eff = expr1 op eff e1 e2 e3
    expr23 op eff = expr2 op eff e1 e2 e3

equalRoundingUpDnImprovement1N :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap) =>
    (OpEff eiOp t -> eiOp -> t) {-^ left hand side expression -} -> 
    (OpEff eiOp t -> eiOp -> t) {-^ right hand side expression -} -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> Bool
equalRoundingUpDnImprovement1N expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort@(initEffortRel, initEffortOp) =
--    unsafePrint 
--    (
--        "equalRoundingUpDnImprovement1N:"
--        ++ "\n  efforts = " ++ show efforts
--        ++ "\n  successes = " ++ show successes
--    ) 
--    $
    and successes && isImprovement
    where
    imprecision0Zero = 
        (imprecision0 NumOrd.==? zero) == Just True
        where
        ?pCompareEffort = effortImprComp
    isImprovement = 
        or $ null imprecisions : -- no way to raise effort  
              imprecision0Zero : -- or it is exact
                (catMaybes $ map (imprecision0 NumOrd.>?) imprecisions)  -- or it can be improved
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
        val1Dn = expr1 opDnEff effortOp
        val1Up = expr1 opUpEff effortOp
        val2Dn = expr2 opDnEff effortOp
        val2Up = expr2 opUpEff effortOp
        (<=) = assumeTotal2 (<=?)
        (<=?) = pCompareEff effortRel

