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

import Numeric.AERN.Basics.Exception
import Control.Exception

roundedImprovingUnit ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    t -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> 
    t -> Bool
roundedImprovingUnit unit =
    equalRoundingUpDnImprovementBin1Var1 (\_ _ e -> e) expr2
    where
    expr2 opEff effort e = 
        unit * e
        where
        (*) = opEff effort

roundedImprovingReflexiveCollapse ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    t -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> 
    t -> Bool
roundedImprovingReflexiveCollapse unit =
    equalRoundingUpDnImprovementBin1Var1 (\_ _ e -> unit) expr2
    where
    expr2 opEff effort e = 
        e * e
        where
        (*) = opEff effort

roundedImprovingCommutative ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> 
    t -> t -> Bool
roundedImprovingCommutative =
    equalRoundingUpDnImprovementBin1Var2 expr1 expr2
    where
    expr1 opEff effort e1 e2 = 
        e1 * e2
        where
        (*) = opEff effort
    expr2 opEff effort e1 e2 = 
        e2 * e1
        where
        (*) = opEff effort

roundedImprovingAssociative ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> 
    t -> t -> t -> Bool
roundedImprovingAssociative =
    equalRoundingUpDnImprovementBin1Var3 expr1 expr2
    where
    expr1 opEff effort e1 e2 e3 = 
        (e1 * e2) * e3
        where
        (*) = opEff effort
    expr2 opEff effort e1 e2 e3 = 
        e1 * (e2 * e3)
        where
        (*) = opEff effort

roundedImprovingNegSymmetric ::
    (Neg t,
     EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (UnaryOpEff eiOp t) -> (UnaryOpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> 
    t -> Bool
roundedImprovingNegSymmetric =
    equalRoundingUpDnImprovementUnary1Var1 expr1 expr2
    where
    expr1 opEff effort e = 
         opEff effort e
    expr2 opEff effort e = 
         opEff effort (neg e) 

roundedImprovingIdempotent ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (UnaryOpEff eiOp t) -> (UnaryOpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> 
    t -> Bool
roundedImprovingIdempotent =
    equalRoundingUpDnImprovementUnary1Var1 expr1 expr2
    where
    expr1 opEff effort e = 
         opEff effort e
    expr2 opEff effort e = 
         opEff effort (opEff effort e)


equalRoundingUpDnImprovementUnary1Var1 :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    (Expr1UnaryOp1Eff eiOp t) -> 
    (Expr1UnaryOp1Eff eiOp t) -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (UnaryOpEff eiOp t) -> (UnaryOpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> t -> Bool
equalRoundingUpDnImprovementUnary1Var1 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort@(initEffortRel, initEffortOp) e =
    equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp (initEffortRel, initEffortOp)
    where
    expr1Up eff = expr1 opUpEff eff e
    expr1Dn eff = expr1 opDnEff eff e
    expr2Up eff = expr2 opUpEff eff e
    expr2Dn eff = expr2 opDnEff eff e

equalRoundingUpDnImprovementBin1Var1 :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    (Expr1Op1Eff eiOp t) -> 
    (Expr1Op1Eff eiOp t) -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> t -> Bool
equalRoundingUpDnImprovementBin1Var1 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort@(initEffortRel, initEffortOp) e =
    equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp (initEffortRel, initEffortOp)
    where
    expr1Up eff = expr1 opUpEff eff e
    expr1Dn eff = expr1 opDnEff eff e
    expr2Up eff = expr2 opUpEff eff e
    expr2Dn eff = expr2 opDnEff eff e

equalRoundingUpDnImprovementBin1Var2 :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    (Expr1Op2Eff eiOp t) -> 
    (Expr1Op2Eff eiOp t) -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> t -> t -> Bool
equalRoundingUpDnImprovementBin1Var2 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort@(initEffortRel, initEffortOp) e1 e2 =
    equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp (initEffortRel, initEffortOp)
    where
    expr1Up eff = expr1 opUpEff eff e1 e2
    expr1Dn eff = expr1 opDnEff eff e1 e2
    expr2Up eff = expr2 opUpEff eff e1 e2
    expr2Dn eff = expr2 opDnEff eff e1 e2

equalRoundingUpDnImprovementBin1Var3 :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    (Expr1Op3Eff eiOp t) -> 
    (Expr1Op3Eff eiOp t) -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> 
    t -> t -> t -> Bool
equalRoundingUpDnImprovementBin1Var3 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort@(initEffortRel, initEffortOp) e1 e2 e3 =
    equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp (initEffortRel, initEffortOp)
    where
    expr1Up eff = expr1 opUpEff eff e1 e2 e3
    expr1Dn eff = expr1 opDnEff eff e1 e2 e3
    expr2Up eff = expr2 opUpEff eff e1 e2 e3
    expr2Dn eff = expr2 opDnEff eff e1 e2 e3

equalRoundingUpDnImprovement :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap) =>
    (eiOp -> t) {-^ left hand side expression UP -} -> 
    (eiOp -> t) {-^ left hand side expression DN -} -> 
    (eiOp -> t) {-^ right hand side expression UP -} -> 
    (eiOp -> t) {-^ right hand side expression DN -} -> 
    (SmdcRelEff eiRel t) -> (t -> t -> gap) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiRel, eiOp) -> Bool
equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp initEffort@(initEffortRel, initEffortOp) =
--    unsafePrint 
--    (
--        "equalRoundingUpDnImprovement:"
--        ++ "\n  efforts = " ++ show efforts
--        ++ "\n  successes = " ++ show successes
--        ++ "\n  imprecisions = " ++ show imprecisions
--    ) 
--    $
    case evalCatchNaNExceptions result of
            Left msg -> True 
            -- ignore tests during which an AERN exception arises 
            --     (typically due to NaN)
            Right res -> res
    where
    result = 
        (and successes) && isImprovement  
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
        -- the following catch does not work, currently have to
        --  catch the exceptions at a higher level 
        case evalCatchNaNExceptions (success, imprecision) of
            Left msg -> (True, plusInfinity)
            Right res -> res 
        where
        success =
            (defined (val1Dn <=? val2Up) ===> (val1Dn <= val2Up))
            &&
            (defined (val2Dn <=? val1Up) ===> (val2Dn <= val1Up))
        imprecision =
            measureGap val1Dn val2Up
        val1Dn = expr1Dn effortOp
        val1Up = expr1Up effortOp
        val2Dn = expr2Dn effortOp
        val2Up = expr2Up effortOp
        (<=) = assumeTotal2 (<=?)
        (<=?) = pCompareEff effortRel

