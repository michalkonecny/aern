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
import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Laws.Utilities

import Numeric.AERN.Misc.Bool
import Numeric.AERN.Misc.Debug
import Numeric.AERN.Misc.List
import Numeric.AERN.Misc.Maybe
import Data.Maybe

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Conversion
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort

import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Exception
import Control.Exception

roundedRefinementMonotone1 ::
    (RefOrd.PartialComparison t, RefOrd.ArbitraryOrderedTuple t) =>
    (Expr1Eff ei t) ->
    (Expr1Eff ei t) ->
    ei -> (RefOrd.LEPair t) -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
roundedRefinementMonotone1 exprUp exprDn effort (RefOrd.LEPair (e1L, e1H)) effortComp =
    case RefOrd.pLeqEff effortComp resDn resUp of
        Just b -> b
        _ -> True
    where
    resUp = exprUp effort e1H
    resDn = exprDn effort e1L

roundedRefinementMonotone2 ::
    (RefOrd.PartialComparison t, RefOrd.ArbitraryOrderedTuple t, Show t) =>
    (Expr2Eff ei t) ->
    (Expr2Eff ei t) ->
    ei -> (RefOrd.LEPair t) -> (RefOrd.LEPair t) -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
roundedRefinementMonotone2 exprUp exprDn effort (RefOrd.LEPair (e1L, e1H)) (RefOrd.LEPair (e2L, e2H)) effortComp =
--    unsafePrint ("\nroundedRefinementMonotone2: " 
--      ++ "\n Up: op(" ++ show e1H ++ ", " ++ show e2H ++ ") = " ++ show resUp 
--      ++ "\n Dn: op(" ++ show e1L ++ ", " ++ show e2L ++ ") = " ++ show resDn
--      ++ "\n" 
--    ) $
    case RefOrd.pLeqEff effortComp resDn resUp of
        Just b -> b
        _ -> True
    where
    resUp = exprUp effort e1H e2H
    resDn = exprDn effort e1L e2L


roundedImprovingUnit ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    t -> 
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> 
    t -> Bool
roundedImprovingUnit unit =
    equalRoundingUpDnImprovementBin1Var1 (\_ _ e -> e) expr2
    where
    expr2 opEff effort e = 
        unit * e
        where
        (*) = opEff effort

roundedImprovingReflexiveCollapse ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    t -> 
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> 
    t -> Bool
roundedImprovingReflexiveCollapse unit =
    equalRoundingUpDnImprovementBin1Var1 (\_ _ e -> unit) expr2
    where
    expr2 opEff effort e = 
        e * e
        where
        (*) = opEff effort

roundedImprovingCommutative ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> 
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
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> 
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

roundedImprovingDistributive ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp1, Show eiOp1, 
     EffortIndicator eiOp2, Show eiOp2,
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, 
     HasAntiConsistency t, Show t) =>
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (ConsistencyEffortIndicator t) ->
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, (eiOp1, eiOp2)) -> 
    t -> t -> t -> Bool
roundedImprovingDistributive 
        pCompareEff measureGap 
        op1UpEff op2UpEff 
        op1DnEff op2DnEff 
        effortConsistency
        effortImprComp initEffort 
        e1 e2 e3 =
--    unsafePrint
--    (
--        "property roundedImprovingDistributive: "
--        ++ "\n e1 = " ++ show e1
--        ++ "\n e2 = " ++ show e2
--        ++ "\n e3 = " ++ show e3
--    ) $        
    thinEqualConsLeqRoundingUpDnImprovement 
        -- cannot get equality when e1 is not thin 
        -- because e1 appears twice in expr1 (dependency error)
        [e1] expr1Up expr1Dn expr2Up expr2Dn 
        pCompareEff measureGap
        effortConsistency 
        effortImprComp initEffort
    where
    expr1Up eff = expr1 op1UpEff op2UpEff eff e1 e2 e3
    expr1Dn eff = expr1 op1DnEff op2DnEff eff e1 e2 e3
    expr2Up eff = expr2 op1UpEff op2UpEff eff e1 e2 e3
    expr2Dn eff = expr2 op1DnEff op2DnEff eff e1 e2 e3
    expr1 op1Eff op2Eff (effort1, effort2) e1 e2 e3 = 
        (e1 * e2) + (e1 * e3)
        where
        (*) = op1Eff effort1
        (+) = op2Eff effort2
    expr2 op1Eff op2Eff (effort1, effort2) e1 e2 e3 = 
        e1 * (e2 + e3)
        where
        (*) = op1Eff effort1
        (+) = op2Eff effort2


roundedImprovingNegSymmetric ::
    (Neg t,
     EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (UnaryOpEff eiOp t) -> (UnaryOpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> 
    t -> Bool
roundedImprovingNegSymmetric =
    equalRoundingUpDnImprovementUnary1Var1 expr1 expr2
    where
    expr1 opEff effort e = 
         opEff effort e
    expr2 opEff effort e = 
         opEff effort (neg e) 

roundedImprovingIdempotent ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (UnaryOpEff eiOp t) -> (UnaryOpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> 
    t -> Bool
roundedImprovingIdempotent =
    equalRoundingUpDnImprovementUnary1Var1 expr1 expr2
    where
    expr1 opEff effort e = 
         opEff effort e
    expr2 opEff effort e = 
         opEff effort (opEff effort e)



equalRoundingUpDnImprovementUnary1Var1 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (Expr1UnaryOp1Eff eiOp t) -> 
    (Expr1UnaryOp1Eff eiOp t) -> 
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (UnaryOpEff eiOp t) -> (UnaryOpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> 
    t -> Bool
equalRoundingUpDnImprovementUnary1Var1 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort e =
    equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp initEffort
    where
    expr1Up eff = expr1 opUpEff eff e
    expr1Dn eff = expr1 opDnEff eff e
    expr2Up eff = expr2 opUpEff eff e
    expr2Dn eff = expr2 opDnEff eff e

equalRoundingUpDnImprovementBin1Var1 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (Expr1Op1Eff eiOp t) -> 
    (Expr1Op1Eff eiOp t) -> 
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> 
    t -> Bool
equalRoundingUpDnImprovementBin1Var1 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort e =
    equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp initEffort
    where
    expr1Up eff = expr1 opUpEff eff e
    expr1Dn eff = expr1 opDnEff eff e
    expr2Up eff = expr2 opUpEff eff e
    expr2Dn eff = expr2 opDnEff eff e

equalRoundingUpDnImprovementBin1Var2 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (Expr1Op2Eff eiOp t) -> 
    (Expr1Op2Eff eiOp t) -> 
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> 
    t -> t -> Bool
equalRoundingUpDnImprovementBin1Var2 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort e1 e2 =
    equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp initEffort
    where
    expr1Up eff = expr1 opUpEff eff e1 e2
    expr1Dn eff = expr1 opDnEff eff e1 e2
    expr2Up eff = expr2 opUpEff eff e1 e2
    expr2Dn eff = expr2 opDnEff eff e1 e2

equalRoundingUpDnImprovementBin1Var3 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (Expr1Op3Eff eiOp t) -> 
    (Expr1Op3Eff eiOp t) -> 
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> 
    t -> t -> t -> Bool
equalRoundingUpDnImprovementBin1Var3 expr1 expr2 pCompareEff measureGap opUpEff opDnEff 
        effortImprComp initEffort e1 e2 e3 =
    equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp initEffort
    where
    expr1Up eff = expr1 opUpEff eff e1 e2 e3
    expr1Dn eff = expr1 opDnEff eff e1 e2 e3
    expr2Up eff = expr2 opUpEff eff e1 e2 e3
    expr2Dn eff = expr2 opDnEff eff e1 e2 e3

equalRoundingUpDnImprovementBin2Var3 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp1, Show eiOp1, 
     EffortIndicator eiOp2, Show eiOp2,
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (Expr2Op3Eff eiOp1 eiOp2 t) -> 
    (Expr2Op3Eff eiOp1 eiOp2 t) -> 
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, (eiOp1, eiOp2)) -> 
    t -> t -> t -> Bool
equalRoundingUpDnImprovementBin2Var3 expr1 expr2 pCompareEff measureGap 
        op1UpEff op2UpEff 
        op1DnEff op2DnEff 
        effortImprComp initEffort e1 e2 e3 =
    equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp initEffort
    where
    expr1Up eff = expr1 op1UpEff op2UpEff eff e1 e2 e3
    expr1Dn eff = expr1 op1DnEff op2DnEff eff e1 e2 e3
    expr2Up eff = expr2 op1UpEff op2UpEff eff e1 e2 e3
    expr2Dn eff = expr2 op1DnEff op2DnEff eff e1 e2 e3

thinEqualConsLeqRoundingUpDnImprovementBin2Var3 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp1, Show eiOp1, 
     EffortIndicator eiOp2, Show eiOp2,
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, 
     HasAntiConsistency t, Show t) =>
    (Expr2Op3Eff eiOp1 eiOp2 t) -> 
    (Expr2Op3Eff eiOp1 eiOp2 t) -> 
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) ->
    (ConsistencyEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, (eiOp1, eiOp2)) -> 
    t -> t -> t -> Bool
thinEqualConsLeqRoundingUpDnImprovementBin2Var3
        expr1 expr2 pCompareEff measureGap 
        op1UpEff op2UpEff 
        op1DnEff op2DnEff 
        effortConsistency
        effortImprComp initEffort 
        e1 e2 e3 =
    thinEqualConsLeqRoundingUpDnImprovement 
        [e1,e2,e3] expr1Up expr1Dn expr2Up expr2Dn 
        pCompareEff measureGap
        effortConsistency 
        effortImprComp initEffort
    where
    expr1Up eff = expr1 op1UpEff op2UpEff eff e1 e2 e3
    expr1Dn eff = expr1 op1DnEff op2DnEff eff e1 e2 e3
    expr2Up eff = expr2 op1UpEff op2UpEff eff e1 e2 e3
    expr2Dn eff = expr2 op1DnEff op2DnEff eff e1 e2 e3



equalRoundingUpDnImprovementBin2Var2 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp1, Show eiOp1, 
     EffortIndicator eiOp2, Show eiOp2,
     EffortIndicator eiMeasure, Show eiMeasure, 
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (Expr2Op2Eff eiOp1 eiOp2 t) -> 
    (Expr2Op2Eff eiOp1 eiOp2 t) -> 
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, (eiOp1, eiOp2)) -> 
    t -> t -> Bool
equalRoundingUpDnImprovementBin2Var2 expr1 expr2 pCompareEff measureGap 
        op1UpEff op2UpEff 
        op1DnEff op2DnEff 
        effortImprComp initEffort e1 e2 =
    equalRoundingUpDnImprovement expr1Up expr1Dn expr2Up expr2Dn pCompareEff measureGap 
        effortImprComp initEffort
    where
    expr1Up eff = expr1 op1UpEff op2UpEff eff e1 e2
    expr1Dn eff = expr1 op1DnEff op2DnEff eff e1 e2
    expr2Up eff = expr2 op1UpEff op2UpEff eff e1 e2
    expr2Dn eff = expr2 op1DnEff op2DnEff eff e1 e2

thinEqualConsLeqRoundingUpDnImprovement :: 
    (EffortIndicator eiMeasure, EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiMeasure, Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, 
     HasAntiConsistency t, Show t) =>
    [t] -> 
    (eiOp -> t) {-^ left hand side expression UP -} -> 
    (eiOp -> t) {-^ left hand side expression DN -} -> 
    (eiOp -> t) {-^ right hand side expression UP -} -> 
    (eiOp -> t) {-^ right hand side expression DN -} ->
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (ConsistencyEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> (eiMeasure, eiRel, eiOp) -> Bool
thinEqualConsLeqRoundingUpDnImprovement
        parameters
        expr1Up expr1Dn expr2Up expr2Dn
        pCompareEff measureGap 
        consistencyEffort 
        effortImprComp initEffort@(_, effComp, effOp)
    | allConsistent && allAntiConsistent =
        okIfThin
    | allConsistent =
        okIfConsistent
    | allAntiConsistent =
        okIfAntiConsistent
    where
    allConsistent =
        and $ map isConsistent parameters
    allAntiConsistent =
        and $ map isAntiConsistent parameters
    okIfThin =
            (equalRoundingUpDnImprovement
                expr1Up expr1Dn expr2Up expr2Dn 
                pCompareEff measureGap effortImprComp initEffort)
    okIfConsistent =
            leqRoundingUpDn expr1Dn expr2Up
    okIfAntiConsistent = 
            leqRoundingUpDn expr2Dn expr1Up
    isConsistent a = 
        justButNot False $ isConsistentEff consistencyEffort a
    isAntiConsistent a = 
        justButNot False $ isAntiConsistentEff consistencyEffort a
    leqRoundingUpDn expr1Dn expr2Up =
        case pCompareEff effComp (expr1Dn effOp) (expr2Up effOp) of
            Just res -> res
            Nothing -> True


equalRoundingUpDnImprovement :: 
    (EffortIndicator eiMeasure, EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiMeasure, Show eiOp, Show eiRel,
     NumOrd.PartialComparison gap, HasZero gap, HasInfinities gap, Show gap, Show t) =>
    (eiOp -> t) {-^ left hand side expression UP -} -> 
    (eiOp -> t) {-^ left hand side expression DN -} -> 
    (eiOp -> t) {-^ right hand side expression UP -} -> 
    (eiOp -> t) {-^ right hand side expression DN -} -> 
    (PRelEff eiRel t) -> (eiMeasure -> t -> t -> gap) -> 
    (NumOrd.PartialCompareEffortIndicator gap) -> 
    (eiMeasure, eiRel, eiOp) -> Bool
equalRoundingUpDnImprovement 
        expr1Up expr1Dn expr2Up expr2Dn 
        pCompareEff measureGap effortImprComp initEffort =
--    unsafePrint 
--    (
--        "leqRoundingUpDnImprovement:"
--        ++ "\n  efforts executed = \n" ++ unlines (map show $ take (comparisonCount + 1) efforts)
--        ++ "\n  5 successes = \n" ++ unlines (map show relevantSuccesses)
--        ++ "\n  5 imprecisions = \n" ++ unlines (map show $ take 5 imprecisions)
--    ) 
--    $
    case evalCatchNaNExceptions result of
            Left msg -> 
                unsafePrint ("leqRoundingUpDnImprovement: NaN exception: " ++ msg) True 
            -- ignore tests during which an AERN exception arises 
            --     (typically due to NaN)
            Right res ->
                case (not noneedComparison) && comparisonCount > 5 of
                    True -> 
                        unsafePrint 
                        ("leqRoundingUpDnImprovement:"
                          ++ " comparison count = " ++ show comparisonCount  
                          ++ "; effort = " ++ show (efforts !! comparisonCount))
                        res 
                    _ -> res
    where
    result = 
        (andUnsafeReportFirstFalse relevantSuccesses) && isImprovement  
    imprecision0Zero = 
        (imprecision0 ==? zero) == Just True
        where
        ?pCompareEffort = effortImprComp
    isImprovement =
        orUnsafeReportFalse $ 
            [
              (noneedComparison,
              "imprecision0 = " ++ show imprecision0)
            ,
             (comparisonResult, 
              "failed to reduce imprecision")  -- or it can be improved
            ]
    noneedComparison =
        (null imprecisions) || imprecision0Zero 
    (comparisonResult, comparisonCount) =
         let ?pCompareEffort = effortImprComp in 
         orMaybesCountUsed 0 $ map (imprecision0 >?) imprecisions
    orMaybesCountUsed cnt [] = (False, cnt)
    orMaybesCountUsed cnt (Just True:t) = (True, cnt)
    orMaybesCountUsed cnt (_:t) = orMaybesCountUsed (cnt + 1) t 
        
        
    relevantSuccesses = take 5 successes
    (successes, imprecision0 : imprecisions) = unzip $ map check efforts
    efforts =
        (initEffort : ) $ take 15 $ effortIncrementSequence initEffort
--            mergeManyLists $
--                map (take 20 . effortIncrementSequence) $ 
--                    effortIncrementVariants initEffort 
    check (effortMeasure, effortRel, effortOp) =
        -- the following catch does not work, currently have to
        --  catch the exceptions at a higher level 
        case evalCatchNaNExceptions (successWithMsg, imprecision) of
            Left msg -> ((True, ""), plusInfinity)
            Right res -> res 
        where
        successWithMsg =
            (success,
              "failure for effortRel = " ++ show effortRel 
                ++ " effortOp = " ++ show effortOp
              ++ "\n val1Dn <=? val2Up is " 
                ++ show val1Dn ++ " <=? " ++ show val2Up ++ " = "
                ++ show (val1Dn <=? val2Up)
              ++ "\n val2Dn <=? val1Up is " 
                ++ show val2Dn ++ " <=? " ++ show val1Up ++ " = "
                ++ show (val2Dn <=? val1Up)
            )
        success =
            (defined (val1Dn <=? val2Up) ===> (val1Dn <= val2Up))
            &&
            (defined (val2Dn <=? val1Up) ===> (val2Dn <= val1Up))
        imprecision =
            measureGap effortMeasure val1Dn val2Up
        val1Dn = expr1Dn effortOp
        val1Up = expr1Up effortOp
        val2Dn = expr2Dn effortOp
        val2Up = expr2Up effortOp
        (<=) = assumeTotal2 (<=?)
        (<=?) = pCompareEff effortRel

