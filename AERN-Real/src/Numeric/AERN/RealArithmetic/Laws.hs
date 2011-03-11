{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams, RankNTypes #-}
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
import Numeric.AERN.Basics.Mutable

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


roundedUnit ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     Show t) =>
    t -> 
    (PRelEff eiRel t) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (eiRel, eiOp) -> 
    t -> Bool
roundedUnit unit =
    equalRoundingUpDnBin1Var1 (\_ _ e -> e) expr2
    where
    expr2 opEff effort e = 
        unit * e
        where
        (*) = opEff effort

roundedReflexiveCollapse ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     Show t) =>
    t -> 
    (PRelEff eiRel t) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (eiRel, eiOp) -> 
    t -> Bool
roundedReflexiveCollapse unit =
    equalRoundingUpDnBin1Var1 (\_ _ e -> unit) expr2
    where
    expr2 opEff effort e = 
        e * e
        where
        (*) = opEff effort

roundedCommutative ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     Show t) =>
    (PRelEff eiRel t) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (eiRel, eiOp) -> 
    t -> t -> Bool
roundedCommutative =
    equalRoundingUpDnBin1Var2 expr1 expr2
    where
    expr1 opEff effort e1 e2 = 
        e1 * e2
        where
        (*) = opEff effort
    expr2 opEff effort e1 e2 = 
        e2 * e1
        where
        (*) = opEff effort

roundedAssociative ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     Show t) =>
    (PRelEff eiRel t) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (eiRel, eiOp) -> 
    t -> t -> t -> Bool
roundedAssociative =
    equalRoundingUpDnBin1Var3 expr1 expr2
    where
    expr1 opEff effort e1 e2 e3 = 
        (e1 * e2) * e3
        where
        (*) = opEff effort
    expr2 opEff effort e1 e2 e3 = 
        e1 * (e2 * e3)
        where
        (*) = opEff effort

roundedDistributive ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp1, Show eiOp1, 
     EffortIndicator eiOp2, Show eiOp2,
     HasAntiConsistency t, Show t) =>
    (PRelEff eiRel t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (ConsistencyEffortIndicator t) ->
    (eiRel, (eiOp1, eiOp2)) -> 
    t -> t -> t -> Bool
roundedDistributive 
        pCompareEff 
        op1UpEff op2UpEff 
        op1DnEff op2DnEff 
        effortConsistency
        initEffort 
        e1 e2 e3 =
--    unsafePrint
--    (
--        "property roundedDistributive: "
--        ++ "\n e1 = " ++ show e1
--        ++ "\n e2 = " ++ show e2
--        ++ "\n e3 = " ++ show e3
--    ) $        
    thinEqualConsLeqRoundingUpDnImprovement 
        -- cannot get equality when e1 is not thin 
        -- because e1 appears twice in expr1 (dependency error)
        [e1] expr1Up expr1Dn expr2Up expr2Dn 
        pCompareEff
        effortConsistency 
        initEffort
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


roundedNegSymmetric ::
    (Neg t,
     EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     Show t) =>
    (PRelEff eiRel t) -> 
    (UnaryOpEff eiOp t) -> (UnaryOpEff eiOp t) -> 
    (eiRel, eiOp) -> 
    t -> Bool
roundedNegSymmetric =
    equalRoundingUpDnUnary1Var1 expr1 expr2
    where
    expr1 opEff effort e = 
         opEff effort e
    expr2 opEff effort e = 
         opEff effort (neg e) 

roundedIdempotent ::
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     Show t) =>
    (PRelEff eiRel t) -> 
    (UnaryOpEff eiOp t) -> (UnaryOpEff eiOp t) -> 
    (eiRel, eiOp) -> 
    t -> Bool
roundedIdempotent =
    equalRoundingUpDnUnary1Var1 expr1 expr2
    where
    expr1 opEff effort e = 
         opEff effort e
    expr2 opEff effort e = 
         opEff effort (opEff effort e)




equalRoundingUpDnUnary1Var1 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     Show t) =>
    (Expr1UnaryOp1Eff eiOp t) -> 
    (Expr1UnaryOp1Eff eiOp t) -> 
    (PRelEff eiRel t) -> 
    (UnaryOpEff eiOp t) -> (UnaryOpEff eiOp t) -> 
    (eiRel, eiOp) -> 
    t -> Bool
equalRoundingUpDnUnary1Var1 expr1 expr2 pCompareEff opUpEff opDnEff 
        initEffort e =
    equalRoundingUpDn expr1Up expr1Dn expr2Up expr2Dn pCompareEff initEffort
    where
    expr1Up eff = expr1 opUpEff eff e
    expr1Dn eff = expr1 opDnEff eff e
    expr2Up eff = expr2 opUpEff eff e
    expr2Dn eff = expr2 opDnEff eff e

equalRoundingUpDnBin1Var1 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     Show t) =>
    (Expr1Op1Eff eiOp t) -> 
    (Expr1Op1Eff eiOp t) -> 
    (PRelEff eiRel t) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (eiRel, eiOp) -> 
    t -> Bool
equalRoundingUpDnBin1Var1 expr1 expr2 pCompareEff opUpEff opDnEff initEffort e =
    equalRoundingUpDn expr1Up expr1Dn expr2Up expr2Dn pCompareEff initEffort
    where
    expr1Up eff = expr1 opUpEff eff e
    expr1Dn eff = expr1 opDnEff eff e
    expr2Up eff = expr2 opUpEff eff e
    expr2Dn eff = expr2 opDnEff eff e

equalRoundingUpDnBin1Var2 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     Show t) =>
    (Expr1Op2Eff eiOp t) -> 
    (Expr1Op2Eff eiOp t) -> 
    (PRelEff eiRel t) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (eiRel, eiOp) -> 
    t -> t -> Bool
equalRoundingUpDnBin1Var2 expr1 expr2 pCompareEff opUpEff opDnEff 
        initEffort e1 e2 =
    equalRoundingUpDn expr1Up expr1Dn expr2Up expr2Dn pCompareEff initEffort
    where
    expr1Up eff = expr1 opUpEff eff e1 e2
    expr1Dn eff = expr1 opDnEff eff e1 e2
    expr2Up eff = expr2 opUpEff eff e1 e2
    expr2Dn eff = expr2 opDnEff eff e1 e2

equalRoundingUpDnBin1Var3 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp, Show eiOp, 
     Show t) =>
    (Expr1Op3Eff eiOp t) -> 
    (Expr1Op3Eff eiOp t) -> 
    (PRelEff eiRel t) -> 
    (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (eiRel, eiOp) -> 
    t -> t -> t -> Bool
equalRoundingUpDnBin1Var3 expr1 expr2 pCompareEff opUpEff opDnEff 
        initEffort e1 e2 e3 =
    equalRoundingUpDn expr1Up expr1Dn expr2Up expr2Dn pCompareEff initEffort
    where
    expr1Up eff = expr1 opUpEff eff e1 e2 e3
    expr1Dn eff = expr1 opDnEff eff e1 e2 e3
    expr2Up eff = expr2 opUpEff eff e1 e2 e3
    expr2Dn eff = expr2 opDnEff eff e1 e2 e3

equalRoundingUpDnBin2Var3 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp1, Show eiOp1, 
     EffortIndicator eiOp2, Show eiOp2,
     Show t) =>
    (Expr2Op3Eff eiOp1 eiOp2 t) -> 
    (Expr2Op3Eff eiOp1 eiOp2 t) -> 
    (PRelEff eiRel t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (eiRel, (eiOp1, eiOp2)) -> 
    t -> t -> t -> Bool
equalRoundingUpDnBin2Var3 expr1 expr2 pCompareEff 
        op1UpEff op2UpEff 
        op1DnEff op2DnEff 
        initEffort e1 e2 e3 =
    equalRoundingUpDn expr1Up expr1Dn expr2Up expr2Dn pCompareEff initEffort
    where
    expr1Up eff = expr1 op1UpEff op2UpEff eff e1 e2 e3
    expr1Dn eff = expr1 op1DnEff op2DnEff eff e1 e2 e3
    expr2Up eff = expr2 op1UpEff op2UpEff eff e1 e2 e3
    expr2Dn eff = expr2 op1DnEff op2DnEff eff e1 e2 e3

thinEqualConsLeqRoundingUpDnImprovementBin2Var3 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp1, Show eiOp1, 
     EffortIndicator eiOp2, Show eiOp2,
     HasAntiConsistency t, Show t) =>
    (Expr2Op3Eff eiOp1 eiOp2 t) -> 
    (Expr2Op3Eff eiOp1 eiOp2 t) -> 
    (PRelEff eiRel t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) ->
    (ConsistencyEffortIndicator t) -> 
    (eiRel, (eiOp1, eiOp2)) -> 
    t -> t -> t -> Bool
thinEqualConsLeqRoundingUpDnImprovementBin2Var3
        expr1 expr2 pCompareEff 
        op1UpEff op2UpEff 
        op1DnEff op2DnEff 
        effortConsistency
        initEffort 
        e1 e2 e3 =
    thinEqualConsLeqRoundingUpDnImprovement 
        [e1,e2,e3] expr1Up expr1Dn expr2Up expr2Dn 
        pCompareEff
        effortConsistency 
        initEffort
    where
    expr1Up eff = expr1 op1UpEff op2UpEff eff e1 e2 e3
    expr1Dn eff = expr1 op1DnEff op2DnEff eff e1 e2 e3
    expr2Up eff = expr2 op1UpEff op2UpEff eff e1 e2 e3
    expr2Dn eff = expr2 op1DnEff op2DnEff eff e1 e2 e3



equalRoundingUpDnBin2Var2 :: 
    (EffortIndicator eiRel, Show eiRel, 
     EffortIndicator eiOp1, Show eiOp1, 
     EffortIndicator eiOp2, Show eiOp2,
     Show t) =>
    (Expr2Op2Eff eiOp1 eiOp2 t) -> 
    (Expr2Op2Eff eiOp1 eiOp2 t) -> 
    (PRelEff eiRel t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (OpEff eiOp1 t) -> (OpEff eiOp2 t) -> 
    (eiRel, (eiOp1, eiOp2)) -> 
    t -> t -> Bool
equalRoundingUpDnBin2Var2 expr1 expr2 pCompareEff 
        op1UpEff op2UpEff 
        op1DnEff op2DnEff 
        initEffort e1 e2 =
    equalRoundingUpDn expr1Up expr1Dn expr2Up expr2Dn pCompareEff initEffort
    where
    expr1Up eff = expr1 op1UpEff op2UpEff eff e1 e2
    expr1Dn eff = expr1 op1DnEff op2DnEff eff e1 e2
    expr2Up eff = expr2 op1UpEff op2UpEff eff e1 e2
    expr2Dn eff = expr2 op1DnEff op2DnEff eff e1 e2

thinEqualConsLeqRoundingUpDnImprovement :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     HasAntiConsistency t, Show t) =>
    [t] -> 
    (eiOp -> t) {-^ left hand side expression UP -} -> 
    (eiOp -> t) {-^ left hand side expression DN -} -> 
    (eiOp -> t) {-^ right hand side expression UP -} -> 
    (eiOp -> t) {-^ right hand side expression DN -} ->
    (PRelEff eiRel t) -> 
    (ConsistencyEffortIndicator t) -> 
    (eiRel, eiOp) -> Bool
thinEqualConsLeqRoundingUpDnImprovement
        parameters
        expr1Up expr1Dn expr2Up expr2Dn
        pCompareEff
        consistencyEffort 
        initEffort@(effComp, effOp)
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
            (equalRoundingUpDn
                expr1Up expr1Dn expr2Up expr2Dn 
                pCompareEff initEffort)
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


equalRoundingUpDn :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     Show t) =>
    (eiOp -> t) {-^ left hand side expression UP -} -> 
    (eiOp -> t) {-^ left hand side expression DN -} -> 
    (eiOp -> t) {-^ right hand side expression UP -} -> 
    (eiOp -> t) {-^ right hand side expression DN -} -> 
    (PRelEff eiRel t) -> 
    (eiRel, eiOp) -> Bool
equalRoundingUpDn 
        expr1Up expr1Dn expr2Up expr2Dn 
        pCompareEff initEffort =
--    unsafePrint 
--    (
--        "equalRoundingUpDn:"
--        ++ "\n  efforts executed = \n" ++ unlines (map show $ take (comparisonCount + 1) efforts)
--        ++ "\n  5 successes = \n" ++ unlines (map show relevantSuccesses)
--    ) 
--    $
    case evalCatchDomViolationExceptions result of
            Left e -> True
--                unsafePrint ("leqRoundingUpDnImprovement: " ++ show e) True 
            -- ignore tests during which a domain violation exception arises 
            Right res -> res
    where
    result = 
        (andUnsafeReportFirstFalse relevantSuccesses)  
    relevantSuccesses = take 5 successes
    successes = map check efforts
    efforts =
        (initEffort : ) $ take 15 $ effortIncrementSequence initEffort
--            mergeManyLists $
--                map (take 20 . effortIncrementSequence) $ 
--                    effortIncrementVariants initEffort 
    check (effortRel, effortOp) =
        -- the following catch does not work, currently have to
        --  catch the exceptions at a higher level 
        case evalCatchDomViolationExceptions (successWithMsg) of
            Left msg -> (True, "")
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
        val1Dn = expr1Dn effortOp
        val1Up = expr1Up effortOp
        val2Dn = expr2Dn effortOp
        val2Up = expr2Up effortOp
        (<=) = assumeTotal2 (<=?)
        (<=?) = pCompareEff effortRel

roundedInPlace1ConsistentWithPure ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     CanBeMutable t, Show t) =>
    (forall s. eiOp -> OpMutable1 t s) {-^ left hand side expression UP -} -> 
    (forall s. eiOp -> OpMutable1 t s) {-^ left hand side expression DN -} -> 
    (eiOp -> UnaryOp t) {-^ right hand side expression UP -} -> 
    (eiOp -> UnaryOp t) {-^ right hand side expression DN -} -> 
    (PRelEff eiRel t) -> 
    (eiRel, eiOp) -> 
    t ->
    Bool
roundedInPlace1ConsistentWithPure
        opUpInPlaceEff opDnInPlaceEff opUpEff opDnEff 
        pLeqEff initEffort
        e
        =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        pLeqEff initEffort
    where
    opUpEffViaInPlace = mutable1EffToPure (opUpInPlaceEff)
    opDnEffViaInPlace = mutable1EffToPure (opDnInPlaceEff)
    expr1Up eff = opUpEff eff e
    expr1Dn eff = opDnEff eff e
    expr2Up eff = opUpEffViaInPlace eff e
    expr2Dn eff = opDnEffViaInPlace eff e

roundedInPlace2ConsistentWithPure ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     Show eiOp, Show eiRel,
     CanBeMutable t, Show t) =>
    (forall s. eiOp -> OpMutable2 t s) {-^ left hand side expression UP -} -> 
    (forall s. eiOp -> OpMutable2 t s) {-^ left hand side expression DN -} -> 
    (eiOp -> Op t) {-^ right hand side expression UP -} -> 
    (eiOp -> Op t) {-^ right hand side expression DN -} -> 
    (PRelEff eiRel t) -> 
    (eiRel, eiOp) -> 
    t -> t ->
    Bool
roundedInPlace2ConsistentWithPure
        opUpInPlaceEff opDnInPlaceEff opUpEff opDnEff 
        pLeqEff initEffort
        e1 e2
        =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        pLeqEff initEffort
    where
    opUpEffViaInPlace = mutable2EffToPure (opUpInPlaceEff)
    opDnEffViaInPlace = mutable2EffToPure (opDnInPlaceEff)
    expr1Up eff =
        let (*^) = opUpEff eff in e1 *^ e2
    expr1Dn eff =
        let (*.) = opDnEff eff in e1 *. e2
    expr2Up eff =
        let (*^) = opUpEffViaInPlace eff in e1 *^ e2
    expr2Dn eff =
        let (*.) = opDnEffViaInPlace eff in e1 *. e2

