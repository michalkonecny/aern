{-# LANGUAGE FlexibleContexts #-}
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

import {-# Source #-} Numeric.AERN.RealArithmetic.NumericOrderRounding.RoundedRing
import Numeric.AERN.RealArithmetic.Measures

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Laws.Utilities

import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Misc.Bool
import Data.Maybe

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd


roundedAbsorbes ::
    (EffortIndicator eiRel, EffortIndicator eiOp,
     HasImprecision t, 
     NumOrd.PartialComparison (Imprecision t),
     RoundedSubtr (Imprecision t)) =>
    t -> (SmdcRelEff eiRel t) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (eiRel, eiOp) -> t -> Bool
roundedAbsorbes unit =
    equalRoundingUpDnImprovement11 (\_ _ e -> e) expr2
    where
    expr2 opEff effort e = 
        unit * e
        where
        (*) = opEff effort

equalRoundingUpDnImprovement11 :: 
    (EffortIndicator eiRel, EffortIndicator eiOp,
     HasImprecision t, 
     NumOrd.PartialComparison (Imprecision t),
     RoundedSubtr (Imprecision t)) =>
    (Expr1Op1Eff eiOp t) -> (Expr1Op1Eff eiOp t) -> 
    (SmdcRelEff eiRel t) -> (OpEff eiOp t) -> (OpEff eiOp t) -> 
    (eiRel, eiOp) -> t -> Bool
equalRoundingUpDnImprovement11 expr1 expr2 pCompareEff opUpEff opDnEff 
        initEffort@(initEffortRel, initEffortOp) e =
    and successes && isImprovement
    where
    isImprovement = or $ catMaybes $ map (improvement0 NumOrd.>?) improvements
    (successes, improvement0 : improvements) = unzip $ map check efforts
    efforts = take 5 $ effortIncrements initEffort
    check (effortRel, effortOp) =
        (success, improvement)
        where
        success =
            (defined (val1Dn <=? val2Up) ===> (val1Dn <= val2Up))
            &&
            (defined (val2Dn <=? val1Up) ===> (val2Dn <= val1Up))
        improvement =
            imprecisionOf val1Dn -. imprecisionOf val2Up
        val1Dn = expr1 opDnEff effortOp e
        val1Up = expr1 opUpEff effortOp e
        val2Dn = expr2 opDnEff effortOp e
        val2Up = expr2 opUpEff effortOp e
        (<=) = assumeTotal2 (<=?)
        (<=?) = pCompareEff effortRel

