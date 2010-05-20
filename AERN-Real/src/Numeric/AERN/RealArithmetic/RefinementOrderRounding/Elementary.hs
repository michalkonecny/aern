{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.Elementary
    Description :  support for various common elementary functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Support for various common elementary functions.
    
    This module is hidden and reexported via its parent RefinementOrderRounding. 
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.Elementary where

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps

import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class RoundedExponentiation t where
    type ExpEffortIndicator t
    expDefaultEffortIndicator :: t -> ExpEffortIndicator t
    expInEff :: (ExpEffortIndicator t) -> t -> t
    expOutEff :: (ExpEffortIndicator t) -> t -> t

-- | @e^a*e^(-a) = 1@
propExpOfNegRecip ::
    (RefOrd.PartialComparison t,
     RoundedExponentiation t, RoundedMultiply t, Neg t, HasOne t,
     HasDistance t,  Show (Distance t), HasInfinities (Distance t), HasZero (Distance t),  
     NumOrd.PartialComparison (Distance t),
     Show (ExpEffortIndicator t),
     EffortIndicator (ExpEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     (ExpEffortIndicator t, MultEffortIndicator t)) -> 
    t -> Bool
propExpOfNegRecip _ effortDist effortDistComp initEffort e1 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1In (effExp, effMult) = one
    expr1Out (effExp, effMult) = one
    expr2In (effExp, effMult) =
        let (>*<) = multInEff effMult in
        (expInEff effExp e1) >*< (expInEff effExp (neg e1))
    expr2Out (effExp, effMult) =
        let (<*>) = multOutEff effMult in
        (expOutEff effExp e1) <*> (expOutEff effExp (neg e1))

-- | @e^(b+c) = e^b * e^c@
propExpOfAddToMult ::
    (RefOrd.PartialComparison t,
     RoundedExponentiation t, RoundedMultiply t,  RoundedAdd t,
     HasDistance t,  Show (Distance t), HasInfinities (Distance t), HasZero (Distance t),  
     NumOrd.PartialComparison (Distance t),
     Show (ExpEffortIndicator t),
     EffortIndicator (ExpEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     (ExpEffortIndicator t, MultEffortIndicator t, AddEffortIndicator t)) -> 
    t -> t -> Bool
propExpOfAddToMult _ effortDist effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1In (effExp, effMult, effAdd) =
        let (+^) = addInEff effAdd in
        (expInEff effExp (e1 +^ e2))
    expr1Out (effExp, effMult, effAdd) =
        let (+.) = addOutEff effAdd in
        (expOutEff effExp (e1 +. e2))
    expr2In (effExp, effMult, effAdd) =
        let (*^) = multInEff effMult in
        (expInEff effExp e1) *^ (expInEff effExp e2)
    expr2Out (effExp, effMult, effAdd) =
        let (*.) = multOutEff effMult in
        (expOutEff effExp e1) *. (expOutEff effExp e2)
    
testsInOutExp (name, sample) =
    testGroup (name ++ " exp in/out") $
        [
            testProperty "e^a * e^(-a) = 1" (propExpOfNegRecip sample)
        ,
            testProperty "e^(a + b) = e^a * e^b" (propExpOfAddToMult sample)
        ]
            