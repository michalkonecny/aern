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
import Numeric.AERN.Basics.Bench
import Numeric.AERN.Basics.Consistency
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Bench
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Numeric.AERN.Misc.Debug

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Criterion

class RoundedExponentiation t where
    type ExpEffortIndicator t
    expDefaultEffort :: t -> ExpEffortIndicator t
    expInEff :: (ExpEffortIndicator t) -> t -> t
    expOutEff :: (ExpEffortIndicator t) -> t -> t

-- | @e^a*e^(-a) = 1@
propExpOfNegRecip ::
    (RefOrd.PartialComparison t,
     RoundedExponentiation t, RoundedMultiply t, Neg t, HasOne t,
     Show t, HasAntiConsistency t,
     HasDistance t,  Show (Distance t), HasInfinities (Distance t), HasZero (Distance t),  
     NumOrd.PartialComparison (Distance t),
     Show (ExpEffortIndicator t),
     EffortIndicator (ExpEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (ConsistencyEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     RefOrd.PartialCompareEffortIndicator t, 
     (ExpEffortIndicator t, MultEffortIndicator t)) -> 
    t -> Bool
propExpOfNegRecip _ effortConsistency effortDistComp initEffort e1 =
    thinEqualConsLeqRoundingUpDnImprovement [e1]
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff distanceBetweenEff
        effortConsistency 
        effortDistComp initEffort
    where
    expr1In (effExp, effMult) =
--        unsafePrintReturn (
--                "propExpOfNegRecip: expr2In: " 
--                ++ "\n e1 = " ++ (show e1)
--                ++ "\n expInEff effExp e1 = " ++ (show $ expInEff effExp e1)
--                ++ "\n expInEff effExp (neg e1) = " ++ (show $ expInEff effExp (neg e1))
--                ++ "\n product of the above = "
--        ) $
        let (>*<) = multInEff effMult in
        (expInEff effExp e1) >*< (expInEff effExp (neg e1))
    expr1Out (effExp, effMult) =
        let (<*>) = multOutEff effMult in
        (expOutEff effExp e1) <*> (expOutEff effExp (neg e1))
    expr2In (effExp, effMult) = one
    expr2Out (effExp, effMult) = one

-- | @e^(b+c) = e^b * e^c@
propExpOfAddToMult ::
    (RefOrd.PartialComparison t,
     RoundedExponentiation t, RoundedMultiply t,  RoundedAdd t,
     Show t,
     HasDistance t,  Show (Distance t), HasInfinities (Distance t), HasZero (Distance t),  
     NumOrd.PartialComparison (Distance t),
     Show (ExpEffortIndicator t),
     EffortIndicator (ExpEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     RefOrd.PartialCompareEffortIndicator t, 
     (ExpEffortIndicator t, MultEffortIndicator t, AddEffortIndicator t)) -> 
    t -> t -> Bool
propExpOfAddToMult _ effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
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
            testProperty "e^a * e^(-a) ⊑/⊒ 1" (propExpOfNegRecip sample)
        ,
            testProperty "e^(a + b) = e^a * e^b" (propExpOfAddToMult sample)
        ]
            
benchInOutExp (name, sample) =
    bgroup (name ++ " exp") $
        mkBenchAreasSequences1 (mkCommentImprecision1 expOutEff expInEff) 
            expOutEff benchExpAreas 10 (expDefaultEffort sample) sample 

benchExpAreas =
    [
        ("near 0", NumOrd.AreaLinear (Just $ -1/2) True (Just $ 1/2) True [])
    ,
        ("near -10", NumOrd.AreaLinear (Just $ -10.5) True (Just $ -9.5) True [])
    ,
        ("near 10", NumOrd.AreaLinear (Just $ 9.5) True (Just $ 10.5) True [])
    ,
        ("near 20", NumOrd.AreaLinear (Just $ 19.5) True (Just $ 20.5) True [])
    ]
    