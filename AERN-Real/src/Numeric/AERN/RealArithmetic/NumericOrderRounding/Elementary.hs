{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.Elementary
    Description :  support for various common elementary functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Support for various common elementary functions.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}

module Numeric.AERN.RealArithmetic.NumericOrderRounding.Elementary where

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps

import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class RoundedExponentiation t where
    type ExpEffortIndicator t
    expDefaultEffortIndicator :: t -> ExpEffortIndicator t
    expUpEff :: (ExpEffortIndicator t) -> t -> t
    expDnEff :: (ExpEffortIndicator t) -> t -> t

-- | @e^a*e^(-a) = 1@
propExpOfNegRecip ::
    (NumOrd.PartialComparison t, NumOrd.RoundedLattice t,
     RoundedExponentiation t, RoundedMultiply t, Neg t, HasOne t,
     Show t,
     HasDistance t,  Show (Distance t), HasInfinities (Distance t), HasZero (Distance t),  
     NumOrd.PartialComparison (Distance t),
     Show (ExpEffortIndicator t),
     EffortIndicator (ExpEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     NumOrd.PartialCompareEffortIndicator t, 
     (ExpEffortIndicator t, MultEffortIndicator t)) -> 
    t -> Bool
propExpOfNegRecip _ effortDistComp initEffort e1 =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    expr1Up (effExp, effMult) = one
    expr1Dn (effExp, effMult) = one
    expr2Up (effExp, effMult) =
        let (*^) = multUpEff effMult in
        (expUpEff effExp e1) *^ (expUpEff effExp (neg e1))
    expr2Dn (effExp, effMult) =
        let (*.) = multDnEff effMult in
        (expDnEff effExp e1) *. (expDnEff effExp (neg e1))

-- | @e^(b+c) = e^b * e^c@
propExpOfAddToMult ::
    (NumOrd.PartialComparison t,
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
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     NumOrd.PartialCompareEffortIndicator t, 
     (ExpEffortIndicator t, MultEffortIndicator t, AddEffortIndicator t)) -> 
    t -> t -> Bool
propExpOfAddToMult _ effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    expr1Up (effExp, effMult, effAdd) =
        let (+^) = addUpEff effAdd in
        (expUpEff effExp (e1 +^ e2))
    expr1Dn (effExp, effMult, effAdd) =
        let (+.) = addDnEff effAdd in
        (expDnEff effExp (e1 +. e2))
    expr2Up (effExp, effMult, effAdd) =
        let (*^) = multUpEff effMult in
        (expUpEff effExp e1) *^ (expUpEff effExp e2)
    expr2Dn (effExp, effMult, effAdd) =
        let (*.) = multDnEff effMult in
        (expDnEff effExp e1) *. (expDnEff effExp e2)
    
testsUpDnExp (name, sample) =
    testGroup (name ++ " exp up/dn") $
        [
            testProperty "e^a * e^(-a) = 1" (propExpOfNegRecip sample)
        ,
            testProperty "e^(a + b) = e^a * e^b" (propExpOfAddToMult sample)
        ]
    