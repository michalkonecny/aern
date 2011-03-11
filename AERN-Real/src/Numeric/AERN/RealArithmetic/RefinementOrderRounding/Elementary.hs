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
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding.Conversion as UpDnConversion

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.ShowInternals
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
     Show (ExpEffortIndicator t),
     EffortIndicator (ExpEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (ConsistencyEffortIndicator t) -> 
    (RefOrd.PartialCompareEffortIndicator t, 
     (ExpEffortIndicator t, MultEffortIndicator t)) -> 
    t -> Bool
propExpOfNegRecip _ effortConsistency initEffort e1 =
    thinEqualConsLeqRoundingUpDnImprovement [e1]
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff
        effortConsistency 
        initEffort
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
    (RefOrd.PartialCompareEffortIndicator t, 
     (ExpEffortIndicator t, MultEffortIndicator t, AddEffortIndicator t)) -> 
    t -> t -> Bool
propExpOfAddToMult _ initEffort e1 e2 =
    equalRoundingUpDn
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff initEffort
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
            
benchInOutExp (name, sample) areas =
    bgroup (name ++ " exp") $
        mkBenchAreasSequences1 (mkCommentImprecision1 expOutEff expInEff) 
            expOutEff areas 10 (expDefaultEffort sample) sample 

benchExpAreasReal =
    [
        ("near 0", NumOrd.AreaLinear (Just $ -1/2) True (Just $ 1/2) True [])
    ,
        ("near -10", NumOrd.AreaLinear (Just $ -10.5) True (Just $ -9.5) True [])
    ,
        ("near 10", NumOrd.AreaLinear (Just $ 9.5) True (Just $ 10.5) True [])
    ,
        ("near 20", NumOrd.AreaLinear (Just $ 19.5) True (Just $ 20.5) True [])
    ]

class RoundedSquareRoot t where
    type SqrtEffortIndicator t
    sqrtDefaultEffort :: t -> SqrtEffortIndicator t
    sqrtInEff :: (SqrtEffortIndicator t) -> t -> t
    sqrtOutEff :: (SqrtEffortIndicator t) -> t -> t

propSqrtSquare ::
    (RefOrd.PartialComparison t, 
     RoundedSquareRoot t, RoundedMultiply t, HasZero t,
     UpDnConversion.Convertible t Double,
     RoundedMixedAdd t Double,
     Show t,
--     ShowInternals t,
     Show (UpDnConversion.ConvertEffortIndicator t Double),
     EffortIndicator (UpDnConversion.ConvertEffortIndicator t Double),
     Show (MixedAddEffortIndicator t Double),
     EffortIndicator (MixedAddEffortIndicator t Double),
     Show (SqrtEffortIndicator t),
     EffortIndicator (SqrtEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (UpDnConversion.ConvertEffortIndicator t Double, 
     MixedAddEffortIndicator t Double) ->
    (RefOrd.PartialCompareEffortIndicator t, 
     (SqrtEffortIndicator t, 
      MultEffortIndicator t, 
      RefOrd.PartialCompareEffortIndicator t)) -> 
    t -> Bool
propSqrtSquare _ (effortToDbl, effortAddDbl) initEffort e1 =
    equalRoundingUpDn
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff initEffort
    where
    e1Pos =
        case maybeE1LowerBoundD of
            Just e1LowerBoundD
                | e1LowerBoundD <= (0 :: Double) -> 
                    mixedAddOutEff effortAddDbl e1 (0.5 - e1LowerBoundD)
                | otherwise -> e1
            _ -> e1
        where
        maybeE1LowerBoundD = UpDnConversion.convertDnEff effortToDbl e1  
    expr1In (effSqrt, effMult, effCompare) =
        sqrtE1 >*< sqrtE1
        where
        (>*<) = multInEff effMult
        sqrtE1 = sqrtInEff effSqrt e1Pos
    expr1Out (effSqrt, effMult, effCompare) =
        sqrtE1 <*> sqrtE1
        where
        (<*>) = multOutEff effMult
        sqrtE1 = sqrtOutEff effSqrt e1Pos
    expr2In _ = e1Pos
    expr2Out _ = e1Pos

testsInOutSqrt (name, sample) =
    testGroup (name ++ " sqrt in/out") $
        [
            testProperty "sqrt(e)^2 = e" (propSqrtSquare sample)
        ]
    