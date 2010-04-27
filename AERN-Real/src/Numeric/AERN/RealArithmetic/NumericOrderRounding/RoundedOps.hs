{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.NumericOrderRounding.RoundedOps
    Description :  rounded basic arithmetical operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding.RoundedOps 
(
    RoundedAdd(..), RoundedSubtr(..), testsUpDnAdd,
    RoundedAbs(..), 
    RoundedMultiply(..), RoundedDivide(..)
)
where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.RealArithmetic.ExactOperations
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Numerals

import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe

infixl 6 +., +^, -., -^
infixl 7 *., *^

class RoundedAdd t where
    type AddEffortIndicator t
    addUpEff :: AddEffortIndicator t -> t -> t -> t
    addDnEff :: AddEffortIndicator t -> t -> t -> t
    addDefaultEffort :: t -> AddEffortIndicator t
    (+^) :: (?addUpDnEffort :: AddEffortIndicator t) => t -> t -> t
    (+.) :: (?addUpDnEffort :: AddEffortIndicator t) => t -> t -> t
    (+^) = addUpEff ?addUpDnEffort
    (+.) = addDnEff ?addUpDnEffort

propUpDnAddZero ::
    (NumOrd.Comparison t, RoundedAdd t, HasZero t,
     HasDistance t,  
     NumOrd.Comparison (Distance t), HasZero (Distance t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.PartialCompareEffortIndicator t, AddEffortIndicator t) -> 
    t -> Bool
propUpDnAddZero _ effortDist =
    roundedImprovingUnit zero NumOrd.pLeqEff (distanceBetweenEff effortDist) addUpEff addDnEff

propUpDnAddCommutative ::
    (NumOrd.Comparison t, RoundedAdd t, HasZero t,
     HasDistance t,  
     NumOrd.Comparison (Distance t), HasZero (Distance t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.PartialCompareEffortIndicator t, AddEffortIndicator t) -> 
    t -> t -> Bool
propUpDnAddCommutative _ effortDist =
    roundedImprovingCommutative NumOrd.pLeqEff (distanceBetweenEff effortDist) addUpEff addDnEff
       
--propUpDnAddAssociative ::
--    (NumOrd.Comparison t, RoundedAdd t) =>
--    t -> t -> t -> t -> Bool
--propUpDnAddAssociative _ =
--    roundedAssociative (NumOrd.<=?) (+^) (+.)
--       

testsUpDnAdd (name, sample) =
    testGroup (name ++ " +. +^") $
        [
            testProperty "0 absorbs" (propUpDnAddZero sample)
        ,
            testProperty "commutativity" (propUpDnAddCommutative sample)
        ]
        
class (RoundedAdd t, Neg t) => RoundedSubtr t where
    (-^) :: (?addUpDnEffort :: AddEffortIndicator t) => t -> t -> t
    (-.) :: (?addUpDnEffort :: AddEffortIndicator t) => t -> t -> t
    a -^ b = addUpEff ?addUpDnEffort a (neg b)
    a -. b = addDnEff ?addUpDnEffort a (neg b)

class RoundedAbs t where
    type AbsEffortIndicator t
    absDefaultEffort :: t -> AbsEffortIndicator t
    absUpEff :: (AbsEffortIndicator t) -> t -> t
    absDnEff :: (AbsEffortIndicator t) -> t -> t

absUpUsingCompMax ::
    (HasZero t, Neg t, 
     NumOrd.PartialComparison t, NumOrd.RoundedLattice t) =>
    (NumOrd.PartialCompareEffortIndicator t,
     NumOrd.MinmaxEffortIndicator t) ->
    t -> t 
absUpUsingCompMax (effortComp, effortMinmax) a =
    case NumOrd.pCompareEff effortComp zero a of
        Just EQ -> a
        Just LT -> a
        Just LEE -> a
        Just GT -> neg a
        Just GEE -> neg a
        _ -> zero `max` (a `max` (neg a))
    where
    max = NumOrd.maxUpEff effortMinmax

absDnUsingCompMax ::
    (HasZero t, Neg t, 
     NumOrd.PartialComparison t, NumOrd.RoundedLattice t) =>
    (NumOrd.PartialCompareEffortIndicator t,
     NumOrd.MinmaxEffortIndicator t) ->
    t -> t 
absDnUsingCompMax (effortComp, effortMinmax) a =
    case NumOrd.pCompareEff effortComp zero a of
        Just EQ -> a
        Just LT -> a
        Just LEE -> a
        Just GT -> neg a
        Just GEE -> neg a
        _ -> zero `max` (a `max` (neg a))
    where
    max = NumOrd.maxDnEff effortMinmax

class RoundedMultiply t where
    type MultEffortIndicator t
    multUpEff :: MultEffortIndicator t -> t -> t -> t
    multDnEff :: MultEffortIndicator t -> t -> t -> t
    multDefaultEffort :: t -> MultEffortIndicator t
    (*^) :: (?multUpDnEffort :: MultEffortIndicator t) => t -> t -> t
    (*.) :: (?multUpDnEffort :: MultEffortIndicator t) => t -> t -> t
    (*^) = multUpEff ?multUpDnEffort
    (*.) = multDnEff ?multUpDnEffort

class (RoundedAdd t, RoundedSubtr t, RoundedMultiply t) => RoundedRRing t

class RoundedDivide t where
    type DivEffortIndicator t
    divUpEff :: DivEffortIndicator t -> t -> t -> t
    divDnEff :: DivEffortIndicator t -> t -> t -> t
    divDefaultEffort :: t -> DivEffortIndicator t
    (/^) :: (?divUpDnEffort :: DivEffortIndicator t) => t -> t -> t
    (/.) :: (?divUpDnEffort :: DivEffortIndicator t) => t -> t -> t
    (/^) = divUpEff ?divUpDnEffort
    (/.) = divDnEff ?divUpDnEffort

class (RoundedRRing t, RoundedDivide t) => RoundedField t

--propUpDnMultOne ::
--    (NumOrd.Comparison t, RoundedMult t, HasOne t) =>
--    t -> t -> Bool
--propUpDnMultOne _ =
--    roundedUnit one (NumOrd.<=?) (*^) (*.)
--
--propUpDnMultCommutative ::
--    (NumOrd.Comparison t, RoundedMult t) =>
--    t -> t -> t -> Bool
--propUpDnMultCommutative _ =
--    roundedCommutative (NumOrd.<=?) (*^) (*.)
--       
--propUpDnMultAssociative ::
--    (NumOrd.Comparison t, RoundedMult t) =>
--    t -> t -> t -> t -> Bool
--propUpDnMultAssociative _ =
--    roundedAssociative (NumOrd.<=?) (*^) (*.)
--
--propUpDnMultAddDistributive ::
--    (NumOrd.Comparison t, RoundedMult t, RoundedAdd t) =>
--    t -> t -> t -> t -> Bool
--propUpDnMultAddDistributive _ =
--    roundedLeftDistributive (NumOrd.<=?) (+^) (*^) (+.) (*.)
--
--testsRoundedAddition ::
--    (NumOrd.Comparison t, RoundedAdd t, HasZero t, 
--     Arbitrary t) =>
--    (String, t) -> Test
--testsRoundedAddition (name, sample) =
--    testGroup (name ++ " rounded addition") $ 
--        [
--         testProperty "absorbs zero" (propUpDnAddZero sample)
--        ,
--         testProperty "commutative" (propUpDnAddCommutative sample)
--        ,
--         testProperty "associative" (propUpDnAddAssociative sample)
--        ]
--
--testsRoundedMultiplication ::
--    (NumOrd.Comparison t, RoundedMult t, RoundedAdd t, HasOne t, 
--     Arbitrary t) =>
--    (String, t) -> Test
--testsRoundedMultiplication (name, sample) =
--    testGroup (name ++ " rounded multiplication") $ 
--        [
--         testProperty "absorbs one" (propUpDnMultOne sample)
--        ,
--         testProperty "commutative" (propUpDnMultCommutative sample)
--        ,
--         testProperty "associative" (propUpDnMultAssociative sample)
--        ,
--         testProperty "distributes over addition" (propUpDnMultAddDistributive sample)
--        ]


