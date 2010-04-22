{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.NumericOrderRounding.RoundedMult
    Description :  rounded addition and multiplication  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded addition and multiplication.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding.RoundedRing where

import Numeric.AERN.RealArithmetic.ExactOperations
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Numerals

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Granularity
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
     HasDistance t, RoundedSubtr (Distance t), 
     NumOrd.Comparison (Distance t), HasZero (Distance t),
     EffortIndicator (AddEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.PartialCompareEffortIndicator t, AddEffortIndicator t) -> 
    t -> Bool
propUpDnAddZero _ =
    roundedImprovingUnit zero NumOrd.pLeqEff distanceBetween addUpEff addDnEff

--propUpDnAddCommutative ::
--    (NumOrd.Comparison t, RoundedAdd t) =>
--    t -> t -> t -> Bool
--propUpDnAddCommutative _ =
--    roundedCommutative (NumOrd.<=?) (+^) (+.)
--       
--propUpDnAddAssociative ::
--    (NumOrd.Comparison t, RoundedAdd t) =>
--    t -> t -> t -> t -> Bool
--propUpDnAddAssociative _ =
--    roundedAssociative (NumOrd.<=?) (+^) (+.)
--       

class (RoundedAdd t, Neg t) => RoundedSubtr t where
    (-^) :: (?addUpDnEffort :: AddEffortIndicator t) => t -> t -> t
    (-.) :: (?addUpDnEffort :: AddEffortIndicator t) => t -> t -> t
    a -^ b = addUpEff ?addUpDnEffort a (neg b)
    a -. b = addDnEff ?addUpDnEffort a (neg b)

class RoundedMultiply t where
    type MultEffortIndicator t
    multUpEff :: MultEffortIndicator t -> t -> t -> t
    multDnEff :: MultEffortIndicator t -> t -> t -> t
    multDefaultEffort :: t -> MultEffortIndicator t
    (*^) :: (?multUpDnEffort :: MultEffortIndicator t) => t -> t -> t
    (*.) :: (?multUpDnEffort :: MultEffortIndicator t) => t -> t -> t
    (*^) = multUpEff ?multUpDnEffort
    (*.) = multDnEff ?multUpDnEffort

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


