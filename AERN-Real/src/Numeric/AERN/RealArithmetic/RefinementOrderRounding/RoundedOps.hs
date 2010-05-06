{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RefinementOrderRounding.RoundedMult
    Description :  rounded addition and multiplication  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded addition and multiplication.
    
    This module is hidden and reexported via its parent RefinementOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.RoundedOps where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.RealArithmetic.ExactOperations
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Numerals

import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe

infixl 6 <+>, >+<, <->, >-<
infixl 7 <*>, >*<

class RoundedAdd t where
    type AddEffortIndicator t
    addInEff :: AddEffortIndicator t -> t -> t -> t
    addOutEff :: AddEffortIndicator t -> t -> t -> t
    addDefaultEffort :: t -> AddEffortIndicator t
    (>+<) :: (?addInOutEffort :: AddEffortIndicator t) => t -> t -> t
    (<+>) :: (?addInOutEffort :: AddEffortIndicator t) => t -> t -> t
    (>+<) = addInEff ?addInOutEffort
    (<+>) = addOutEff ?addInOutEffort

propInOutAddZero ::
    (RefOrd.Comparison t, RoundedAdd t, HasZero t,
     HasDistance t, ArithUpDn.RoundedSubtr (Distance t), 
     NumOrd.Comparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (RefOrd.PartialCompareEffortIndicator t, AddEffortIndicator t) -> 
    t -> Bool
propInOutAddZero _ effortDist =
    roundedImprovingUnit zero RefOrd.pLeqEff (distanceBetweenEff effortDist) addInEff addOutEff

--propInOutAddCommutative ::
--    (RefOrd.Comparison t, RoundedAdd t) =>
--    t -> t -> t -> Bool
--propInOutAddCommutative _ =
--    roundedCommutative (RefOrd.<=?) (+^) (+.)
--       
--propInOutAddAssociative ::
--    (RefOrd.Comparison t, RoundedAdd t) =>
--    t -> t -> t -> t -> Bool
--propInOutAddAssociative _ =
--    roundedAssociative (RefOrd.<=?) (+^) (+.)
--       

class (RoundedAdd t, Neg t) => RoundedSubtr t where
    (>-<) :: (?addInOutEffort :: AddEffortIndicator t) => t -> t -> t
    (<->) :: (?addInOutEffort :: AddEffortIndicator t) => t -> t -> t
    a >-< b = addInEff ?addInOutEffort a (neg b)
    a <-> b = addOutEff ?addInOutEffort a (neg b)

class RoundedAbs t where
    type AbsEffortIndicator t
    absDefaultEffort :: t -> AbsEffortIndicator t
    absInEff :: (AbsEffortIndicator t) -> t -> t
    absOutEff :: (AbsEffortIndicator t) -> t -> t

absOutUsingCompMax ::
    (HasZero t, Neg t, 
     NumOrd.PartialComparison t, NumOrd.OuterRoundedLattice t) =>
    (NumOrd.PartialCompareEffortIndicator t,
     NumOrd.MinmaxOuterEffortIndicator t) ->
    t -> t 
absOutUsingCompMax (effortComp, effortMinmax) a =
    case NumOrd.pCompareEff effortComp zero a of
        Just EQ -> a
        Just LT -> a
        Just LEE -> a
        Just GT -> neg a
        Just GEE -> neg a
        _ -> zero `max` (a `max` (neg a))
    where
    max = NumOrd.maxOuterEff effortMinmax

absInUsingCompMax ::
    (HasZero t, Neg t, 
     NumOrd.PartialComparison t, NumOrd.InnerRoundedLattice t) =>
    (NumOrd.PartialCompareEffortIndicator t,
     NumOrd.MinmaxInnerEffortIndicator t) ->
    t -> t 
absInUsingCompMax (effortComp, effortMinmax) a =
    case NumOrd.pCompareEff effortComp zero a of
        Just EQ -> a
        Just LT -> a
        Just LEE -> a
        Just GT -> neg a
        Just GEE -> neg a
        _ -> zero `max` (a `max` (neg a))
    where
    max = NumOrd.maxInnerEff effortMinmax

class RoundedMultiply t where
    type MultEffortIndicator t
    multInEff :: MultEffortIndicator t -> t -> t -> t
    multOutEff :: MultEffortIndicator t -> t -> t -> t
    multDefaultEffort :: t -> MultEffortIndicator t
    (>*<) :: (?multInOutEffort :: MultEffortIndicator t) => t -> t -> t
    (<*>) :: (?multInOutEffort :: MultEffortIndicator t) => t -> t -> t
    (>*<) = multInEff ?multInOutEffort
    (<*>) = multOutEff ?multInOutEffort

class RoundedDivide t where
    type DivEffortIndicator t
    divInEff :: DivEffortIndicator t -> t -> t -> t
    divOutEff :: DivEffortIndicator t -> t -> t -> t
    divDefaultEffort :: t -> DivEffortIndicator t
    (>/<) :: (?divInOutEffort :: DivEffortIndicator t) => t -> t -> t
    (</>) :: (?divInOutEffort :: DivEffortIndicator t) => t -> t -> t
    (>/<) = divInEff ?divInOutEffort
    (</>) = divOutEff ?divInOutEffort

--propInOutMultOne ::
--    (RefOrd.Comparison t, RoundedMult t, HasOne t) =>
--    t -> t -> Bool
--propInOutMultOne _ =
--    roundedUnit one (RefOrd.<=?) (*^) (*.)
--
--propInOutMultCommutative ::
--    (RefOrd.Comparison t, RoundedMult t) =>
--    t -> t -> t -> Bool
--propInOutMultCommutative _ =
--    roundedCommutative (RefOrd.<=?) (*^) (*.)
--       
--propInOutMultAssociative ::
--    (RefOrd.Comparison t, RoundedMult t) =>
--    t -> t -> t -> t -> Bool
--propInOutMultAssociative _ =
--    roundedAssociative (RefOrd.<=?) (*^) (*.)
--
--propInOutMultAddDistributive ::
--    (RefOrd.Comparison t, RoundedMult t, RoundedAdd t) =>
--    t -> t -> t -> t -> Bool
--propInOutMultAddDistributive _ =
--    roundedLeftDistributive (RefOrd.<=?) (+^) (*^) (+.) (*.)
--
--testsRoundedAddition ::
--    (RefOrd.Comparison t, RoundedAdd t, HasZero t, 
--     Arbitrary t) =>
--    (String, t) -> Test
--testsRoundedAddition (name, sample) =
--    testGroup (name ++ " rounded addition") $ 
--        [
--         testProperty "absorbs zero" (propInOutAddZero sample)
--        ,
--         testProperty "commutative" (propInOutAddCommutative sample)
--        ,
--         testProperty "associative" (propInOutAddAssociative sample)
--        ]
--
--testsRoundedMultiplication ::
--    (RefOrd.Comparison t, RoundedMult t, RoundedAdd t, HasOne t, 
--     Arbitrary t) =>
--    (String, t) -> Test
--testsRoundedMultiplication (name, sample) =
--    testGroup (name ++ " rounded multiplication") $ 
--        [
--         testProperty "absorbs one" (propInOutMultOne sample)
--        ,
--         testProperty "commutative" (propInOutMultCommutative sample)
--        ,
--         testProperty "associative" (propInOutMultAssociative sample)
--        ,
--         testProperty "distributes over addition" (propInOutMultAddDistributive sample)
--        ]

