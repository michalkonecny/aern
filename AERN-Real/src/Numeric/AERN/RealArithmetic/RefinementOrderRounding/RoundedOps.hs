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
    (RefOrd.PartialComparison t, RoundedAdd t, HasZero t,
     HasDistance t, RoundedSubtr (Distance t), 
     NumOrd.Comparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     EffortIndicator (AddEffortIndicator t),
     Show (AddEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (RefOrd.PartialCompareEffortIndicator t, AddEffortIndicator t) -> 
    t -> Bool
propInOutAddZero _ effortDist =
    roundedImprovingUnit zero RefOrd.pLeqEff (distanceBetweenEff effortDist) addInEff addOutEff

propInOutAddCommutative ::
    (RefOrd.PartialComparison t, RoundedAdd t, HasZero t,
     HasDistance t,  
     NumOrd.Comparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (RefOrd.PartialCompareEffortIndicator t, AddEffortIndicator t) -> 
    t -> t -> Bool
propInOutAddCommutative _ effortDist =
    roundedImprovingCommutative RefOrd.pLeqEff (distanceBetweenEff effortDist) addInEff addOutEff

propInOutAddAssociative ::
    (RefOrd.PartialComparison t, RoundedAdd t, HasZero t,
     HasDistance t,  
     NumOrd.Comparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (RefOrd.PartialCompareEffortIndicator t, AddEffortIndicator t) -> 
    t -> t -> t -> Bool
propInOutAddAssociative _ effortDist =
    roundedImprovingAssociative RefOrd.pLeqEff (distanceBetweenEff effortDist) addInEff addOutEff

propInOutAddMonotone ::
    (RefOrd.PartialComparison t, RoundedAdd t,
     RefOrd.ArbitraryOrderedTuple t,  
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (AddEffortIndicator t) -> 
    (RefOrd.LEPair t) -> (RefOrd.LEPair t) -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
propInOutAddMonotone _ =
    roundedRefinementMonotone2 addInEff addOutEff

testsInOutAdd (name, sample) =
    testGroup (name ++ " <+> >+<") $
        [
            testProperty "0 absorbs" (propInOutAddZero sample)
        ,
            testProperty "commutative" (propInOutAddCommutative sample)
        ,
            testProperty "associative" (propInOutAddAssociative sample)
        ,
            testProperty "refinement monotone" (propInOutAddMonotone sample)
        ]


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

