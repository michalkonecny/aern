{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.FieldOps
    Description :  rounded basic arithmetic operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    In-place versions of rounded basic arithmetic operations.
    
    Each operations takes mutable parameters instead of pure parameters
    and has one extra mutable parameter before the other parameters, 
    in which it stores the result.
    The mutable parameters can alias arbitrarily, making it possible
    to eg add to a number overwriting the original number.
    
    The operations have as their first paramter a non-mutable sample value
    to aid type-checking, ie to help work out which type the mutable parameters
    contain.
    
    This module is hidden and reexported via its parent RefinementOrderRounding.InPlace. 
-}
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.FieldOps 
(
    RoundedAddInPlace(..), 
    RoundedSubtrInPlace(..),
    RoundedAbsInPlace(..),
    RoundedMultiplyInPlace(..),
    RoundedPowerToNonnegIntInPlace(..),
    powerToNonnegIntInInPlaceEffFromMult,
    powerToNonnegIntOutInPlaceEffFromMult,
    RoundedDivideInPlace(..),
    testsInOutFieldOpsInPlace,
    RoundedRingInPlace(..), RoundedFieldInPlace(..)
)
where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps

import Numeric.AERN.RealArithmetic.Auxiliary
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad.ST
import Data.Maybe

class (RoundedAdd t, CanBeMutable t) => RoundedAddInPlace t where
    addInInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    addOutInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    addInInPlaceEff sample = pureToMutable2Eff sample addInEff 
    addOutInPlaceEff sample = pureToMutable2Eff sample addOutEff 

propInOutAddInPlace ::
    (RefOrd.PartialComparison t, RoundedAddInPlace t, Neg t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
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
     AddEffortIndicator t) -> 
    t -> t -> Bool
propInOutAddInPlace sample effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    addInEffViaInPlace = mutable2EffToPure (addInInPlaceEff sample)
    addOutEffViaInPlace = mutable2EffToPure (addOutInPlaceEff sample)
    expr1In eff =
        let (>+<) = addInEff eff in e1 >+< e2
    expr1Out eff =
        let (<+>) = addOutEff eff in e1 <+> e2
    expr2In eff =
        let (>+<) = addInEffViaInPlace eff in e1 >+< e2
    expr2Out eff =
        let (<+>) = addOutEffViaInPlace eff in e1 <+> e2

class (RoundedAddInPlace t,  RoundedSubtr t, NegInPlace t) => RoundedSubtrInPlace t where
    subtrInInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    subtrOutInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    subtrInInPlaceEff sample effort rM aM bM =
        do
        bbM <- cloneMutable sample bM
        negInPlace sample bbM bM
        addInInPlaceEff sample effort rM aM bbM
    subtrOutInPlaceEff sample effort rM aM bM = 
        do
        bbM <- cloneMutable sample bM
        negInPlace sample bbM bM
        addOutInPlaceEff sample effort rM aM bbM

propInOutSubtrInPlace ::
    (RefOrd.PartialComparison t, RoundedSubtrInPlace t, Neg t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
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
     AddEffortIndicator t) -> 
    t -> t -> Bool
propInOutSubtrInPlace sample effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    subtrInEffViaInPlace = mutable2EffToPure (subtrInInPlaceEff sample)
    subtrOutEffViaInPlace = mutable2EffToPure (subtrOutInPlaceEff sample)
    expr1In eff =
        let (>-<) = subtrInEff eff in e1 >-< e2
    expr1Out eff =
        let (<->) = subtrOutEff eff in e1 <-> e2
    expr2In eff =
        let (>-<) = subtrInEffViaInPlace eff in e1 >-< e2
    expr2Out eff =
        let (<->) = subtrOutEffViaInPlace eff in e1 <-> e2


class (RoundedAbs t, CanBeMutable t) => RoundedAbsInPlace t where
    absInInPlaceEff :: t -> OpMutable1Eff (AbsEffortIndicator t) t s
    absOutInPlaceEff :: t -> OpMutable1Eff (AbsEffortIndicator t) t s
    absInInPlaceEff sample = pureToMutable1Eff sample absInEff 
    absOutInPlaceEff sample = pureToMutable1Eff sample absOutEff 

propInOutAbsInPlace ::
    (RefOrd.PartialComparison t, RoundedAbsInPlace t, Neg t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (AbsEffortIndicator t),
     EffortIndicator (AbsEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t, 
     RefOrd.PartialCompareEffortIndicator t, 
     AbsEffortIndicator t) -> 
    t -> Bool
propInOutAbsInPlace sample effortDistComp initEffort e1 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    absInEffViaInPlace = mutable1EffToPure (absInInPlaceEff sample)
    absOutEffViaInPlace = mutable1EffToPure (absOutInPlaceEff sample)
    expr1In eff = absInEff eff e1
    expr1Out eff = absOutEff eff e1
    expr2In eff = absInEffViaInPlace eff e1
    expr2Out eff = absOutEffViaInPlace eff e1


class (RoundedMultiply t, CanBeMutable t) => RoundedMultiplyInPlace t where
    multInInPlaceEff :: t -> OpMutable2Eff (MultEffortIndicator t) t s
    multOutInPlaceEff :: t -> OpMutable2Eff (MultEffortIndicator t) t s
    multInInPlaceEff sample = pureToMutable2Eff sample multInEff 
    multOutInPlaceEff sample = pureToMutable2Eff sample multOutEff 

propInOutMultInPlace ::
    (RefOrd.PartialComparison t, RoundedMultiplyInPlace t, Neg t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t, 
     RefOrd.PartialCompareEffortIndicator t, 
     MultEffortIndicator t) -> 
    t -> t -> Bool
propInOutMultInPlace sample effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    multInEffViaInPlace = mutable2EffToPure (multInInPlaceEff sample)
    multOutEffViaInPlace = mutable2EffToPure (multOutInPlaceEff sample)
    expr1In eff =
        let (>*<) = multInEff eff in e1 >*< e2
    expr1Out eff =
        let (<*>) = multOutEff eff in e1 <*> e2
    expr2In eff =
        let (>*<) = multInEffViaInPlace eff in e1 >*< e2
    expr2Out eff =
        let (<*>) = multOutEffViaInPlace eff in e1 <*> e2

powerToNonnegIntInInPlaceEffFromMult ::
    (RoundedMultiplyInPlace t, HasOne t) =>
    t ->
    OpMutableNonmutEff (PowerToNonnegIntEffortIndicatorFromMult t) t Int s 
powerToNonnegIntInInPlaceEffFromMult sample effMult rM eM n =
    powerFromMultInPlace sample (multInInPlaceEff sample effMult) rM eM n

powerToNonnegIntOutInPlaceEffFromMult ::
    (RoundedMultiplyInPlace t, HasOne t) =>
    t ->
    OpMutableNonmutEff (PowerToNonnegIntEffortIndicatorFromMult t) t Int s 
powerToNonnegIntOutInPlaceEffFromMult sample effMult rM eM n =
    powerFromMultInPlace sample (multOutInPlaceEff sample effMult) rM eM n


class (RoundedPowerToNonnegInt t, CanBeMutable t) => RoundedPowerToNonnegIntInPlace t where
    powerToNonnegIntInInPlaceEff ::
        t -> OpMutableNonmutEff (PowerToNonnegIntEffortIndicator t) t Int s
    powerToNonnegIntOutInPlaceEff ::
        t -> OpMutableNonmutEff (PowerToNonnegIntEffortIndicator t) t Int s
    powerToNonnegIntInInPlaceEff sample =
        pureToMutableNonmutEff sample powerToNonnegIntInEff 
    powerToNonnegIntOutInPlaceEff sample =
        pureToMutableNonmutEff sample powerToNonnegIntOutEff 

propInOutPowerToNonnegInPlace ::
    (RefOrd.PartialComparison t, RoundedPowerToNonnegIntInPlace t, Neg t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (PowerToNonnegIntEffortIndicator t),
     EffortIndicator (PowerToNonnegIntEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t, 
     RefOrd.PartialCompareEffortIndicator t, 
     PowerToNonnegIntEffortIndicator t) -> 
    t -> Int -> Bool
propInOutPowerToNonnegInPlace sample effortDistComp initEffort e1 n =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    powerToNonnegIntInEffViaInPlace = 
        mutableNonmutEffToPure (powerToNonnegIntInInPlaceEff sample)
    powerToNonnegIntOutEffViaInPlace = 
        mutableNonmutEffToPure (powerToNonnegIntOutInPlaceEff sample)
    expr1In eff =
        let (>^<) = powerToNonnegIntInEff eff in e1 >^< n
    expr1Out eff =
        let (<^>) = powerToNonnegIntOutEff eff in e1 <^> n
    expr2In eff =
        let (>^<) = powerToNonnegIntInEffViaInPlace eff in e1 >^< n
    expr2Out eff =
        let (<^>) = powerToNonnegIntOutEffViaInPlace eff in e1 <^> n

class (RoundedDivide t, CanBeMutable t) => RoundedDivideInPlace t where
    divInInPlaceEff :: t -> OpMutable2Eff (DivEffortIndicator t) t s
    divOutInPlaceEff :: t -> OpMutable2Eff (DivEffortIndicator t) t s
    divInInPlaceEff sample = pureToMutable2Eff sample divInEff 
    divOutInPlaceEff sample = pureToMutable2Eff sample divOutEff 

propInOutDivInPlace ::
    (RefOrd.PartialComparison t, RoundedDivideInPlace t, Neg t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t, 
     RefOrd.PartialCompareEffortIndicator t, 
     DivEffortIndicator t) -> 
    t -> t -> Bool
propInOutDivInPlace sample effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    divInEffViaInPlace = mutable2EffToPure (divInInPlaceEff sample)
    divOutEffViaInPlace = mutable2EffToPure (divOutInPlaceEff sample)
    expr1In eff =
        let (>/<) = divInEff eff in e1 >/< e2
    expr1Out eff =
        let (</>) = divOutEff eff in e1 </> e2
    expr2In eff =
        let (>/<) = divInEffViaInPlace eff in e1 >/< e2
    expr2Out eff =
        let (</>) = divOutEffViaInPlace eff in e1 </> e2

testsInOutFieldOpsInPlace (name, sample) =
    testGroup (name ++ " in-place up/down rounded ops match pure ops") $
        [
            testProperty "addition" (propInOutAddInPlace sample)
        ,
            testProperty "subtraction" (propInOutSubtrInPlace sample)
        ,
            testProperty "absolute value" (propInOutAbsInPlace sample)
        ,
            testProperty "multiplication" (propInOutMultInPlace sample)
        ,
            testProperty "integer power" (propInOutMultInPlace sample)
        ,
            testProperty "division" (propInOutDivInPlace sample)
        ]


class (RoundedSubtrInPlace t, RoundedMultiplyInPlace t) => RoundedRingInPlace t
class (RoundedRingInPlace t, RoundedDivideInPlace t) => RoundedFieldInPlace t

    