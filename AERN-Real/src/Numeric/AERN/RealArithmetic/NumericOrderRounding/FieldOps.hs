{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.RoundedOps
    Description :  rounded basic arithmetic operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps 
(
    RoundedAdd(..), RoundedSubtr(..), testsUpDnAdd, testsUpDnSubtr,
    RoundedAbs(..), testsUpDnAbs, absUpUsingCompMax, absDnUsingCompMax,
    RoundedMultiply(..), testsUpDnMult,
    RoundedPowerNonnegToNonnegInt(..),
    PowerNonnegToNonnegIntEffortIndicatorFromMult, 
    powerNonnegToNonnegIntDefaultEffortFromMult,
    powerNonnegToNonnegIntUpEffFromMult,
    powerNonnegToNonnegIntDnEffFromMult,
    RoundedPowerToNonnegInt(..), testsUpDnIntPower, 
    PowerToNonnegIntEffortIndicatorFromMult, 
    powerToNonnegIntDefaultEffortFromMult,
    powerToNonnegIntUpEffFromMult,
    powerToNonnegIntDnEffFromMult,
    RoundedDivide(..), testsUpDnDiv,
    RoundedRing, RoundedField
)
where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Conversion

import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.Maybe

class RoundedAdd t where
    type AddEffortIndicator t
    addUpEff :: AddEffortIndicator t -> t -> t -> t
    addDnEff :: AddEffortIndicator t -> t -> t -> t
    addDefaultEffort :: t -> AddEffortIndicator t

propUpDnAddZero ::
    (NumOrd.PartialComparison t, RoundedAdd t, HasZero t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
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
    (NumOrd.PartialComparison t, RoundedAdd t, HasZero t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
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
       
propUpDnAddAssociative ::
    (NumOrd.PartialComparison t, RoundedAdd t, HasZero t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.PartialCompareEffortIndicator t, AddEffortIndicator t) -> 
    t -> t -> t -> Bool
propUpDnAddAssociative _ effortDist =
    roundedImprovingAssociative NumOrd.pLeqEff (distanceBetweenEff effortDist) addUpEff addDnEff

testsUpDnAdd (name, sample) =
    testGroup (name ++ " +. +^") $
        [
            testProperty "0 absorbs" (propUpDnAddZero sample)
        ,
            testProperty "commutative" (propUpDnAddCommutative sample)
        ,
            testProperty "associative" (propUpDnAddAssociative sample)
        ]
        
class (RoundedAdd t, Neg t) => RoundedSubtr t where
    subtrUpEff :: (AddEffortIndicator t) -> t -> t -> t
    subtrDnEff :: (AddEffortIndicator t) -> t -> t -> t
    subtrUpEff effort a b = addUpEff effort a (neg b)
    subtrDnEff effort a b = addDnEff effort a (neg b)

propUpDnSubtrElim ::
    (NumOrd.PartialComparison t, RoundedSubtr t, HasZero t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
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
propUpDnSubtrElim _ effortDist =
    roundedImprovingReflexiveCollapse zero NumOrd.pLeqEff (distanceBetweenEff effortDist) subtrUpEff subtrDnEff

propUpDnSubtrNegAdd ::
    (NumOrd.PartialComparison t, RoundedSubtr t, Neg t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
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
propUpDnSubtrNegAdd _ effortDist effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1Up eff =
        let (-^) = subtrUpEff eff in e1 -^ (neg e2)
    expr1Dn eff =
        let (-.) = subtrDnEff eff in e1 -. (neg e2)
    expr2Up eff =
        let (+^) = addUpEff eff in e1 +^ e2
    expr2Dn eff =
        let (+.) = addDnEff eff in e1 +. e2


testsUpDnSubtr (name, sample) =
    testGroup (name ++ " -. -^") $
        [
            testProperty "a-a=0" (propUpDnSubtrElim sample)
            ,
            testProperty "a+b=a-(-b)" (propUpDnSubtrNegAdd sample)
        ]

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

propUpDnAbsNegSymmetric ::
    (NumOrd.PartialComparison t, RoundedAbs t, HasZero t,
     Show t,
     HasDistance t,  Show (Distance t), Neg t,
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (AbsEffortIndicator t),
     EffortIndicator (AbsEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.PartialCompareEffortIndicator t, AbsEffortIndicator t) -> 
    t -> Bool
propUpDnAbsNegSymmetric _ effortDist =
    roundedImprovingNegSymmetric NumOrd.pLeqEff (distanceBetweenEff effortDist) absUpEff absDnEff

propUpDnAbsIdempotent ::
    (NumOrd.PartialComparison t, RoundedAbs t, HasZero t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (AbsEffortIndicator t),
     EffortIndicator (AbsEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.PartialCompareEffortIndicator t, AbsEffortIndicator t) -> 
    t -> Bool
propUpDnAbsIdempotent _ effortDist =
    roundedImprovingIdempotent NumOrd.pLeqEff (distanceBetweenEff effortDist) absUpEff absDnEff

testsUpDnAbs (name, sample) =
    testGroup (name ++ " up/dn rounded abs") $
        [
            testProperty "neg -> no change" (propUpDnAbsNegSymmetric sample)
        ,
            testProperty "idempotent" (propUpDnAbsIdempotent sample)
        ]


class RoundedMultiply t where
    type MultEffortIndicator t
    multUpEff :: MultEffortIndicator t -> t -> t -> t
    multDnEff :: MultEffortIndicator t -> t -> t -> t
    multDefaultEffort :: t -> MultEffortIndicator t

class (RoundedAdd t, RoundedSubtr t, RoundedMultiply t) => RoundedRing t

propUpDnMultOne ::
    (NumOrd.PartialComparison t, RoundedMultiply t, HasOne t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.PartialCompareEffortIndicator t, MultEffortIndicator t) -> 
    t -> Bool
propUpDnMultOne _ effortDist =
    roundedImprovingUnit one NumOrd.pLeqEff (distanceBetweenEff effortDist) multUpEff multDnEff

propUpDnMultCommutative ::
    (NumOrd.PartialComparison t, RoundedMultiply t, HasZero t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.PartialCompareEffortIndicator t, MultEffortIndicator t) -> 
    t -> t -> Bool
propUpDnMultCommutative _ effortDist =
    roundedImprovingCommutative NumOrd.pLeqEff (distanceBetweenEff effortDist) multUpEff multDnEff
       
propUpDnMultAssociative ::
    (NumOrd.PartialComparison t, NumOrd.RoundedLattice t, 
     Show t,
     RoundedMultiply t, HasZero t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.MinmaxEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator t, MultEffortIndicator t) -> 
    t -> t -> t -> Bool
propUpDnMultAssociative _ effortDist effortDistComp minmaxEffort initEffort e1 e2 e3 =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1Up eff =
        let (*^) = multUpEff eff; (*.) = multDnEff eff in
        let r1 = e1 *^ (e2 *^ e3) in
        let r2 = e1 *^ (e2 *. e3) in
        NumOrd.maxUpEff minmaxEffort r1 r2
    expr1Dn eff =
        let (*^) = multUpEff eff; (*.) = multDnEff eff in
        let r1 = e1 *. (e2 *^ e3) in
        let r2 = e1 *. (e2 *. e3) in
        NumOrd.minDnEff minmaxEffort r1 r2
    expr2Up eff =
        let (*^) = multUpEff eff; (*.) = multDnEff eff in
        let r1 = (e1 *^ e2) *^ e3 in
        let r2 = (e1 *. e2) *^ e3 in
        NumOrd.maxUpEff minmaxEffort r1 r2
    expr2Dn eff =
        let (*^) = multUpEff eff; (*.) = multDnEff eff in
        let r1 = (e1 *^ e2) *. e3 in
        let r2 = (e1 *. e2) *. e3 in
        NumOrd.minDnEff minmaxEffort r1 r2

propUpDnMultDistributesOverAdd ::
    (NumOrd.PartialComparison t, NumOrd.RoundedLattice t,
     Show t,
     RoundedMultiply t,  RoundedAdd t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.MinmaxEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator t, (MultEffortIndicator t, AddEffortIndicator t)) -> 
    t -> t -> t -> Bool
propUpDnMultDistributesOverAdd _ effortDist effortDistComp minmaxEffort initEffort e1 e2 e3 =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1Up (effMult, effAdd) =
        let (*^) = multUpEff effMult in
        let (+^) = addUpEff effAdd; (+.) = addDnEff effAdd in
        let r1 = e1 *^ (e2 +^ e3) in
        let r2 = e1 *^ (e2 +. e3) in
        NumOrd.maxUpEff minmaxEffort r1 r2
    expr1Dn (effMult, effAdd) =
        let (*.) = multDnEff effMult in
        let (+^) = addUpEff effAdd; (+.) = addDnEff effAdd in
        let r1 = e1 *. (e2 +^ e3) in
        let r2 = e1 *. (e2 +. e3) in
        NumOrd.minDnEff minmaxEffort r1 r2
    expr2Up (effMult, effAdd) =
        let (*^) = multUpEff effMult in
        let (+^) = addUpEff effAdd in
        (e1 *^ e2) +^ (e1 *^ e3)
    expr2Dn (effMult, effAdd) =
        let (*.) = multDnEff effMult in
        let (+.) = addDnEff effAdd in
        (e1 *. e2) +. (e1 *. e3)
       
    
testsUpDnMult (name, sample) =
    testGroup (name ++ " *. *^") $
        [
            testProperty "1 absorbs" (propUpDnMultOne sample)
        ,
            testProperty "commutative" (propUpDnMultCommutative sample)
        ,
            testProperty "associative" (propUpDnMultAssociative sample)
        ,
            testProperty "distributes over +" (propUpDnMultDistributesOverAdd sample)
        ]

-- simpler versions assuming the argument is non-negative:
class RoundedPowerNonnegToNonnegInt t where
    type PowerNonnegToNonnegIntEffortIndicator t
    powerNonnegToNonnegIntDefaultEffort :: 
        t -> PowerNonnegToNonnegIntEffortIndicator t 
    powerNonnegToNonnegIntUpEff :: 
        (PowerNonnegToNonnegIntEffortIndicator t) -> 
        t {-^ @x@ (assumed >=0) -} -> 
        Int {-^ @n@ (assumed >=0)-} -> 
        t {-^ @x^n@ rounded up -}
    powerNonnegToNonnegIntDnEff ::
        (PowerNonnegToNonnegIntEffortIndicator t) -> 
        t {-^ @x@ (assumed >=0) -} -> 
        Int {-^ @n@ (assumed >=0)-} -> 
        t {-^ @x^n@ rounded down -}
        
-- functions providing an implementation derived from rounded multiplication: 
        
type PowerNonnegToNonnegIntEffortIndicatorFromMult t =
    MultEffortIndicator t
    
powerNonnegToNonnegIntDefaultEffortFromMult a =
    multDefaultEffort a

powerNonnegToNonnegIntUpEffFromMult ::
    (RoundedMultiply t, HasOne t) => 
    PowerNonnegToNonnegIntEffortIndicatorFromMult t -> 
    t -> Int -> t
powerNonnegToNonnegIntUpEffFromMult effMult e n =
    powerNonneg (multUpEff effMult) e n

powerNonnegToNonnegIntDnEffFromMult ::
    (RoundedMultiply t, HasOne t) => 
    PowerNonnegToNonnegIntEffortIndicatorFromMult t -> 
    t -> Int -> t
powerNonnegToNonnegIntDnEffFromMult effMult e n =
    powerNonneg (multDnEff effMult) e n

powerNonneg mult x n = p n -- assuming x >= 0
    where
    p n
        | n == 1 = x
        | otherwise =
            case even n of
                True -> 
                    powHalf `mult` powHalf 
                False -> 
                    x `mult` (powHalf `mult` powHalf)
        where
        powHalf = p (n `div` 2)

-- now not assuming the argument is non-negative:
class RoundedPowerToNonnegInt t where
    type PowerToNonnegIntEffortIndicator t
    powerToNonnegIntDefaultEffort :: 
        t -> PowerToNonnegIntEffortIndicator t 
    powerToNonnegIntUpEff ::
        (PowerToNonnegIntEffortIndicator t) -> 
        t {-^ @x@ -} -> 
        Int {-^ @n@ (assumed >=0)-} -> 
        t {-^ @x^n@ rounded up -}
    powerToNonnegIntDnEff ::
        (PowerToNonnegIntEffortIndicator t) -> 
        t {-^ @x@ -} -> 
        Int {-^ @n@ (assumed >=0)-} -> 
        t {-^ @x^n@ rounded down -}

-- functions providing an implementation derived from roudned multiplication: 

type PowerToNonnegIntEffortIndicatorFromMult t =
    (MultEffortIndicator t, 
     NumOrd.PartialCompareEffortIndicator t, 
     NumOrd.MinmaxEffortIndicator t)
     
powerToNonnegIntDefaultEffortFromMult a =
    (multDefaultEffort a,
     NumOrd.pCompareDefaultEffort a,
     NumOrd.minmaxDefaultEffort a)

powerToNonnegIntUpEffFromMult :: 
    (RoundedMultiply t, HasOne t, 
     NumOrd.PartialComparison t, HasZero t, 
     Neg t, NumOrd.RoundedLattice t) => 
    PowerToNonnegIntEffortIndicatorFromMult t ->
    t -> Int -> t
powerToNonnegIntUpEffFromMult (effMult, effComp, effMinmax) e n =
    powerToNonnegIntDir
        (multUpEff effMult) (multDnEff effMult)
        (NumOrd.maxUpEff effMinmax)
        effComp e n

powerToNonnegIntDnEffFromMult :: 
    (RoundedMultiply t, HasOne t, 
     NumOrd.PartialComparison t, HasZero t, 
     Neg t, NumOrd.RoundedLattice t) => 
    PowerToNonnegIntEffortIndicatorFromMult t ->
    t -> Int -> t
powerToNonnegIntDnEffFromMult (effMult, effComp, effMinmax) e n =
    powerToNonnegIntDir 
        (multDnEff effMult) (multUpEff effMult) 
        (NumOrd.minDnEff effMinmax)
        effComp e n

powerToNonnegIntDir :: 
    (HasOne t, 
     NumOrd.PartialComparison t, HasZero t, 
     Neg t) => 
    (t -> t -> t) {-^ multiplication rounded in the desired direction -} ->
    (t -> t -> t) {-^ multiplication rounded in the opposite direction -} ->
    (t -> t -> t) {-^ safe combination of alternative results -} ->
    (NumOrd.PartialCompareEffortIndicator t) -> 
    t -> Int -> t
powerToNonnegIntDir mult1 mult2 combine effComp x n
    | n == 0 = one
    | n == 1 = x
    | otherwise =
        case (pNonnegNonposEff effComp x) of
            Just (True, _) -> resNonneg
            Just (_, True) -> resNonpos
            _ -> resNonneg `combine` resNonpos
    where
    resNonneg = powerNonneg mult1 x n
    resNonpos 
        | even n = 
            powerNonneg mult1 (neg x) n
        | otherwise = 
            neg $ powerNonneg mult2 (neg x) n 
            -- switching rounding direction

propUpDnPowerSumExponents ::
    (NumOrd.PartialComparison t, NumOrd.RoundedLattice t,
     RoundedPowerToNonnegInt t, RoundedMultiply t, 
     HasOne t, HasZero t, Neg t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (PowerToNonnegIntEffortIndicator t),
     EffortIndicator (PowerToNonnegIntEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (NumOrd.MinmaxEffortIndicator t),
     EffortIndicator (NumOrd.MinmaxEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.PartialCompareEffortIndicator t,
     (PowerToNonnegIntEffortIndicator t,
      (NumOrd.PartialCompareEffortIndicator t,
       MultEffortIndicator t,
       NumOrd.MinmaxEffortIndicator t))) -> 
    t -> Int -> Int -> Bool
propUpDnPowerSumExponents _ effortDist effortDistComp initEffort a nR mR =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    n = nR `mod` 10
    m = mR `mod` 10
    minusA = neg a
    expr1Up (effPower, (effComp, effMult, effMinmax)) =
        let (^^) = powerToNonnegIntUpEff effPower in
        a ^^ (n + m)
    expr1Dn (effPower, (effComp, effMult, effMinmax)) =
        let (^.) = powerToNonnegIntDnEff effPower in
        a ^. (n + m)
    expr2Up (effPower, (effComp, effMult, effMinmax)) =
        case pNonnegNonposEff effComp a of
            Just (True, _) -> rNonneg
            Just (_, True) -> rNonpos
            _ -> rNonneg `max` rNonpos
        where
        max = NumOrd.maxUpEff effMinmax
        (^^) = powerToNonnegIntUpEff effPower
        (^.) = powerToNonnegIntDnEff effPower
        (*^) = multUpEff effMult
        (*.) = multDnEff effMult
        rNonneg = (a ^^ n) *^ (a ^^ m)
        rNonpos =
            case (even (n + m)) of
                True -> (minusA ^^ n) *^ (minusA ^^ m)
                False -> neg $ (minusA ^. n) *. (minusA ^. m)
    expr2Dn (effPower, (effComp, effMult, effMinmax)) =
        case pNonnegNonposEff effComp a of
            Just (True, _) -> rNonneg
            Just (_, True) -> rNonpos
            _ -> rNonneg `min` rNonpos
        where
        min = NumOrd.minDnEff effMinmax
        (^^) = powerToNonnegIntUpEff effPower
        (^.) = powerToNonnegIntDnEff effPower
        (*^) = multUpEff effMult
        (*.) = multDnEff effMult
        rNonneg = (a ^. n) *. (a ^. m)
        rNonpos =
            case (even (n + m)) of
                True -> (minusA ^. n) *. (minusA ^. m)
                False -> neg $ (minusA ^^ n) *^ (minusA ^^ m)

testsUpDnIntPower (name, sample) =
    testGroup (name ++ " non-negative integer power") $
        [
            testProperty "a^(n+m) = a^n * a^m" (propUpDnPowerSumExponents sample)
--            ,
--            testProperty "a/b=a*(1/b)" (propUpDnDivRecipMult sample)
        ]

class RoundedDivide t where
    type DivEffortIndicator t
    divUpEff :: DivEffortIndicator t -> t -> t -> t
    divDnEff :: DivEffortIndicator t -> t -> t -> t
    divDefaultEffort :: t -> DivEffortIndicator t

class (RoundedRing t, RoundedDivide t) => RoundedField t

propUpDnDivElim ::
    (NumOrd.PartialComparison t, RoundedDivide t, HasOne t, HasZero t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.PartialCompareEffortIndicator t, DivEffortIndicator t) -> 
    t -> Bool
propUpDnDivElim _ effortDist effortCompDist efforts2 a =
    roundedImprovingReflexiveCollapse 
        one 
        NumOrd.pLeqEff (distanceBetweenEff effortDist) 
        divUpEff divDnEff 
        effortCompDist efforts2 
        a

propUpDnDivRecipMult ::
    (NumOrd.PartialComparison t, NumOrd.RoundedLattice t,
     Show t,
     RoundedMultiply t, RoundedDivide t, HasOne t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), HasInfinities (Distance t), HasZero (Distance t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (DistanceEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (NumOrd.MinmaxEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator t, (MultEffortIndicator t, DivEffortIndicator t)) -> 
    t -> t -> Bool
propUpDnDivRecipMult _ effortDist effortDistComp minmaxEffort initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1Up (effMult, effDiv) =
        let (*^) = multUpEff effMult in
        let (/^) = divUpEff effDiv; (/.) = divDnEff effDiv in
        let r1 = e1 *^ (one /^ e2) in
        let r2 = e1 *^ (one /. e2) in
        NumOrd.maxUpEff minmaxEffort r1 r2
    expr1Dn (effMult, effDiv) =
        let (*.) = multDnEff effMult in
        let (/^) = divUpEff effDiv; (/.) = divDnEff effDiv in
        let r1 = e1 *. (one /^ e2) in
        let r2 = e1 *. (one /. e2) in
        NumOrd.minDnEff minmaxEffort r1 r2
    expr2Up (effMult, effDiv) =
        let (/^) = divUpEff effDiv in
        e1 /^ e2
    expr2Dn (effMult, effDiv) =
        let (/.) = divDnEff effDiv in
        e1 /. e2

testsUpDnDiv (name, sample) =
    testGroup (name ++ " /. /^") $
        [
            testProperty "a/a=1" (propUpDnDivElim sample)
            ,
            testProperty "a/b=a*(1/b)" (propUpDnDivRecipMult sample)
        ]
    