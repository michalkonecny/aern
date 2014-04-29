{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Measures
    Description :  measures of quality for approximations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Measures of quality for approximations.
-}
module Numeric.AERN.RealArithmetic.Measures where

import {-# Source #-} Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps

import qualified Numeric.AERN.NumericOrder as NumOrd

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Effort

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
   Ability to measure a distance.  Distance
   should be a numeric type approximating the
   positive real numbers with Partial comparison.
-}
class
    (EffortIndicator (DistanceEffortIndicator t))
    => 
    HasDistance t 
    where
    type Distance t
    type DistanceEffortIndicator t
    distanceDefaultEffort :: t -> (DistanceEffortIndicator t)
    {-| distance measure -}
    distanceBetweenEff :: 
        DistanceEffortIndicator t -> t -> t -> Distance t

propDistanceTriangular :: 
    (HasDistance t, 
     NumOrd.PartialComparison (Distance t),
     RoundedAdd (Distance t)
    ) =>
    t ->
    (DistanceEffortIndicator t) ->     
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (AddEffortIndicator (Distance t)) ->     
    t -> t -> t -> Bool
propDistanceTriangular _ effortDist effortComp effortAdd e1 e2 e3 =
    let (>=?) = NumOrd.pGeqEff effortComp in 
    let (<+>) = addOutEff effortAdd in 
        let
        d12 = distanceBetweenEff effortDist e1 e2
        d23 = distanceBetweenEff effortDist e2 e3
        d13 = distanceBetweenEff effortDist e1 e3
        in
        case (d12 <+> d23) >=? d13 of
            Nothing -> True
            Just b -> b

testsDistance ::
    (HasDistance t, 
     NumOrd.PartialComparison (Distance t),
     RoundedAdd (Distance t),
     Arbitrary (NumOrd.PartialCompareEffortIndicator (Distance t)), 
     Show (NumOrd.PartialCompareEffortIndicator (Distance t)),
     Arbitrary (AddEffortIndicator (Distance t)), 
     Show (AddEffortIndicator (Distance t)),
     Arbitrary (DistanceEffortIndicator t), 
     Show (DistanceEffortIndicator t), 
     Arbitrary t, Show t) =>
    (String, t) -> Test
testsDistance (name, sample) =
    testGroup (name ++ " distance measure") $ 
        [
         testProperty "triangle inequality" (propDistanceTriangular sample)
        ]

{-|
   A numeric measure of imprecision of approximations.
   The imprecision type should support Partial comparison.
   A zero imprecision usually means that the approximation is exact.
-}
class HasImprecision t where
    type Imprecision t
    type ImprecisionEffortIndicator t
    imprecisionDefaultEffort :: t -> ImprecisionEffortIndicator t
    imprecisionOfEff :: ImprecisionEffortIndicator t -> t -> Imprecision t

propImprecisionDecreasesWithRefinement ::
    (HasImprecision t, NumOrd.PartialComparison (Imprecision t)) =>
    t -> 
    (ImprecisionEffortIndicator t) -> 
    (NumOrd.PartialCompareEffortIndicator (Imprecision t)) -> 
    RefOrd.LEPair t -> Bool
propImprecisionDecreasesWithRefinement _ effortImpr effortComp (RefOrd.LEPair (e1,e2)) =
    let (>=?) = NumOrd.pGeqEff effortComp in
    case (imprecisionOfEff effortImpr e1) >=? (imprecisionOfEff effortImpr e2) of
        Nothing -> True
        Just b -> b

testsImprecision (name, sample) =
    testGroup (name ++ " imprecision measure") $ 
        [
         testProperty "decreases with refinement" (propImprecisionDecreasesWithRefinement sample)
        ]

instance 
    (HasImprecision t,
     NumOrd.RefinementRoundedLattice (Imprecision t)) 
    => 
    HasImprecision [t]
    where
    type Imprecision [t] = Imprecision t
    type ImprecisionEffortIndicator [t] = 
        (ImprecisionEffortIndicator t, NumOrd.MinmaxInOutEffortIndicator (Imprecision t))
    imprecisionDefaultEffort (h : _) =
        (effImpr, NumOrd.minmaxInOutDefaultEffort (imprecisionOfEff effImpr h))
        where
        effImpr = imprecisionDefaultEffort h
    imprecisionOfEff (effImpr, effMax) list@(_:_) =
        foldl1 (NumOrd.maxOutEff effMax) $ map (imprecisionOfEff effImpr) list
--    isExactEff (eff, _) list =
--        fmap and $ sequence $ map (isExactEff eff) list
         
         
{-|
    A generic function that applies a given approximate function
    repeatedly with increasing effort until the required accuracy
    is reached.
-}
iterateUntilAccurate :: 
    (HasImprecision t,
    NumOrd.PartialComparison (Imprecision t),
    EffortIndicator eff) 
    =>
    Int {-^ @iterLimit@ maximum number of different efforts to try out -} -> 
    Imprecision t {-^ @maxImprecision@ imprecision threshold for the result -} -> 
    eff {-^ @initEff@ the initial effort -} -> 
    (eff -> t) {-^ the function to compute -} -> 
    [(eff,t)] {-^ the efforts that were tried and the corresponding results -}
iterateUntilAccurate iterLimit maxImprecision initEff fn =
    stopWhenAccurate $ 
        take iterLimit $ 
            zip efforts (map fn efforts)
    where
    efforts = effortIncrementSequence initEff

    stopWhenAccurate [] = []
    stopWhenAccurate ((eff, result) : rest)
        | accurateEnough = [(eff, result)]
        | otherwise = (eff, result) : (stopWhenAccurate rest)
        where
        accurateEnough =
            (maxImprecision NumOrd.>=? resultImprecision) == Just True
        resultImprecision =
            imprecisionOfEff effImpr result
        effImpr =
            imprecisionDefaultEffort result
         
