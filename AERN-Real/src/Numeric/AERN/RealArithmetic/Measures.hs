{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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

import {-# Source #-} Numeric.AERN.RealArithmetic.NumericOrderRounding.RoundedRing

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
   Ability to measure a distance.  Distance
   should be a numeric type approximating the
   positive real numbers with Partial comparison.
-}
class HasDistance t where
    type Distance t
    {-| distance measure -}
    distanceBetween :: t -> t -> Distance t

propDistanceTriangular :: 
    (HasDistance t, 
     NumOrd.PartialComparison (Distance t),
     RoundedAdd (Distance t)
    ) =>
    t -> 
    t -> t -> t -> Bool
propDistanceTriangular _ e1 e2 e3 =
    case (d12 +. d23) NumOrd.<=? d13 of
        Nothing -> True
        Just b -> b
    where
    d12 = e1 `distanceBetween` e2
    d23 = e2 `distanceBetween` e3
    d13 = e1 `distanceBetween` e3

testsDistance ::
    (Arbitrary t, HasDistance t, 
     NumOrd.PartialComparison (Distance t),
     RoundedAdd (Distance t),
     Show t) =>
    (String, t) -> Test
testsDistance (name, sample) =
    testGroup (name ++ " distance measure") $ 
        [
         testProperty "triangle inequality" (propDistanceTriangular sample)
        ]

{-|
   A numeric measure of imprecision of approximations.
   A zero imprecision means the approximation is exact.
   The imprecision type should support Partial comparison.
-}
class HasImprecision t where
    type Imprecision t
    imprecisionOf :: t -> Imprecision t

propImprecisionDecreasesWithRefinement ::
    (HasImprecision t, NumOrd.PartialComparison (Imprecision t)) =>
    t -> RefOrd.LEPair t -> Bool
propImprecisionDecreasesWithRefinement _ (RefOrd.LEPair (e1,e2)) =
    case (imprecisionOf e1) NumOrd.>=? (imprecisionOf e2) of
        Nothing -> True
        Just b -> b

testsImprecision (name, sample) =
    testGroup (name ++ " imprecision measure") $ 
        [
         testProperty "decreases with refinement" (propImprecisionDecreasesWithRefinement sample)
        ]

        