{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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

import {-# Source #-} Numeric.AERN.RealArithmetic.RefinementOrderRounding.RoundedOps

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
    let 
    ?pCompareEffort = effortComp 
    ?addInOutEffort = effortAdd 
    in
        let
        d12 = distanceBetweenEff effortDist e1 e2
        d23 = distanceBetweenEff effortDist e2 e3
        d13 = distanceBetweenEff effortDist e1 e3
        in
        case (d12 <+> d23) NumOrd.>=? d13 of
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
   A zero imprecision means the approximation is exact.
   The imprecision type should support Partial comparison.
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
    let 
    ?pCompareEffort = effortComp 
    in
    case (imprecisionOfEff effortImpr e1) NumOrd.>=? (imprecisionOfEff effortImpr e2) of
        Nothing -> True
        Just b -> b

testsImprecision (name, sample) =
    testGroup (name ++ " imprecision measure") $ 
        [
         testProperty "decreases with refinement" (propImprecisionDecreasesWithRefinement sample)
        ]

        
