{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double
    Description :  instances of Double required for serving as interval endpoints  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.RealArithmetic.Basis.Double where

import Prelude hiding (EQ,LT,GT)

import Numeric.AERN.Basics.Exception
import Control.Exception

import Numeric.AERN.Basics.Granularity
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Data.Maybe
import Data.List (sort)
import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

instance HasGranularity Double where
    type Granularity Double = Int
    getGranularity _ = 53

propDoubleEqReflexive :: Double -> Bool
propDoubleEqReflexive = propEqReflexive 

propDoubleEqSymmetric :: Double -> Double -> Bool
propDoubleEqSymmetric = propEqSymmetric

propDoubleEqTransitive :: Double -> Double -> Double -> Bool
propDoubleEqTransitive = propEqTransitive

testsDoubleEq =
    testGroup "Double (==)" 
        [
         testProperty "reflexive" propDoubleEqReflexive
        ,
         testProperty "symmetric" propDoubleEqSymmetric
        ,
         testProperty "transitive" propDoubleEqTransitive
        ]

instance SemidecidableEq Double where
    maybeEqualEff _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Just $ a == b  
           _ -> throw (AERNException $ "illegal Double argument: maybeEqual " 
                        ++ show a ++ " " ++ show b)
    maybeEqualDefaultEffort _ = []

propDoubleSemidecidableEqReflexive :: Double -> Bool
propDoubleSemidecidableEqReflexive = propSemidecidableEqReflexive

propDoubleSemidecidableEqSymmetric :: Double -> Double -> Bool
propDoubleSemidecidableEqSymmetric = propSemidecidableEqSymmetric

propDoubleSemidecidableEqTransitive :: Double -> Double -> Double -> Bool
propDoubleSemidecidableEqTransitive = propSemidecidableEqTransitive

testsDoubleSemidecidableEq =
    testGroup "Double (==?)" 
        [
         testProperty "reflexive" propDoubleSemidecidableEqReflexive
        ,
         testProperty "symmetric" propDoubleSemidecidableEqSymmetric
        ,
         testProperty "transitive" propDoubleSemidecidableEqTransitive
        ]

instance NumOrd.HasLeast Double where
    least = - 1/0

instance NumOrd.HasHighest Double where
    highest = 1/0

instance NumOrd.HasExtrema Double where

instance NumOrd.SemidecidablePoset Double where
    maybeCompareEff _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Just $ toPartialOrdering $ Prelude.compare a b  
           (True, True) -> Just EQ
           _ -> Just NC 
    maybeCompareDefaultEffort _ = []

propDoubleSemidecidablePosetEqCompatible :: Double -> Double -> Bool
propDoubleSemidecidablePosetEqCompatible = NumOrd.propSemidecidablePosetEqCompatible

propDoubleSemidecidablePosetAntiSymmetric :: Double -> Double -> Bool
propDoubleSemidecidablePosetAntiSymmetric = NumOrd.propSemidecidablePosetAntiSymmetric

propDoubleSemidecidablePosetTransitive :: Double -> Double -> Double -> Bool
propDoubleSemidecidablePosetTransitive = NumOrd.propSemidecidablePosetTransitive

propDoubleExtremaInSemidecidablePoset :: Double -> Bool
propDoubleExtremaInSemidecidablePoset = NumOrd.propExtremaInSemidecidablePoset

testsDoubleSemidecidablePoset =
    testGroup "Double (>=?)" 
        [
         testProperty "equality compatible" propDoubleSemidecidablePosetEqCompatible
        ,
         testProperty "anti symmetric" propDoubleSemidecidablePosetAntiSymmetric
        ,
         testProperty "transitive" propDoubleSemidecidablePosetTransitive
        ,
         testProperty "extrema" propDoubleExtremaInSemidecidablePoset
        ]

instance NumOrd.Poset Double where
    compare a b =
        case (isNaN a, isNaN b) of
           (False, False) -> toPartialOrdering $ Prelude.compare a b  
           _ -> throw (AERNException $ "illegal Double argument: NumOrd.Poset.compare " 
                        ++ show a ++ " " ++ show b) 

propDoublePosetEqCompatible :: Double -> Double -> Bool
propDoublePosetEqCompatible = NumOrd.propPosetEqCompatible

propDoublePosetAntiSymmetric :: UniformlyOrderedPair Double -> Bool
propDoublePosetAntiSymmetric = NumOrd.propPosetAntiSymmetric

propDoublePosetTransitive :: Double -> Double -> Double -> Bool
propDoublePosetTransitive = NumOrd.propPosetTransitive

propDoubleExtremaInPoset :: Double -> Bool
propDoubleExtremaInPoset = NumOrd.propExtremaInPoset

testsDoublePoset =
    testGroup "Double (>=)" 
        [
         testProperty "equality compatible" propDoublePosetEqCompatible
        ,
         testProperty "anti symmetric" propDoublePosetAntiSymmetric
        ,
         testProperty "transitive" propDoublePosetTransitive
        ,
         testProperty "extrema" propDoubleExtremaInPoset
        ]

instance NumOrd.Lattice Double where
    max a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Prelude.max a b  
           _ -> throw (AERNException $ "illegal Double argument: NumOrd.Lattice.max " 
                        ++ show a ++ " " ++ show b) 
    min a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Prelude.min a b  
           _ -> throw (AERNException $ "illegal Double argument: NumOrd.Lattice.min " 
                        ++ show a ++ " " ++ show b) 
    
instance NumOrd.RoundedLattice Double where
    maxUpEff _ = max
    maxDnEff _ = max
    minUpEff _ = min
    minDnEff _ = min
    minmaxDefaultEffort _ = []

instance ArbitraryOrderedTuple Double where
    {--------------- pairs --------------}
    arbitraryPairRelatedBy EQ = Just $
        do
        d <- arbitrary
        return (d,d)
    arbitraryPairRelatedBy LT = Just gen
        where
        gen =
            do
            (d1u, d2u) <- incrSize arbitrary
            let [d1,d2] = sort [d1u,d2u]
            if d1 < d2
                then return (d1,d2)
                else  gen -- bad luck, try again
    arbitraryPairRelatedBy GT = Just $
        do
        (d1,d2) <- fromJust $ arbitraryPairRelatedBy LT
        return (d2,d1)
    arbitraryPairRelatedBy NC = Nothing
--        Just gen
--        where
--        gen =
--            do
--            d <- arbitrary
--            case isNaN d of
--               True -> gen -- bad luck, try again
--               _ -> elements [(nan,d), (d,nan)]
--        nan = 0/0
    {--------------- no NC allowed in triples ----------------}
    arbitraryTripleRelatedBy (NC,_ ,_ ) = Nothing
    arbitraryTripleRelatedBy (_ ,NC,_ ) = Nothing
    arbitraryTripleRelatedBy (_ ,_ ,NC) = Nothing
    {--------------- triples with some equality --------------}
    arbitraryTripleRelatedBy (EQ, r2, r3) | r2 == r3 = Just $ -- e1 = e2
        do
        (d2,d3) <- fromJust $ arbitraryPairRelatedBy r2
        return (d2,d2,d3)
    arbitraryTripleRelatedBy (r1, EQ, r3) | r1 == r3 = Just $
        do
        (d1,d3) <- fromJust $ arbitraryPairRelatedBy r3
        return (d1,d3,d3)
    arbitraryTripleRelatedBy (r1, r2, EQ) | r1 == partialOrderingTranspose r2 = Just $
        do
        (d1,d2) <- fromJust $ arbitraryPairRelatedBy r1
        return (d1,d2,d1)
--    {--------------- triples with some NaN but no equality -------}
--    arbitraryTripleRelatedBy (NC, NC, NC) = Nothing -- Double consists of 2 linearly ordered components
--    arbitraryTripleRelatedBy (NC, NC, r3) = Just $ -- e2 = NaN
--        do
--        (d1,d3) <- fromJust $ arbitraryPairRelatedBy r3
--        return (d1,0/0,d3)
--    arbitraryTripleRelatedBy (NC, r2, NC) = Just $ -- e1 = NaN
--        do
--        (d2,d3) <- fromJust $ arbitraryPairRelatedBy r2
--        return (0/0,d2,d3)
--    arbitraryTripleRelatedBy (r1, NC, NC) = Just $ -- e3 = NaN
--        do
--        (d1,d2) <- fromJust $ arbitraryPairRelatedBy r1
--        return (d1,d2,0/0)
--    -- cannot have single NC because it forces NaN which forces two NCs 
--    -- (except equality which was sorted out earlier)
    {--------------- strictly ordered triples ----------}
    arbitraryTripleRelatedBy (LT, LT, LT) = Just gen
        where
        gen =
            do
            (d1u, d2u, d3u) <- incrSize arbitrary
            let [d1,d2,d3] = sort [d1u,d2u,d3u] 
            if d1 < d2 && d2 < d3 
                then return (d1,d2,d3)
                else gen -- bad luck, try again
    arbitraryTripleRelatedBy (LT, GT, LT) = Just $
        do
        (d1,d2,d3) <- fromJust $ arbitraryTripleRelatedBy (LT, LT, LT)
        return (d1,d3,d2)
    arbitraryTripleRelatedBy (GT, LT, LT) = Just $
        do
        (d1,d2,d3) <- fromJust $ arbitraryTripleRelatedBy (LT, LT, LT)
        return (d2,d1,d3)
    arbitraryTripleRelatedBy (GT, LT, GT) = Just $
        do
        (d1,d2,d3) <- fromJust $ arbitraryTripleRelatedBy (LT, LT, LT)
        return (d3,d1,d2)
    arbitraryTripleRelatedBy (LT, GT, GT) = Just $
        do
        (d1,d2,d3) <- fromJust $ arbitraryTripleRelatedBy (LT, LT, LT)
        return (d2,d3,d1)
    arbitraryTripleRelatedBy (GT, GT, GT) = Just $
        do
        (d1,d2,d3) <- fromJust $ arbitraryTripleRelatedBy (LT, LT, LT)
        return (d3,d2,d1)


incrSize gen = sized (\size -> resize (size + 1) gen)
    