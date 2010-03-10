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

import Prelude hiding (EQ)

import Numeric.AERN.Basics.Granularity
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

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
    maybeEqual _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Just $ a == b  
           (True, True) -> Just True
           _ -> Just False
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
    maybeCompare _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Just $ toPartialOrdering $ Prelude.compare a b  
           (True, True) -> Just EQ
           _ -> Just NC 
    maybeCompareDefaultEffort _ = []

--propDoubleSemidecidablePosetEqCompatible :: Double -> Double -> Bool
--propDoubleSemidecidablePosetEqCompatible = NumOrd.propSemidecidablePosetEqCompatible
--
--propDoubleSemidecidablePosetAntiSymmetric :: Double -> Double -> Bool
--propDoubleSemidecidablePosetAntiSymmetric = NumOrd.propSemidecidablePosetAntiSymmetric
--
--propDoubleSemidecidablePosetTransitive :: Double -> Double -> Double -> Bool
--propDoubleSemidecidablePosetTransitive = NumOrd.propSemidecidablePosetTransitive

propDoubleExtremaInSemidecidablePoset :: Double -> Bool
propDoubleExtremaInSemidecidablePoset = NumOrd.propExtremaInSemidecidablePoset

testsDoubleSemidecidablePoset =
    testGroup "Double (>=?)" 
        [
--         testProperty "equality compatible" propDoubleSemidecidablePosetEqCompatible
--        ,
--         testProperty "anti symmetric" propDoubleSemidecidablePosetAntiSymmetric
--        ,
--         testProperty "transitive" propDoubleSemidecidablePosetTransitive
--        ,
         testProperty "extrema" propDoubleExtremaInSemidecidablePoset
        ]

instance NumOrd.Poset Double where
    compare a b =
        case (isNaN a, isNaN b) of
           (False, False) -> toPartialOrdering $ Prelude.compare a b  
           (True, True) -> EQ
           _ -> NC 

propDoublePosetEqCompatible :: Double -> Double -> Bool
propDoublePosetEqCompatible = NumOrd.propPosetEqCompatible

propDoublePosetAntiSymmetric :: Double -> Double -> Bool
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
           _ -> 0/0 -- ie NaN 
    min a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Prelude.min a b  
           _ -> 0/0 -- ie NaN
    
instance NumOrd.RoundedLattice Double where
    maxUpEff _ = max
    maxDnEff _ = max
    minUpEff _ = min
    minDnEff _ = min
    minmaxDefaultEffort _ = []
    