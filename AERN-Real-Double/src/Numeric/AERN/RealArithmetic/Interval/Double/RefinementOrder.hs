{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Double.RefinementOrder
    Description :  numeric order tests for Interval Double  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Numeric order tests for Interval Double.
-}
module Numeric.AERN.RealArithmetic.Interval.Double.RefinementOrder
(
   testsDIRefinementSemidecidableComparison,
   testsDIRefinementBasis, testsDIRefinementRoundedBasis
)
where

import Numeric.AERN.RealArithmetic.Interval.Double.Basics 

import Numeric.AERN.RealArithmetic.Basis.Double

import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.Interval

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

propDIRefinementSemidecidableComparisonAntiSymmetric :: DI -> DI -> Bool
propDIRefinementSemidecidableComparisonAntiSymmetric = RefOrd.propSemidecidableComparisonAntiSymmetric

propDIRefinementSemidecidableComparisonTransitiveEQ :: DI -> DI -> DI -> Bool
propDIRefinementSemidecidableComparisonTransitiveEQ = RefOrd.propSemidecidableComparisonTransitiveEQ

propDIRefinementSemidecidableComparisonTransitiveLT :: DI -> DI -> DI -> Bool
propDIRefinementSemidecidableComparisonTransitiveLT = RefOrd.propSemidecidableComparisonTransitiveLT

propDIRefinementSemidecidableComparisonTransitiveLE :: DI -> DI -> DI -> Bool
propDIRefinementSemidecidableComparisonTransitiveLE = RefOrd.propSemidecidableComparisonTransitiveLE

propDIRefinementExtremaInSemidecidableComparison :: DI -> Bool
propDIRefinementExtremaInSemidecidableComparison = RefOrd.propExtremaInSemidecidableComparison

testsDIRefinementSemidecidableComparison :: Test
testsDIRefinementSemidecidableComparison =
    testGroup "DI (⊑?)" 
        [
         testProperty "anti symmetric" propDIRefinementSemidecidableComparisonAntiSymmetric
        ,
         testProperty "transitive" propDIRefinementSemidecidableComparisonTransitiveEQ
        ,
         testProperty "transitive" propDIRefinementSemidecidableComparisonTransitiveLT
        ,
         testProperty "transitive" propDIRefinementSemidecidableComparisonTransitiveLE
        ,
         testProperty "extrema" propDIRefinementExtremaInSemidecidableComparison
        ]

propDIRefinementBasisComparisonCompatible :: RefOrd.UniformlyOrderedPair DI -> Bool
propDIRefinementBasisComparisonCompatible (RefOrd.UniformlyOrderedPair (i1, i2)) = 
    RefOrd.propBasisComparisonCompatible i1 i2

propDIRefinementBasisJoinAboveBoth :: RefOrd.UniformlyOrderedPair DI -> Bool
propDIRefinementBasisJoinAboveBoth = RefOrd.propBasisJoinAboveBoth

propDIRefinementBasisJoinIdempotent :: DI -> Bool
propDIRefinementBasisJoinIdempotent = RefOrd.propBasisJoinIdempotent

propDIRefinementBasisJoinCommutative :: RefOrd.UniformlyOrderedPair DI -> Bool
propDIRefinementBasisJoinCommutative = RefOrd.propBasisJoinCommutative

propDIRefinementBasisJoinAssociative :: RefOrd.UniformlyOrderedTriple DI -> Bool
propDIRefinementBasisJoinAssociative = RefOrd.propBasisJoinAssocative

testsDIRefinementBasis :: Test
testsDIRefinementBasis =
    testGroup "DI (⊔?)"
        [
         testProperty "join comparison compatible"  propDIRefinementBasisComparisonCompatible,
         testProperty "join above both"  propDIRefinementBasisJoinAboveBoth,
         testProperty "join idempotent" propDIRefinementBasisJoinIdempotent,
         testProperty "join commutative" propDIRefinementBasisJoinCommutative,
         testProperty "join associative" propDIRefinementBasisJoinAssociative
        ]

propDIRefinementRoundedBasisComparisonCompatible :: RefOrd.UniformlyOrderedPair DI -> Bool
propDIRefinementRoundedBasisComparisonCompatible (RefOrd.UniformlyOrderedPair (i1, i2)) = 
    RefOrd.propOuterRoundedBasisComparisonCompatible i1 i2

propDIRefinementRoundedBasisJoinAboveBoth :: RefOrd.UniformlyOrderedPair DI -> Bool
propDIRefinementRoundedBasisJoinAboveBoth (RefOrd.UniformlyOrderedPair (i1, i2)) =
    RefOrd.propInnerRoundedBasisJoinAboveBoth i1 i2

propDIRefinementRoundedBasisJoinIdempotent :: DI -> Bool
propDIRefinementRoundedBasisJoinIdempotent = RefOrd.propRoundedBasisJoinIdempotent

propDIRefinementRoundedBasisJoinCommutative :: RefOrd.UniformlyOrderedPair DI -> Bool
propDIRefinementRoundedBasisJoinCommutative = RefOrd.propRoundedBasisJoinCommutative

propDIRefinementRoundedBasisJoinAssociative :: RefOrd.UniformlyOrderedTriple DI -> Bool
propDIRefinementRoundedBasisJoinAssociative = RefOrd.propRoundedBasisJoinAssocative

testsDIRefinementRoundedBasis :: Test
testsDIRefinementRoundedBasis =
    testGroup "DI (<⊔>?, >⊔<?)"
        [
         testProperty "rounded join comparison compatible"  propDIRefinementRoundedBasisComparisonCompatible,
         testProperty "rounded join above both"  propDIRefinementRoundedBasisJoinAboveBoth,
         testProperty "rounded join idempotent" propDIRefinementRoundedBasisJoinIdempotent,
         testProperty "rounded join commutative" propDIRefinementRoundedBasisJoinCommutative,
         testProperty "rounded join associative" propDIRefinementRoundedBasisJoinAssociative
        ]
        