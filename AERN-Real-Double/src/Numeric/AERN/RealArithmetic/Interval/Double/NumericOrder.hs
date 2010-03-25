{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Double.NumericOrder
    Description :  numeric order tests for Interval Double  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Numeric order tests for Interval Double.
-}
module Numeric.AERN.RealArithmetic.Interval.Double.NumericOrder 
(
   testsDINumericSemidecidableComparison, testsDINumericLattice
)
where

import Numeric.AERN.RealArithmetic.Interval.Double.Basics 

import Numeric.AERN.RealArithmetic.Basis.Double

import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.Interval

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

propDINumericSemidecidableComparisonAntiSymmetric :: DI -> DI -> Bool
propDINumericSemidecidableComparisonAntiSymmetric = NumOrd.propSemidecidableComparisonAntiSymmetric

propDINumericSemidecidableComparisonTransitiveEQ :: DI -> DI -> DI -> Bool
propDINumericSemidecidableComparisonTransitiveEQ = NumOrd.propSemidecidableComparisonTransitiveEQ

propDINumericSemidecidableComparisonTransitiveLT :: DI -> DI -> DI -> Bool
propDINumericSemidecidableComparisonTransitiveLT = NumOrd.propSemidecidableComparisonTransitiveLT

propDINumericSemidecidableComparisonTransitiveLE :: DI -> DI -> DI -> Bool
propDINumericSemidecidableComparisonTransitiveLE = NumOrd.propSemidecidableComparisonTransitiveLE

propDIExtremaInSemidecidableComparison :: DI -> Bool
propDIExtremaInSemidecidableComparison = NumOrd.propExtremaInSemidecidableComparison

testsDINumericSemidecidableComparison :: Test
testsDINumericSemidecidableComparison =
    testGroup "DI (>=?)" 
        [
         testProperty "anti symmetric" propDINumericSemidecidableComparisonAntiSymmetric
        ,
         testProperty "transitive EQ" propDINumericSemidecidableComparisonTransitiveEQ
        ,
         testProperty "transitive LE" propDINumericSemidecidableComparisonTransitiveLE
        ,
         testProperty "transitive LT" propDINumericSemidecidableComparisonTransitiveLT
        ,
         testProperty "extrema" propDIExtremaInSemidecidableComparison
        ]
        
propDINumericLatticeComparisonCompatible :: NumOrd.UniformlyOrderedPair DI -> Bool
propDINumericLatticeComparisonCompatible = NumOrd.propLatticeComparisonCompatible

propDINumericLatticeJoinAboveBoth :: NumOrd.UniformlyOrderedPair DI -> Bool
propDINumericLatticeJoinAboveBoth = NumOrd.propLatticeJoinAboveBoth

propDINumericLatticeMeetBelowBoth :: NumOrd.UniformlyOrderedPair DI -> Bool
propDINumericLatticeMeetBelowBoth = NumOrd.propLatticeMeetBelowBoth

propDINumericLatticeJoinIdempotent :: DI -> Bool
propDINumericLatticeJoinIdempotent = NumOrd.propLatticeJoinIdempotent

propDINumericLatticeJoinCommutative :: NumOrd.UniformlyOrderedPair DI -> Bool
propDINumericLatticeJoinCommutative = NumOrd.propLatticeJoinCommutative

propDINumericLatticeJoinAssocative :: NumOrd.UniformlyOrderedTriple DI -> Bool
propDINumericLatticeJoinAssocative = NumOrd.propLatticeJoinAssocative

propDINumericLatticeMeetIdempotent :: DI -> Bool
propDINumericLatticeMeetIdempotent = NumOrd.propLatticeMeetIdempotent

propDINumericLatticeMeetCommutative :: NumOrd.UniformlyOrderedPair DI -> Bool
propDINumericLatticeMeetCommutative = NumOrd.propLatticeMeetCommutative

propDINumericLatticeMeetAssocative :: NumOrd.UniformlyOrderedTriple DI -> Bool
propDINumericLatticeMeetAssocative = NumOrd.propLatticeMeetAssocative

propDINumericLatticeDistributive :: NumOrd.UniformlyOrderedTriple DI -> Bool
propDINumericLatticeDistributive = NumOrd.propLatticeDistributive

testsDINumericLattice :: Test
testsDINumericLattice =
    testGroup "DI (min,max)" 
        [
         testProperty "Comparison compatible" propDINumericLatticeComparisonCompatible
        ,
         testProperty "join above" propDINumericLatticeJoinAboveBoth
        ,
         testProperty "meet below" propDINumericLatticeMeetBelowBoth
        ,
         testProperty "join idempotent" propDINumericLatticeJoinIdempotent
        ,
         testProperty "join commutative" propDINumericLatticeJoinCommutative
        ,
         testProperty "join associative" propDINumericLatticeJoinAssocative
        ,
         testProperty "meet idempotent" propDINumericLatticeMeetIdempotent
        ,
         testProperty "meet commutative" propDINumericLatticeMeetCommutative
        ,
         testProperty "meet associative" propDINumericLatticeMeetAssocative
        ,
         testProperty "distributive" propDINumericLatticeDistributive
        ]
        
