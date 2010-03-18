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
   testsDISemidecidableComparison, testsDILattice
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

propDISemidecidableComparisonAntiSymmetric :: DI -> DI -> Bool
propDISemidecidableComparisonAntiSymmetric = NumOrd.propSemidecidableComparisonAntiSymmetric

propDISemidecidableComparisonTransitiveEQ :: DI -> DI -> DI -> Bool
propDISemidecidableComparisonTransitiveEQ = NumOrd.propSemidecidableComparisonTransitiveEQ

propDISemidecidableComparisonTransitiveLT :: DI -> DI -> DI -> Bool
propDISemidecidableComparisonTransitiveLT = NumOrd.propSemidecidableComparisonTransitiveLT

propDISemidecidableComparisonTransitiveLE :: DI -> DI -> DI -> Bool
propDISemidecidableComparisonTransitiveLE = NumOrd.propSemidecidableComparisonTransitiveLE

propDIExtremaInSemidecidableComparison :: DI -> Bool
propDIExtremaInSemidecidableComparison = NumOrd.propExtremaInSemidecidableComparison

testsDISemidecidableComparison :: Test
testsDISemidecidableComparison =
    testGroup "DI (>=?)" 
        [
         testProperty "anti symmetric" propDISemidecidableComparisonAntiSymmetric
        ,
         testProperty "transitive EQ" propDISemidecidableComparisonTransitiveEQ
        ,
         testProperty "transitive LE" propDISemidecidableComparisonTransitiveLE
        ,
         testProperty "transitive LT" propDISemidecidableComparisonTransitiveLT
        ,
         testProperty "extrema" propDIExtremaInSemidecidableComparison
        ]
        
propDILatticeComparisonCompatible :: NumOrd.UniformlyOrderedPair DI -> Bool
propDILatticeComparisonCompatible = NumOrd.propLatticeComparisonCompatible

propDILatticeJoinAboveBoth :: NumOrd.UniformlyOrderedPair DI -> Bool
propDILatticeJoinAboveBoth = NumOrd.propLatticeJoinAboveBoth

propDILatticeMeetBelowBoth :: NumOrd.UniformlyOrderedPair DI -> Bool
propDILatticeMeetBelowBoth = NumOrd.propLatticeMeetBelowBoth

propDILatticeJoinIdempotent :: DI -> Bool
propDILatticeJoinIdempotent = NumOrd.propLatticeJoinIdempotent

propDILatticeJoinCommutative :: NumOrd.UniformlyOrderedPair DI -> Bool
propDILatticeJoinCommutative = NumOrd.propLatticeJoinCommutative

propDILatticeJoinAssocative :: NumOrd.UniformlyOrderedTriple DI -> Bool
propDILatticeJoinAssocative = NumOrd.propLatticeJoinAssocative

propDILatticeMeetIdempotent :: DI -> Bool
propDILatticeMeetIdempotent = NumOrd.propLatticeMeetIdempotent

propDILatticeMeetCommutative :: NumOrd.UniformlyOrderedPair DI -> Bool
propDILatticeMeetCommutative = NumOrd.propLatticeMeetCommutative

propDILatticeMeetAssocative :: NumOrd.UniformlyOrderedTriple DI -> Bool
propDILatticeMeetAssocative = NumOrd.propLatticeMeetAssocative

propDILatticeDistributive :: NumOrd.UniformlyOrderedTriple DI -> Bool
propDILatticeDistributive = NumOrd.propLatticeDistributive

testsDILattice :: Test
testsDILattice =
    testGroup "DI (min,max)" 
        [
         testProperty "Comparison compatible" propDILatticeComparisonCompatible
        ,
         testProperty "join above" propDILatticeJoinAboveBoth
        ,
         testProperty "meet below" propDILatticeMeetBelowBoth
        ,
         testProperty "join idempotent" propDILatticeJoinIdempotent
        ,
         testProperty "join commutative" propDILatticeJoinCommutative
        ,
         testProperty "join associative" propDILatticeJoinAssocative
        ,
         testProperty "meet idempotent" propDILatticeMeetIdempotent
        ,
         testProperty "meet commutative" propDILatticeMeetCommutative
        ,
         testProperty "meet associative" propDILatticeMeetAssocative
        ,
         testProperty "distributive" propDILatticeDistributive
        ]
        
