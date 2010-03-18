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
   testsDIRefinementSemidecidableComparison
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
