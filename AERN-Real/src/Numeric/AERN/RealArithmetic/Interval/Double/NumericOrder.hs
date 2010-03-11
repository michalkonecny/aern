{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Double
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
   testsDISemidecidablePoset
)
where

import Numeric.AERN.RealArithmetic.Interval.Double.Basics 

import Numeric.AERN.RealArithmetic.Basis.Double

import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.Interval

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

propDISemidecidablePosetEqCompatible :: DI -> DI -> Bool
propDISemidecidablePosetEqCompatible = NumOrd.propSemidecidablePosetEqCompatible

propDISemidecidablePosetAntiSymmetric :: DI -> DI -> Bool
propDISemidecidablePosetAntiSymmetric = NumOrd.propSemidecidablePosetAntiSymmetric

propDISemidecidablePosetTransitive :: DI -> DI -> DI -> Bool
propDISemidecidablePosetTransitive = NumOrd.propSemidecidablePosetTransitive

propDIExtremaInSemidecidablePoset :: DI -> Bool
propDIExtremaInSemidecidablePoset = NumOrd.propExtremaInSemidecidablePoset

testsDISemidecidablePoset :: Test
testsDISemidecidablePoset =
    testGroup "DI (>=?)" 
        [
         testProperty "equality compatible" propDISemidecidablePosetEqCompatible
        ,
         testProperty "anti symmetric" propDISemidecidablePosetAntiSymmetric
        ,
         testProperty "transitive" propDISemidecidablePosetTransitive
        ,
         testProperty "extrema" propDIExtremaInSemidecidablePoset
        ]
