{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Double
    Description :  type synonym and basic tests for Interval Double  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Type synonym and basic tests for Interval Double.
-}
module Numeric.AERN.RealArithmetic.Interval.Double.Basics 
(
   DI, testsDISemidecidableEq
)
where

import Numeric.AERN.RealArithmetic.Basis.Double

import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.Equality

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

type DI = Interval Double

propDISemidecidableEqReflexive :: DI -> Bool
propDISemidecidableEqReflexive = propSemidecidableEqReflexive

propDISemidecidableEqSymmetric :: DI -> DI -> Bool
propDISemidecidableEqSymmetric = propSemidecidableEqSymmetric

propDISemidecidableEqTransitive :: DI -> DI -> DI -> Bool
propDISemidecidableEqTransitive = propSemidecidableEqTransitive

testsDISemidecidableEq :: Test
testsDISemidecidableEq =
    testGroup "DI (==?)" 
        [
         testProperty "reflexive" propDISemidecidableEqReflexive
        ,
         testProperty "symmetric" propDISemidecidableEqSymmetric
        ,
         testProperty "transitive" propDISemidecidableEqTransitive
        ]

