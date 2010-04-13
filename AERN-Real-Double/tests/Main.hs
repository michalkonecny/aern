{-|
    Module      :  Main
    Description :  run all tests defined in the AERN-Real package  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Main where

import Numeric.AERN.RealArithmetic.Basis.Double
import Numeric.AERN.RealArithmetic.Interval.Double

import Numeric.AERN.Basics.Granularity
import Numeric.AERN.Basics.Interval
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Test.Framework (defaultMain)

main =
    do
    initGranularityRounding (0 :: Double) 
    defaultMain tests

tests = testsDouble ++ testsDI

testsDouble =
    [
--       NumOrd.testsArbitraryTuple ("Double", sampleD, NumOrd.compare),
       NumOrd.testsComparison ("Double", sampleD) (Just ("NaN", nanD)),
       NumOrd.testsSemidecidableComparison ("Double", sampleD),
       NumOrd.testsLatticeDistributive ("Double", sampleD) (Just ("NaN", nanD)),
       NumOrd.testsRoundedLatticeDistributive ("Double", sampleD) (Just ("NaN", nanD))
    ]

testsDI =
    [
       testsIntervalConsistencyFlip ("DI", sampleDI),
       NumOrd.testsSemidecidableComparison ("DI", sampleDI),
       NumOrd.testsLatticeDistributive ("DI", sampleDI) Nothing,
       NumOrd.testsRefinementRoundedLatticeDistributive  ("DI", sampleDI) Nothing,
       RefOrd.testsSemidecidableComparison  ("DI", sampleDI), 
       RefOrd.testsBasis ("DI", sampleDI),
       RefOrd.testsRoundedBasis ("DI", sampleDI),
       RefOrd.testsLatticeDistributive ("DI", sampleDI),
       RefOrd.testsRoundedLatticeDistributive ("DI", sampleDI)
    ]