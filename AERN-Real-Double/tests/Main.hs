{-# LANGUAGE TypeFamilies #-}
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
import Numeric.AERN.RealArithmetic.Interval
import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.Consistency
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import Test.Framework (defaultMain)

main =
    do
    defaultMain tests

tests = testsDouble ++ testsDI

testsDouble =
    [
--       NumOrd.testsArbitraryTuple ("Double", sampleD, NumOrd.compare),
       NumOrd.testsComparison ("Double", sampleD) Nothing, -- (Just ("NaN", nanD)),
       NumOrd.testsPartialComparison ("Double", sampleD),
       NumOrd.testsLatticeDistributive ("Double", sampleD) Nothing, --  (Just ("NaN", nanD)),
       NumOrd.testsRoundedLatticeDistributive ("Double", sampleD) Nothing, -- (Just ("NaN", nanD)),
       testsDistance ("Double", sampleD),
       ArithUpDn.testsFromToInteger ("Double", sampleD),
       ArithUpDn.testsFromToDouble ("Double", sampleD),
       ArithUpDn.testsUpDnAdd ("Double", sampleD),
       ArithUpDn.testsUpDnSubtr ("Double", sampleD),
       ArithUpDn.testsUpDnAbs ("Double", sampleD),
       ArithUpDn.testsUpDnMult ("Double", sampleD),
       ArithUpDn.testsUpDnDiv ("Double", sampleD)
    ]

testsDI =
    [
       testsConsistency ("DI", sampleDI),
       NumOrd.testsPartialComparison ("DI", sampleDI),
       NumOrd.testsLatticeDistributiveMonotone ("DI", sampleDI) Nothing,
       NumOrd.testsRefinementRoundedLatticeDistributiveMonotone  ("DI", sampleDI) Nothing,
       RefOrd.testsPartialComparison  ("DI", sampleDI), 
       RefOrd.testsBasis ("DI", sampleDI),
       RefOrd.testsRoundedBasis ("DI", sampleDI),
       RefOrd.testsLatticeDistributive ("DI", sampleDI),
       RefOrd.testsRoundedLatticeDistributive ("DI", sampleDI),
       testsDistance ("DI", sampleDI),
       testsImprecision ("DI", sampleDI),
       ArithInOut.testsFromToInteger ("DI", sampleDI),
       ArithInOut.testsInOutAdd ("DI", sampleDI),
       ArithInOut.testsInOutSubtr ("DI", sampleDI),
       ArithInOut.testsInOutAbs ("DI", sampleDI),
       ArithInOut.testsInOutMult ("DI", sampleDI),
       ArithInOut.testsInOutDiv ("DI", sampleDI)
    ]
