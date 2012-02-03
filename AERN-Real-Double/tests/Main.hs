{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Main
    Description :  run all tests defined in the AERN-Real package  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Main where

import Numeric.AERN.RealArithmetic.Basis.Double
import Numeric.AERN.RealArithmetic.Interval.Double
import Numeric.AERN.RealArithmetic.Interval
import Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps
import Numeric.AERN.RealArithmetic.Interval.Mutable
import Numeric.AERN.RealArithmetic.Interval.Mutable.ElementaryFromFieldOps
import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.Consistency
import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

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
       NumOrd.testsPartialComparison ("Double", sampleD) areaN,
       NumOrd.testsRoundedLatticeDistributive ("Double", sampleD) areaN,
       NumOrd.testsRoundedLatticeInPlace ("Double", sampleD),
       testsDistance ("Double", sampleD),
       ArithUpDn.testsConvert ("Double", sampleD, "Integer", sampleI),
       ArithUpDn.testsConvert ("Integer", sampleI, "Double", sampleD),
       ArithUpDn.testsConvert ("Double", sampleD, "Rational", sampleR),
       ArithUpDn.testsConvert ("Rational", sampleR, "Double", sampleD),
       ArithUpDn.testsConvert ("Double", sampleD, "Double", sampleD),
       ArithUpDn.testsUpDnAdd ("Double", sampleD),
       ArithUpDn.testsUpDnSubtr ("Double", sampleD),
       ArithUpDn.testsUpDnAbs ("Double", sampleD),
       ArithUpDn.testsUpDnMult ("Double", sampleD),
       ArithUpDn.testsUpDnIntPower ("Double", sampleD),
       ArithUpDn.testsUpDnDiv ("Double", sampleD),
       ArithUpDn.testsUpDnFieldOpsInPlace ("Double", sampleD),
       ArithUpDn.testsUpDnMixedFieldOps ("Double", sampleD) ("Integer", sampleI),
       ArithUpDn.testsUpDnMixedFieldOps ("Double", sampleD) ("Rational", sampleR),
       ArithUpDn.testsUpDnMixedFieldOps ("Double", sampleD) ("Double", sampleD),
       ArithUpDn.testsUpDnMixedFieldOpsInPlace ("Double", sampleD) ("Integer", sampleI),
       ArithUpDn.testsUpDnMixedFieldOpsInPlace ("Double", sampleD) ("Rational", sampleR),
       ArithUpDn.testsUpDnMixedFieldOpsInPlace ("Double", sampleD) ("Double", sampleD)
    ]

testsDI =
    [
       testsConsistency ("DI", sampleDI),
       NumOrd.testsPartialComparison ("DI", sampleDI) areaNInterval,
       NumOrd.testsRefinementRoundedLatticeDistributiveMonotone  ("DI", sampleDI) areaNInterval areaR,
       NumOrd.testsRefinementRoundedLatticeInPlace ("DI", sampleDI),
       RefOrd.testsPartialComparison  ("DI", sampleDI) areaR, 
       RefOrd.testsRoundedBasis ("DI", sampleDI),
       RefOrd.testsOuterInnerRoundedBasisInPlace ("DI", sampleDI),
       RefOrd.testsRoundedLatticeDistributive ("DI", sampleDI) areaR,
       RefOrd.testsOuterInnerRoundedLatticeInPlace ("DI", sampleDI),
       testsDistance ("DI", sampleDI),
       testsImprecision ("DI", sampleDI),
       ArithInOut.testsConvertNumOrd ("Integer", sampleI, "DI", sampleDI),
       ArithInOut.testsConvertNumOrd ("Double", sampleD, "DI", sampleDI),
       ArithInOut.testsConvertNumOrd ("Rational", sampleR, "DI", sampleDI),
       ArithInOut.testsInOutAdd ("DI", sampleDI) areaR,
       ArithInOut.testsInOutSubtr ("DI", sampleDI) areaR,
       ArithInOut.testsInOutAbs ("DI", sampleDI) areaR,
       ArithInOut.testsInOutMult ("DI", sampleDI) areaR,
       ArithInOut.testsInOutIntPower ("DI", sampleDI) areaR,
       ArithInOut.testsInOutDiv ("DI", sampleDI) areaR,
       ArithInOut.testsInOutFieldOpsInPlace ("DI", sampleDI),
       ArithInOut.testsInOutMixedFieldOps ("DI", sampleDI) ("Integer", sampleI) areaR,
       ArithInOut.testsInOutMixedFieldOps ("DI", sampleDI) ("Rational", sampleR) areaR,
       ArithInOut.testsInOutMixedFieldOps ("DI", sampleDI) ("Double", sampleD) areaR,
       ArithInOut.testsInOutMixedFieldOpsInPlace ("DI", sampleDI) ("Integer", sampleI),
       ArithInOut.testsInOutMixedFieldOpsInPlace ("DI", sampleDI) ("Rational", sampleR),
       ArithInOut.testsInOutMixedFieldOpsInPlace ("DI", sampleDI) ("Double", sampleD),
       ArithInOut.testsInOutExp ("DI", sampleDI),
       ArithInOut.testsInOutExpInPlace ("DI", sampleDI),
       ArithInOut.testsInOutSqrt ("DI", sampleDI) unPositiveDI,
       ArithInOut.testsInOutSqrtInPlace ("DI", sampleDI) unPositiveDI
    ]

areaN = NumOrd.areaWhole sampleD
areaNInterval = NumOrd.areaWhole sampleDI
areaR = RefOrd.areaWhole sampleDI

sampleI = 1 :: Integer
sampleR = 1 :: Rational

