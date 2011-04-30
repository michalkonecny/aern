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
import Numeric.AERN.RealArithmetic.Interval.Mutable
import Numeric.AERN.RealArithmetic.Interval.ElementaryDirect
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
       NumOrd.testsPartialComparison ("Double", sampleD),
       NumOrd.testsRoundedLatticeDistributive ("Double", sampleD), -- (Just ("NaN", nanD)),
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
       NumOrd.testsPartialComparison ("DI", sampleDI),
       NumOrd.testsRefinementRoundedLatticeDistributiveMonotone  ("DI", sampleDI),
       NumOrd.testsRefinementRoundedLatticeInPlace ("DI", sampleDI),
       RefOrd.testsPartialComparison  ("DI", sampleDI), 
       RefOrd.testsRoundedBasis ("DI", sampleDI),
       RefOrd.testsRoundedLatticeDistributive ("DI", sampleDI),
       RefOrd.testsOuterInnerRoundedLatticeInPlace ("DI", sampleDI),
       testsDistance ("DI", sampleDI),
       testsImprecision ("DI", sampleDI),
       ArithInOut.testsConvertNumOrd ("Integer", sampleI, "DI", sampleDI),
       ArithInOut.testsConvertNumOrd ("Double", sampleD, "DI", sampleDI),
       ArithInOut.testsConvertNumOrd ("Rational", sampleR, "DI", sampleDI),
       ArithInOut.testsInOutAdd ("DI", sampleDI),
       ArithInOut.testsInOutSubtr ("DI", sampleDI),
       ArithInOut.testsInOutAbs ("DI", sampleDI),
       ArithInOut.testsInOutMult ("DI", sampleDI),
       ArithInOut.testsInOutIntPower ("DI", sampleDI),
       ArithInOut.testsInOutDiv ("DI", sampleDI),
       ArithInOut.testsInOutFieldOpsInPlace ("DI", sampleDI),
       ArithInOut.testsInOutMixedFieldOps ("DI", sampleDI) ("Integer", sampleI),
       ArithInOut.testsInOutMixedFieldOps ("DI", sampleDI) ("Rational", sampleR),
       ArithInOut.testsInOutMixedFieldOps ("DI", sampleDI) ("Double", sampleD),
       ArithInOut.testsInOutMixedFieldOpsInPlace ("DI", sampleDI) ("Integer", sampleI),
       ArithInOut.testsInOutMixedFieldOpsInPlace ("DI", sampleDI) ("Rational", sampleR),
       ArithInOut.testsInOutMixedFieldOpsInPlace ("DI", sampleDI) ("Double", sampleD),
       ArithInOut.testsInOutExp ("DI", sampleDI),
       ArithInOut.testsInOutExpInPlace ("DI", sampleDI),
       ArithInOut.testsInOutSqrt ("DI", sampleDI) unPositiveDI
    ]

sampleI = 1 :: Integer
sampleR = 1 :: Rational
