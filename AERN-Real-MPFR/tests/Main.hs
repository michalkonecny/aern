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

import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.RealArithmetic.Interval.MPFR
import Numeric.AERN.RealArithmetic.Interval
import Numeric.AERN.RealArithmetic.Interval.Mutable
-- import Numeric.AERN.RealArithmetic.Interval.ElementaryDirect
import Numeric.AERN.RealArithmetic.Interval.ElementaryFromBasis
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

tests = testsMPFR ++ testsMI

testsMPFR =
    [
--       NumOrd.testsArbitraryTuple ("MPFR", sampleM, NumOrd.compare),
       NumOrd.testsPartialComparison ("MPFR", sampleM),
       NumOrd.testsRoundedLatticeDistributive ("MPFR", sampleM),
       testsDistance ("MPFR", sampleM),
       ArithUpDn.testsConvert ("MPFR", sampleM, "Integer", sampleI),
       ArithUpDn.testsConvert ("Integer", sampleI, "MPFR", sampleM),
       ArithUpDn.testsConvert ("MPFR", sampleM, "Rational", sampleR),
       ArithUpDn.testsConvert ("Rational", sampleR, "MPFR", sampleM),
       ArithUpDn.testsConvert ("Double", sampleD, "MPFR", sampleM),
       ArithUpDn.testsConvert ("MPFR", sampleM, "Double", sampleD),
       ArithUpDn.testsUpDnAdd ("MPFR", sampleM),
       ArithUpDn.testsUpDnSubtr ("MPFR", sampleM),
       ArithUpDn.testsUpDnAbs ("MPFR", sampleM),
       ArithUpDn.testsUpDnMult ("MPFR", sampleM),
       ArithUpDn.testsUpDnIntPower ("MPFR", sampleM),
       ArithUpDn.testsUpDnDiv ("MPFR", sampleM),
       ArithUpDn.testsUpDnFieldOpsInPlace ("MPFR", sampleM),
       ArithUpDn.testsUpDnMixedFieldOps ("MPFR", sampleM) ("Integer", sampleI),
       ArithUpDn.testsUpDnMixedFieldOps ("MPFR", sampleM) ("Rational", sampleR),
       ArithUpDn.testsUpDnMixedFieldOps ("MPFR", sampleM) ("Double", sampleD),
       ArithUpDn.testsUpDnMixedFieldOpsInPlace ("MPFR", sampleM) ("Integer", sampleI),
       ArithUpDn.testsUpDnMixedFieldOpsInPlace ("MPFR", sampleM) ("Rational", sampleR),
       ArithUpDn.testsUpDnMixedFieldOpsInPlace ("MPFR", sampleM) ("Double", sampleD),
       ArithUpDn.testsUpDnExp ("MPFR", sampleM)
    ]

testsMI =
    [
       testsConsistency ("MI", sampleMI),
       NumOrd.testsPartialComparison ("MI", sampleMI),
       NumOrd.testsRefinementRoundedLatticeDistributiveMonotone  ("MI", sampleMI),
       NumOrd.testsRefinementRoundedLatticeInPlace ("MI", sampleMI),
       RefOrd.testsPartialComparison  ("MI", sampleMI), 
       RefOrd.testsRoundedBasis ("MI", sampleMI),
       RefOrd.testsRoundedLatticeDistributive ("MI", sampleMI),
       testsDistance ("MI", sampleMI),
       testsImprecision ("MI", sampleMI),
       ArithInOut.testsConvertNumOrd ("Integer", sampleI, "MI", sampleMI),
       ArithInOut.testsConvertNumOrd ("Double", sampleD, "MI", sampleMI),
       ArithInOut.testsConvertNumOrd ("Rational", sampleR, "MI", sampleMI),
       ArithInOut.testsInOutAdd ("MI", sampleMI),
       ArithInOut.testsInOutSubtr ("MI", sampleMI),
       ArithInOut.testsInOutAbs ("MI", sampleMI),
       ArithInOut.testsInOutMult ("MI", sampleMI),
       ArithInOut.testsInOutIntPower ("MI", sampleMI),
       ArithInOut.testsInOutDiv ("MI", sampleMI),
       ArithInOut.testsInOutFieldOpsInPlace ("MI", sampleMI),
       ArithInOut.testsInOutMixedFieldOps ("MI", sampleMI) ("Integer", sampleI),
       ArithInOut.testsInOutMixedFieldOps ("MI", sampleMI) ("Rational", sampleR),
       ArithInOut.testsInOutMixedFieldOps ("MI", sampleMI) ("Double", sampleD),
       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", sampleMI) ("Integer", sampleI),
       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", sampleMI) ("Rational", sampleR),
       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", sampleMI) ("Double", sampleD)
       ,
       ArithInOut.testsInOutExp ("MI", sampleMI),
       ArithInOut.testsInOutSqrt ("MI", sampleMI) unPositiveMI
    ]

sampleD = 1 :: Double
sampleI = 1 :: Integer
sampleR = 1 :: Rational
