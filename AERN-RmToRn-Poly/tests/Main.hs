{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Main
    Description :  run tests defined in AERN-Real and AERN-RmToRn
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Main where

import Numeric.AERN.RmToRn.MinimalFnBasis
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff as GCPoly
import Numeric.AERN.RealArithmetic.Basis.Double()

import Numeric.AERN.RealArithmetic.Interval.Double
import Numeric.AERN.RealArithmetic.Interval
import Numeric.AERN.RealArithmetic.Interval.Mutable
import Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps
-- import Numeric.AERN.RealArithmetic.Interval.ElementaryFromBasis
import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.Consistency
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import Test.Framework (defaultMain)

type P = FnEndpoint (GCPoly.Poly Double)
sampleP :: P
sampleP = FnEndpoint sampleFn
    where
    [sampleVar] = getNVariables sampleFn 1
    box = fromAscList [(sampleVar, fixedDomain sampleFn)]
    sampleFn = newProjection sizeLimits box sampleVar
    sizeLimits =
        GCPoly.PolySizeLimits sampleFn (0::Double) opsFP 10 3 3
    opsFP = GCPoly.opsFPArithUpDnDefaultEffort (0 :: Double)

main =
    do
    defaultMain tests

tests = testsGCPolyD ++ testsGCPolyDI

testsGCPolyD =
    [
       NumOrd.testsPartialComparison ("GCPolyD", sampleP),
--       NumOrd.testsRoundedLatticeDistributive ("GCPolyD", sampleP),
--       testsDistance ("GCPolyD", sampleP),
--       ArithUpDn.testsConvert ("GCPolyD", sampleP, "Integer", sampleI),
--       ArithUpDn.testsConvert ("GCPolyD", sampleP, "Rational", sampleR),
--       ArithUpDn.testsConvert ("GCPolyD", sampleP, "Double", sampleD),
       ArithUpDn.testsUpDnAdd ("GCPolyD", sampleP),
       ArithUpDn.testsUpDnSubtr ("GCPolyD", sampleP)
--       ,
--       ArithUpDn.testsUpDnAbs ("GCPolyD", sampleP),
--       ArithUpDn.testsUpDnMult ("GCPolyD", sampleP)
--       ,
--       ArithUpDn.testsUpDnIntPower ("GCPolyD", sampleP),
--       ArithUpDn.testsUpDnDiv ("GCPolyD", sampleP),
--       ArithUpDn.testsUpDnFieldOpsInPlace ("GCPolyD", sampleP),
--       ArithUpDn.testsUpDnMixedFieldOps ("GCPolyD", sampleP) ("Integer", sampleI),
--       ArithUpDn.testsUpDnMixedFieldOps ("GCPolyD", sampleP) ("Rational", sampleR),
--       ArithUpDn.testsUpDnMixedFieldOps ("GCPolyD", sampleP) ("Double", sampleD),
--       ArithUpDn.testsUpDnMixedFieldOpsInPlace ("GCPolyD", sampleP) ("Integer", sampleI),
--       ArithUpDn.testsUpDnMixedFieldOpsInPlace ("GCPolyD", sampleP) ("Rational", sampleR),
--       ArithUpDn.testsUpDnMixedFieldOpsInPlace ("GCPolyD", sampleP) ("Double", sampleD)
    ]

testsGCPolyDI =
    [
--       testsConsistency ("MI", samplePI),
--       NumOrd.testsPartialComparison ("MI", samplePI),
--       NumOrd.testsRefinementRoundedLatticeDistributiveMonotone  ("MI", samplePI),
--       NumOrd.testsRefinementRoundedLatticeInPlace ("MI", samplePI),
--       RefOrd.testsPartialComparison  ("MI", samplePI), 
--       RefOrd.testsRoundedBasis ("MI", samplePI),
--       RefOrd.testsRoundedLatticeDistributive ("MI", samplePI),
--       testsDistance ("MI", samplePI),
--       testsImprecision ("MI", samplePI),
--       ArithInOut.testsConvertNumOrd ("Integer", sampleI, "MI", samplePI),
--       ArithInOut.testsConvertNumOrd ("Double", sampleD, "MI", samplePI),
--       ArithInOut.testsConvertNumOrd ("Rational", sampleR, "MI", samplePI),
--       ArithInOut.testsInOutAdd ("MI", samplePI),
--       ArithInOut.testsInOutSubtr ("MI", samplePI),
--       ArithInOut.testsInOutAbs ("MI", samplePI),
--       ArithInOut.testsInOutMult ("MI", samplePI),
--       ArithInOut.testsInOutIntPower ("MI", samplePI),
--       ArithInOut.testsInOutDiv ("MI", samplePI),
--       ArithInOut.testsInOutFieldOpsInPlace ("MI", samplePI),
--       ArithInOut.testsInOutMixedFieldOps ("MI", samplePI) ("Integer", sampleI),
--       ArithInOut.testsInOutMixedFieldOps ("MI", samplePI) ("Rational", sampleR),
--       ArithInOut.testsInOutMixedFieldOps ("MI", samplePI) ("Double", sampleD),
--       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", samplePI) ("Integer", sampleI),
--       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", samplePI) ("Rational", sampleR),
--       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", samplePI) ("Double", sampleD)
--       ,
--       ArithInOut.testsInOutExp ("MI", samplePI),
--       ArithInOut.testsInOutSqrt ("MI", samplePI) unPositiveMI
    ]

sampleD = 1 :: Double
sampleI = 1 :: Integer
sampleR = 1 :: Rational
