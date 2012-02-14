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

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly
import Numeric.AERN.RmToRn.New

import Numeric.AERN.RealArithmetic.Basis.Double
import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.Basics.Interval

import Numeric.AERN.RealArithmetic.Interval
import Numeric.AERN.RealArithmetic.Interval.Mutable
-- import Numeric.AERN.RealArithmetic.Interval.ElementaryDirect
import Numeric.AERN.RealArithmetic.Interval.ElementaryFromBasis
import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.Consistency
import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Test.Framework (defaultMain)

import qualified Data.Map as Map

--type CF = Interval MPFR
type CF = Interval Double
type Poly = IntPoly String CF
polyTypeName = "IntPoly-DI"

main =
    do
    defaultMain tests

tests = testsPoly

testsPoly =
    [
--       testsConsistency ("IntPoly-DI", samplePoly),
--       NumOrd.testsPartialComparison ("IntPoly-DI", samplePoly) areaN
--       ,
--       NumOrd.testsRoundedLatticeDistributive ("IntPoly-DI", samplePoly) areaN
--       ,
--       NumOrd.testsRefinementRoundedLatticeDistributiveMonotone ("IntPoly-DI", samplePoly) areaN areaR
--       ,
--       RefOrd.testsPartialComparison ("IntPoly-DI", samplePoly) areaR
--       ,
----       RefOrd.testsRoundedBasis ("MI", sampleMI),
--       RefOrd.testsRoundedLatticeDistributive ("IntPoly-DI", samplePoly) areaR
--       ,
----       testsDistance ("MI", sampleMI),
----       testsImprecision ("MI", sampleMI),
----       ArithInOut.testsConvertNumOrd ("Integer", sampleI, "MI", sampleMI),
----       ArithInOut.testsConvertNumOrd ("Double", sampleD, "MI", sampleMI),
----       ArithInOut.testsConvertNumOrd ("Rational", sampleR, "MI", sampleMI),
       ArithInOut.testsInOutAdd ("IntPoly-DI", samplePoly) areaR,
       ArithInOut.testsInOutSubtr ("IntPoly-DI", samplePoly) areaR,
----       ArithInOut.testsInOutAbs ("MI", sampleMI),
       ArithInOut.testsInOutMult ("IntPoly-DI", samplePoly) areaR,
       ArithInOut.testsInOutIntPower ("IntPoly-DI", samplePoly) areaR
--       ,
----       ArithInOut.testsInOutDiv ("MI", sampleMI),
----       ArithInOut.testsInOutFieldOpsInPlace ("MI", sampleMI),
--       ArithInOut.testsInOutMixedFieldOps ("IntPoly-DI", samplePoly) ("Integer", sampleI) areaR,
--       ArithInOut.testsInOutMixedFieldOps ("IntPoly-DI", samplePoly) ("Rational", sampleR) areaR,
--       ArithInOut.testsInOutMixedFieldOps ("IntPoly-DI", samplePoly) ("Double", sampleD) areaR
--      ,
----       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", sampleMI) ("Integer", sampleI),
----       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", sampleMI) ("Rational", sampleR),
----       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", sampleMI) ("Double", sampleD)
--       ,
----       ArithInOut.testsInOutExp ("MI", sampleMI),
----       ArithInOut.testsInOutSqrt ("MI", sampleMI) unPositiveMI
    ]

areaN = NumOrd.areaWhole samplePoly
areaR = RefOrd.areaWhole samplePoly

--sampleD = 1 :: Double
sampleI = 1 :: Integer
sampleR = 1 :: Rational

samplePoly :: Poly
samplePoly = newConstFn cfg dombox 0

cfg =
    IntPolyCfg
        {
            ipolycfg_vars = vars,
            ipolycfg_domsLZ = doms,
            ipolycfg_domsLE = replicate (length vars) 0,
            ipolycfg_sample_cf = 0 :: CF,
            ipolycfg_maxdeg = 4,
            ipolycfg_maxsize = 30
        }

dombox = Map.fromList $ zip vars doms

doms :: [CF]
--doms = [(0 </\> 1), 0 </\> 1]
--vars = ["x", "y"]
doms = [(0 </\> 1)]
vars = ["x"]
        