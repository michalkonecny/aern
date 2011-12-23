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

main =
    do
    defaultMain tests

tests = testsIntPoly

testsIntPoly =
    [
--       testsConsistency ("MI", sampleMI),
       NumOrd.testsPartialComparison ("IntPoly-DI", samplePoly) (NumOrd.areaWhole samplePoly)
--       ,
--       NumOrd.testsRefinementRoundedLatticeDistributiveMonotone  ("MI", sampleMI),
--       NumOrd.testsRefinementRoundedLatticeInPlace ("MI", sampleMI),
--       RefOrd.testsPartialComparison  ("MI", sampleMI), 
--       RefOrd.testsRoundedBasis ("MI", sampleMI),
--       RefOrd.testsRoundedLatticeDistributive ("MI", sampleMI),
--       testsDistance ("MI", sampleMI),
--       testsImprecision ("MI", sampleMI),
--       ArithInOut.testsConvertNumOrd ("Integer", sampleI, "MI", sampleMI),
--       ArithInOut.testsConvertNumOrd ("Double", sampleD, "MI", sampleMI),
--       ArithInOut.testsConvertNumOrd ("Rational", sampleR, "MI", sampleMI),
--       ArithInOut.testsInOutAdd ("MI", sampleMI),
--       ArithInOut.testsInOutSubtr ("MI", sampleMI),
--       ArithInOut.testsInOutAbs ("MI", sampleMI),
--       ArithInOut.testsInOutMult ("MI", sampleMI),
--       ArithInOut.testsInOutIntPower ("MI", sampleMI),
--       ArithInOut.testsInOutDiv ("MI", sampleMI),
--       ArithInOut.testsInOutFieldOpsInPlace ("MI", sampleMI),
--       ArithInOut.testsInOutMixedFieldOps ("MI", sampleMI) ("Integer", sampleI),
--       ArithInOut.testsInOutMixedFieldOps ("MI", sampleMI) ("Rational", sampleR),
--       ArithInOut.testsInOutMixedFieldOps ("MI", sampleMI) ("Double", sampleD),
--       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", sampleMI) ("Integer", sampleI),
--       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", sampleMI) ("Rational", sampleR),
--       ArithInOut.testsInOutMixedFieldOpsInPlace ("MI", sampleMI) ("Double", sampleD)
--       ,
--       ArithInOut.testsInOutExp ("MI", sampleMI),
--       ArithInOut.testsInOutSqrt ("MI", sampleMI) unPositiveMI
    ]

sampleD = 1 :: Double
sampleI = 1 :: Integer
sampleR = 1 :: Rational

samplePoly :: Poly
samplePoly = newConstFn cfg dombox 0

cfg =
    IntPolyCfg
        {
            ipolycfg_vars = vars,
            ipolycfg_doms = doms,
            ipolycfg_sample_cf = 0 :: CF,
            ipolycfg_maxdeg = 4,
            ipolycfg_maxsize = 30
        }

dombox = Map.fromList $ zip vars doms

doms :: [CF]
doms = [(0 </\> 1), 0 </\> 1]

vars = ["x", "y"]
        