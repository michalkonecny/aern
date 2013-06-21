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

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Laws (testsEval, testsFnNumCompare)
--import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.Basis.Double ()
import Numeric.AERN.RealArithmetic.Basis.MPFR ()
import Numeric.AERN.Basics.Interval

-- import Numeric.AERN.RealArithmetic.Interval
-- import Numeric.AERN.RealArithmetic.Interval.Mutable
-- import Numeric.AERN.RealArithmetic.Interval.ElementaryDirect
--import Numeric.AERN.RealArithmetic.Interval.ElementaryFromBasis
--import Numeric.AERN.Basics.Interval

--import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Arbitrary
--import qualified Numeric.AERN.NumericOrder as NumOrd
--import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.Operators

--import Numeric.AERN.RealArithmetic.Measures
--import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
--import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Test.Framework (defaultMain, Test)

import qualified Data.Map as Map

--type CF = Interval MPFR
type CF = Interval Double
type Poly = IntPoly String CF

polyTypeName :: String
polyTypeName = "IntPoly-DI"

main :: IO ()
main =
    do
    defaultMain tests

tests :: [Test]
tests = testsPoly

testsPoly :: [Test]
testsPoly =
    [
        testsEval ("IntPoly-DI", samplePoly) areaPoly
        ,
        testsFnNumCompare ("IntPoly-DI", samplePoly) areaPoly
    ]

areaPoly :: Area Poly
areaPoly = areaWhole samplePoly

--sampleD = 1 :: Double
sampleI :: Integer
sampleI = 1
sampleR :: Rational
sampleR = 1

samplePoly :: Poly
samplePoly = newConstFn cfg dombox 0

cfg :: IntPolyCfg String CF
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

dombox :: DomainBox Poly
dombox = Map.fromList $ zip vars doms

doms :: [CF]
vars :: [Var Poly]

doms = [(0 </\> 1), 0 </\> 1]
vars = ["x", "y"]

--doms = [(0 </\> 1)]
--vars = ["x"]
        