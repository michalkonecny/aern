module Main where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly
import Numeric.AERN.RmToRn.New

import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.Basics.Interval

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.Basics.ShowInternals

import Numeric.AERN.Misc.Debug

import System.IO

import qualified Data.Map as Map
import qualified Data.List as List

type MI = Interval MPFR
type Poly = IntPoly String MI

main =
    do
    putStrLn $ "x = " ++ showPoly id show x
    putStrLn $ "y = " ++ showPoly id show y
    putStrLn $ "x + y = " ++ (showPoly id show $ xPy)
    putStrLn $ "2(x + y) = " ++ (showPoly id show $ twoBxPy)
    putStrLn $ "2(x + y) = " ++ (showPoly id show $ twoBxPy)
    putStrLn $ "d (2(x + y))/dx = " ++ (showPoly id show $ diffPoly (100) "x" twoBxPy)
    putStrLn $ "d (2(x + y))/dy = " ++ (showPoly id show $ diffPoly (100) "y" twoBxPy)

x,y :: Poly
x = newProjection cfg dombox "x"
y = newProjection cfg dombox "y"

xPy = x <+> y
twoBxPy = (2::Int) |<*> (x <+> y)

cfg =
    IntPolyCfg
        {
            ipolycfg_vars = vars,
            ipolycfg_doms = doms,
            ipolycfg_sample_cf = 0 :: MI,
            ipolycfg_maxdeg = 2,
            ipolycfg_maxsize = 10
        }

dombox = Map.fromList $ zip vars doms

vars = ["x", "y"]

doms :: [(MI, MI)]
doms = [(0, 1), (0, 1)]