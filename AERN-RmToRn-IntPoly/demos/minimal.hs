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
    putStrLn $ "0 = " ++ showP c0
    putStrLn $ "1 = " ++ showP c1
    putStrLn $ "x = " ++ showP x
    putStrLn $ "y = " ++ showP y
    putStrLn $ "x + y = " ++ (showP $ xPy)
    putStrLn $ "x + y + 1 + 1 = " ++ (showP $ xPyP1P1)
    putStrLn $ "(x + y)*(x - y) = " ++ (showP $ xPyBTxMyB)
    putStrLn $ "2(x + y + 2) = " ++ (showP $ twoBxPyP2)
    putStrLn $ "2(x + y + 2) = " ++ (showP $ twoBxPyP2)
    putStrLn $ "d (2(x + y + 2))/dx = " ++ (showP $ diffPoly (100) "x" twoBxPyP2)
    putStrLn $ "d (2(x + y + 2))/dy = " ++ (showP $ diffPoly (100) "y" twoBxPyP2)
    putStrLn $ "d (x^2 + 2xy + 4x + 1)/dx = " ++ (showP $ diffPoly (100) "x" integTwoBxPyP2)
    putStrLn $ "d (x^2 + 2xy + 4x + 1)/dy = " ++ (showP $ diffPoly (100) "y" integTwoBxPyP2)
    putStrLn $ "1 + int (2(x + y + 2)) dx = " ++ (showP integTwoBxPyP2)
    putStrLn $ "(x + y + 2)drct[x=-1,y=-1] = " ++ (show $ evalPolyDirect 100 0 [(-1,-1),(-1,-1)] xPyP1P1)
    putStrLn $ "(x + y + 2)drct[x=[-1,0],y=[-1,0]] = " ++ (show $ evalPolyDirect 100 0 [(-1,0),(-1,0)] xPyP1P1)
    putStrLn $ "(x^2 + 2xy + 4x + 1)drct[x=-1,y=-1] = " ++ (show $ evalPolyDirect 100 0 [(-1,-1),(-1,-1)] integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)drct[x=[-1,0],y=[-1,0]] = " ++ (show $ evalPolyDirect 100 0 [(-1,0),(-1,0)] integTwoBxPyP2)
    putStrLn $ "(x + y + 2)mono[x=-1,y=-1] = " ++ (show $ evalPolyMono 100 0 [(-1,-1),(-1,-1)] xPyP1P1)
    putStrLn $ "(x + y + 2)mono[x=[-1,0],y=[-1,0]] = " ++ (show $ evalPolyMono 100 0 [(-1,0),(-1,0)] xPyP1P1)
    putStrLn $ "(x^2 + 2xy + 4x + 1)mono[x=-1,y=-1] = " ++ (show $ evalPolyMono 100 0 [(-1,-1),(-1,-1)] integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)mono[x=[-1,0],y=[-1,0]] = " ++ (show $ evalPolyMono 100 0 [(-1,0),(-1,0)] integTwoBxPyP2)
--    putStrLn $ "exp(x + y + 2) = " ++ (showP $ expBxPyP2)
    where
    showP p = showPoly id show p ++ " [" ++ show p ++ "]" 

c1,c0,x,y :: Poly
x = newProjection cfg dombox "x"
y = newProjection cfg dombox "y"
c0 = newConstFn cfg dombox 0
c1 = newConstFn cfg dombox 1

xPy = x <+> y
xMy = x <-> y
xPyP1P1 = xPy <+> c1 <+> c1
xPyBTxMyB = xPy <*> xMy
twoBxPyP2 = (2::Int) |<*> xPyP1P1
integTwoBxPyP2 = integratePolyMainVar (100) 0 c1 twoBxPyP2
--expBxPyP2 = exp xPyP1P1

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