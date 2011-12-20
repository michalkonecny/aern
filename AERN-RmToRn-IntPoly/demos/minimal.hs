module Main where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.Basics.Interval

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

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
    putStrLn $ "getEndpoints([0,1]x^2+[-1,0]) = " ++ (showPPair $ RefOrd.getEndpointsOutWithDefaultEffort $ c01 <*> (x <*> x <-> c1))
    putStrLn $ "fromEndpoints(x^2-1, 0) = " ++ (showP $ RefOrd.fromEndpointsOutWithDefaultEffort ((x <*> x <-> c1), c0))
    putStrLn $ "(x^2-1 `comp` 1) = " ++ (show $ numCompare (x <*> x <-> c1) c1)
    putStrLn $ "(x^2-1 `comp` 0) = " ++ (show $ numCompare (x <*> x <-> c1) c0)
    putStrLn $ "(x^2-1 `comp` -0.5) = " ++ (show $ numCompare (x <*> x <-> c1) (c0 <-> (c1 </>| (2::Int))))
    putStrLn $ "(x^2-1 `comp` -1) = " ++ (show $ numCompare (x <*> x <-> c1) (c0 <-> c1))
    putStrLn $ "(x^2-1 `comp` -2) = " ++ (show $ numCompare (x <*> x <-> c1) ((c0 <-> c1) <-> c1))
    putStrLn $ "d (2(x + y + 2))/dx = " ++ (showP $ diffPoly eff "x" twoBxPyP2)
    putStrLn $ "d (2(x + y + 2))/dy = " ++ (showP $ diffPoly eff "y" twoBxPyP2) 
    putStrLn $ "d (x^2 + 2xy + 4x + 1)/dx = " ++ (showP $ diffPoly eff "x" integTwoBxPyP2)
    putStrLn $ "d (x^2 + 2xy + 4x + 1)/dy = " ++ (showP $ diffPoly eff "y" integTwoBxPyP2)
    putStrLn $ "1 + int (2(x + y + 2)) dx = " ++ (showP integTwoBxPyP2)
    putStrLn $ "(x + y + 2)drct[x=-1,y=-1] = " ++ (show $ evalPolyAtPoint eff [-1, -1] xPyP1P1)
    putStrLn $ "(x + y + 2)drct[x=[-1,0],y=[-1,0]] = " ++ (show $ evalPolyAtPoint eff [mOneToZ,mOneToZ] xPyP1P1)
    putStrLn $ "(x^2 + 2xy + 4x + 1)drct[x=-1,y=-1] = " ++ (show $ evalPolyAtPoint eff [-1,-1] integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)drct[x=[-1,0],y=[-1,0]] = " ++ (show $ evalPolyAtPoint eff [mOneToZ,mOneToZ] integTwoBxPyP2)
    putStrLn $ "(x + y + 2)mono[x=-1,y=-1] = " ++ (show $ evalPolyOnInterval eff [(-1),(-1)] xPyP1P1)
    putStrLn $ "(x + y + 2)mono[x=[-1,0],y=[-1,0]] = " ++ (show $ evalPolyOnInterval eff [(-1) </\> 0,(-1) </\> 0] xPyP1P1)
    putStrLn $ "(x^2 + 2xy + 4x + 1)mono[x=-1,y=-1] = " ++ (show $ evalPolyOnInterval eff [(-1),(-1)] integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)mono[x=[-1,0],y=[-1,0]] = " ++ (show $ evalPolyOnInterval eff [(-1) </\> 0,(-1) </\> 0] integTwoBxPyP2)
    putStrLn $ "(x + y + 2)generic[x=-1,y=-1] = " ++ (show $ evalOtherType evalOpsOutCf (Map.fromList [("x",-1),("y",-1)]) xPyP1P1)
    putStrLn $ "(x + y + 2)generic[x=[-1,0],y=[-1,0]] = " ++ (show $ evalOtherType evalOpsOutCf (Map.fromList [("x",(-1) </\> 0),("y",(-1) </\> 0)]) xPyP1P1)
    putStrLn $ "(x^2 + 2xy + 4x + 1)generic[x=-1,y=-1] = " ++ (show $ evalOtherType evalOpsOutCf (Map.fromList [("x",-1),("y",-1)]) integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)generic[x=[-1,0],y=[-1,0]] = " ++ (show $ evalOtherType evalOpsOutCf (Map.fromList [("x",(-1) </\> 0),("y",(-1) </\> 0)]) integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)subst[x=[y,y]] = " ++ (showP $ substPolyMainVar eff 0 y integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)subst[x=[0,y]] = " ++ (showP $ substPolyMainVar eff 0 (c01 <*> y) integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)subst[x=[0,x]] = " ++ (showP $ substPolyMainVar eff 0 (c01 <*> x) integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)subst[x=[x^2-1,0]] = " ++ (showP $ substPolyMainVar eff 0 (RefOrd.fromEndpointsOutWithDefaultEffort ((x <*> x <-> c1), c0)) integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)substElim[x=[2,2]] = " ++ (showP $ substPolyMainVarElim eff 0 2 integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)substElim[x=[0,2]] = " ++ (showP $ substPolyMainVarElim eff 0 (0 </\> 2) integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)[swap order of x,y] = " ++ (showP $ polySwapFirstTwoVars integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)[x->z] = " ++ (showP $ polyRenameMainVar "z" integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)[add z] = " ++ (showP $ polyAddMainVar "z" (0 </\> 2) integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)[degree 1] = " ++ (showP $ reducePolyDegree eff $ changeSizeLimits cfgDeg1 integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)[degree 0] = " ++ (showP $ reducePolyDegree eff $ changeSizeLimits cfgDeg0 integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)[size 2] = " ++ (showP $ reducePolyTermCount eff $ changeSizeLimits cfgSize2 integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)[size 1] = " ++ (showP $ reducePolyTermCount eff $ changeSizeLimits cfgSize1 integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)[size 2, x tiny] = " ++ (showP $ reducePolyTermCount eff $ changeSizeLimits cfgXTinySize2 integTwoBxPyP2)
    putStrLn $ "(x^2 + 2xy + 4x + 1)[size 1, x tiny] = " ++ (showP $ reducePolyTermCount eff $ changeSizeLimits cfgXTinySize1 integTwoBxPyP2)
    putStrLn $ "sin x [n = 1] = " ++ (showP $ sinePoly eff 1 x)
    putStrLn $ "sin x [n = 2] = " ++ (showP $ sinePoly eff 2 x)
    putStrLn $ "sin x [n = 3] = " ++ (showP $ sinePoly eff 3 x)
--    putStrLn $ "exp(x + y + 2) = " ++ (showP $ expBxPyP2)
    where
    mOneToZ = (-1) </\> 0

showP p = showPoly id show p -- ++ " [" ++ show p ++ "]"
showPPair (p1,p2) = "(" ++ showP p1 ++ "," ++ showP p2 ++ ")" 

c1,c0,x,y,c01 :: Poly
x = newProjection cfg dombox "x"
y = newProjection cfg dombox "y"
c0 = newConstFn cfg dombox 0
c1 = newConstFn cfg dombox 1
c01 = newConstFn cfg dombox $ 0 </\> 1

xPy = x <+> y
xMy = x <-> y
xPyP1P1 = xPy <+> c1 <+> c1
xPyBTxMyB = xPy <*> xMy
twoBxPyP2 = (2::Int) |<*> xPyP1P1
integTwoBxPyP2 = integratePolyMainVar eff 0 c1 twoBxPyP2
--expBxPyP2 = exp xPyP1P1

eff = (100, (100,()))

evalOpsOutCf = evalOpsOut eff x (0::MI)

numCompare a b =
    NumOrd.pCompareInFullEff (NumOrd.pCompareDefaultEffort a) a b

cfg =
    IntPolyCfg
        {
            ipolycfg_vars = vars,
            ipolycfg_doms = doms,
            ipolycfg_sample_cf = 0 :: MI,
            ipolycfg_maxdeg = 4,
            ipolycfg_maxsize = 30
        }

cfgDeg0 = cfg { ipolycfg_maxdeg = 0 }
cfgDeg1 = cfg { ipolycfg_maxdeg = 1 }

cfgSize1 = cfg { ipolycfg_maxsize = 1 }
cfgSize2 = cfg { ipolycfg_maxsize = 2 }
cfgSize3 = cfg { ipolycfg_maxsize = 3 }

cfgXTiny = cfg { ipolycfg_doms = domsXTiny }

cfgXTinySize1 = cfgXTiny { ipolycfg_maxsize = 1 }
cfgXTinySize2 = cfgXTiny { ipolycfg_maxsize = 2 }

dombox = Map.fromList $ zip vars doms

vars = ["x", "y"]

doms :: [MI]
doms = [(0 </\> 1), 0 </\> 1]

domsXTiny :: [MI]
domsXTiny = [(0 </\> 0.0625), (0 </\> 1)]

