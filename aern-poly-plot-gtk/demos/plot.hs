module Main where

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot ()

import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import Numeric.AERN.RmToRn.New
--import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.Basis.Double ()
import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.Basics.Interval

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Effort

import Numeric.AERN.Misc.Debug

import qualified Graphics.UI.Gtk as Gtk

import qualified Control.Concurrent as Concurrent
import Control.Concurrent.STM

import qualified Data.Map as Map
import qualified Data.List as List


--type CF = Interval MPFR
type CF = Interval Double
type Poly = IntPoly String CF

main =
    do
--    putStrLn $ "comparing v1 and v2: " 
--        ++ (show $ RefOrd.pCompareInFullEff effRefComp v1 v2)
--        ++ "\n a1 = " ++ showP a1
--        ++ "\n a2 = " ++ showP a2
--        ++ "\n v1 = " ++ showP v1
--        ++ "\n v2 = " ++ showP v2
--    Gtk.initGUI
    -- enable multithreaded GUI:
    Gtk.unsafeInitGUIForThreadedRTS
--    let (fns, fnmeta) = (fnsSinsin, fnmetaSinsin)
--    let (fns, fnmeta) = (fnsMinmaxInOut, fnmetaMinmaxInOut)
    let (fns, fnmeta) = (fnsMinmaxOut, fnmetaMinmaxOut)
--    let (fns, fnmeta) = (fnsTest, fnmetaTest)
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
--    putStrLn "plot main: calling FV.new"
    FV.new samplePoly effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
--    putStrLn "plot main: FV.new completed"
--    Concurrent.forkIO $ signalFn fnMetaTV
    Gtk.mainGUI
    
addPlotVar :: [[Poly]] -> [[(Poly, String)]]
addPlotVar fns =
    map (map addV) fns
    where
    addV fn = (fn, plotVar)
    (plotVar : _) = vars    
    
--signalFn fnMetaTV =
--    do
--    atomically $
--        do
--        fnmeta <- readTVar fnMetaTV
--        writeTVar fnMetaTV $ fnmeta { FV.dataFnsUpdated = True }

--showP p = showPoly id show p -- ++ " [" ++ show p ++ "]"

-----------------------------------
--fnmetaSinsin = (FV.defaultFnMetaData x)
--    {
--        FV.dataFnGroupNames = ["sin(3x+1)","sin(sin(3x)+1)"],
--        FV.dataFnNames = 
--            [
--             [
--                "sin(sin(3x)+1)",
--                "sin(sin(3x)+1) approx"
--             ]
--             ,
--             [
--                "sin(3x+1)",
--                "sin(3x+1) approx"
--             ]
--            ],
--        FV.dataFnStyles = 
--            [[black, black], [blue, blue]]
--    }
--
--fnsSinsin :: [[Poly]]
--fnsSinsin = 
--    [
--     [
--        sinsin3xPlus1Accurate,
--        sinsin3xPlus1Thick
--     ]
--     ,
--     [
--        sin3xPlus1Accurate,
--        sin3xPlus1Thick
--     ]
--    ]
--    
--
--sinsin3xPlus1Accurate =
--    sineOutPoly ((),()) 4 effCF 35  $
--        (sineOutPolyThin  ((),()) 4 effCF 35 xTimes3D200) <+>| (1 :: Int) 
--    
--sinsin3xPlus1Thick =
--    sineOutPoly ((),()) 4 effCF 15  $ 
--        (sineOutPolyThin  ((),()) 4 effCF 15 xTimes3D80) <+>| (1 :: Int) 
--
--sin3xPlus1Accurate =
--    sineOutPolyThin  ((),()) 4 effCF 15 $ xTimes3D30 <+>| (1 :: Int) 
--    
--sin3xPlus1Thick =
--    sineOutPolyThin  ((),()) 4 effCF 5 $ xTimes3D4 <+>| (1 :: Int) 
--    
--xTimes3D200, 
--  xTimes3D100, xTimes3D90, xTimes3D80, xTimes3D70, xTimes3D60, 
--  xTimes3D50, xTimes3D40, xTimes3D30, xTimes3D20, xTimes3D10, xTimes3D4 :: Poly    
--xTimes3D200 = (xD 200) <*>| (3 :: Int)
--xTimes3D100 = (xD 100) <*>| (3 :: Int)
--xTimes3D90 = (xD 90) <*>| (3 :: Int)
--xTimes3D80 = (xD 80) <*>| (3 :: Int)
--xTimes3D70 = (xD 70) <*>| (3 :: Int)
--xTimes3D60 = (xD 60) <*>| (3 :: Int)
--xTimes3D50 = (xD 50) <*>| (3 :: Int)
--xTimes3D40 = (xD 40) <*>| (3 :: Int)
--xTimes3D30 = (xD 30) <*>| (3 :: Int)
--xTimes3D20 = (xD 20) <*>| (3 :: Int)
--xTimes3D10 = (xD 10) <*>| (3 :: Int)
--xTimes3D4 = (xD 4) <*>| (3 :: Int)
      
---------------------------------

--fnsMixed :: [[Poly]]
--fnsMixed = 
--    [[
--        x <*> cHalf1,
--        x <*> cHalf1 <+> (c10 </>| (4::Int))
--    ]]
--    
--fnmetaMixed = (FV.defaultFnMetaData x)
--    {
--        FV.dataFnGroupNames = ["consistency"],
--        FV.dataFnNames = 
--            [["thick","mixed"]],
--        FV.dataFnStyles = 
--            [[black, blue]]
--    }

---------------------------------

--fnsMinmaxUpDn :: [[Poly]]    
--fnsMinmaxUpDn = 
--    [ 
--     [x <*> cHalf1, cOneOver16, 
--      NumOrd.maxUpEff minmaxUpDnEff (x <*> cHalf1) cOneOver16,  
--      NumOrd.maxDnEff minmaxUpDnEff (x <*> cHalf1) cOneOver16
--     ]
--     ,
--     [x <*> cHalf1, cSevenOver16, 
--      NumOrd.maxUpEff minmaxUpDnEff (x <*> cHalf1) cSevenOver16,  
--      NumOrd.maxDnEff minmaxUpDnEff (x <*> cHalf1) cSevenOver16
--     ]
--     ,
--     [x <*> cHalf1, cOneMinusOneOver16, 
--      NumOrd.maxUpEff minmaxUpDnEff (x <*> cHalf1) cOneMinusOneOver16,  
--      NumOrd.maxDnEff minmaxUpDnEff (x <*> cHalf1) cOneMinusOneOver16
--     ]
--    ]

--fnsMinmaxInOut :: [[Poly]]    
--fnsMinmaxInOut = 
--    [ 
--     [x <*> cHalf1, cOneOver16, 
--      NumOrd.maxOutEff minmaxInOutEff (x <*> cHalf1) cOneOver16,  
--      NumOrd.maxInEff minmaxInOutEff (x <*> cHalf1) cOneOver16
--     ]
--     ,
--     [x <*> cHalf1, cSevenOver16, 
--      NumOrd.maxOutEff minmaxInOutEff (x <*> cHalf1) cSevenOver16,  
--      NumOrd.maxInEff minmaxInOutEff (x <*> cHalf1) cSevenOver16
--     ]
--     ,
--     [x <*> cHalf1, cOneMinusOneOver16, 
--      NumOrd.maxOutEff minmaxInOutEff (x <*> cHalf1) cOneMinusOneOver16,  
--      NumOrd.maxInEff minmaxInOutEff (x <*> cHalf1) cOneMinusOneOver16
--     ]
--    ]
--
--fnmetaMinmaxUpDn = (FV.defaultFnMetaData x)
--    {
--        FV.dataFnGroupNames = ["minimax 1/16", "minmax 7/16", "minmax 15/16"],
--        FV.dataFnNames = 
--            [["x", "1/16", "maxUp", "maxDn"], 
--             ["x", "7/16", "maxUp", "maxDn"],
--             ["x", "15/16", "maxUp", "maxDn"]],
--        FV.dataFnStyles = 
--            [[black, black, black, black],
--             [black, black, black, black],
--             [black, black, black, black]]
--    }
--
--fnmetaMinmaxInOut = (FV.defaultFnMetaData x)
--    {
--        FV.dataFnGroupNames = ["minimax 1/16", "minmax 7/16", "minmax 15/16"],
--        FV.dataFnNames = 
--            [["x", "1/16", "maxOut", "maxIn"], 
--             ["x", "7/16", "maxOut", "maxIn"],
--             ["x", "15/16", "maxOut", "maxIn"]],
--        FV.dataFnStyles = 
--            [[black, black, black, black],
--             [black, black, black, black],
--             [black, black, black, black]]
--    }

fnsMinmaxOut :: [[Poly]]    
fnsMinmaxOut = 
    [ 
     [x, cOneOver16, 
      NumOrd.maxOutEff minmaxInOutEff (x) cOneOver16
     ]
     ,
     [x, cSevenOver16, 
      NumOrd.maxOutEff minmaxInOutEff (x) cSevenOver16
     ]
     ,
     [x, cOneMinusOneOver16, 
      NumOrd.maxOutEff minmaxInOutEff (x) cOneMinusOneOver16
     ]
    ]

fnmetaMinmaxOut = (FV.defaultFnMetaData x)
    {
        FV.dataFnGroupNames = ["max 1/16", "max 7/16", "max 15/16"],
        FV.dataFnNames = 
            [["x", "1/16", "maxOut"], 
             ["x", "7/16", "maxOut"],
             ["x", "15/16", "maxOut"]],
        FV.dataFnStyles = 
            [[black, black, blue],
             [black, black, blue],
             [black, black, blue]]
    }
    
black = FV.defaultFnPlotStyle
blue = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.1,0.1,0.8,1), 
        FV.styleFillColour = Just (0.1,0.1,0.8,0.1) 
    } 
    
c1,c0,x,c01,c10,cHalf,cHalf1,cOneOver16,cSevenOver16,cOneMinusOneOver16, samplePoly :: Poly
x = newProjection cfg dombox "x"
xD maxdeg = newProjection (cfgD maxdeg) dombox "x"
c0 = newConstFn cfg dombox 0
c1 = newConstFn cfg dombox 1
cHalf = newConstFn cfg dombox 0.5
cHalf1 = newConstFn cfg dombox $ 0.5 </\> 1
cOneOver16 = newConstFn cfg dombox $ 0.5^4
cSevenOver16 = newConstFn cfg dombox $ 7 * 0.5^4
cOneMinusOneOver16 = newConstFn cfg dombox $ 15 * 0.5^4
c01 = newConstFn cfg dombox $ 0 </\> 1
c10 = newConstFn cfg dombox $ 1 <\/> 0

samplePoly = x

--eff = (100, (100,())) -- MPFR
effCF = ArithInOut.roundedRealDefaultEffort (0:: CF)
effNumComp = NumOrd.pCompareDefaultEffort x
effRefComp = RefOrd.pCompareDefaultEffort x
minmaxUpDnEff = minmaxUpDnDefaultEffortIntPolyWithBezierDegree 10 x
minmaxInOutEff = minmaxInOutDefaultEffortIntPolyWithBezierDegree 10 x
effDrawFn = cairoDrawFnDefaultEffort samplePoly
effEval = (effCF, Int1To10 10)

cfg = cfgD 4
cfgD maxdeg =
    IntPolyCfg
        {
            ipolycfg_vars = vars,
            ipolycfg_domsLZ = doms,
            ipolycfg_domsLE = replicate (length vars) 0,
            ipolycfg_sample_cf = 0 :: CF,
            ipolycfg_maxdeg = maxdeg,
            ipolycfg_maxsize = 1000
        }

dombox = Map.fromList $ zip vars doms

vars = ["x"]

doms :: [CF]
doms = [constructCF 0 1]

constructCF :: Double -> Double -> CF
constructCF l r =
    RefOrd.fromEndpointsOutWithDefaultEffort (cf0 <+>| l, cf0 <+>| r)
cf0 = 0 :: CF
    
-------------
    
fnsTest :: [[Poly]]
fnsTest = 
    [
        [
            a1,
            b1,
            a1 <*> b1
--            ,
--            a1 >*< b1
        ]
        ,
        [
            a2,
            b2,
            a2 <*> b2
--            ,
--            a2 >*< b2
        ]
    ]
    
a1,a2,b1,b2 :: Poly


a1 =
--  [_-1.0,0.0^]*x + [_2,3^]
 
    (constructCF (-1) (0.0)) |<*> x 
    <+>|
    (constructCF (2) (3))
    
b1 = 
--  [_1,2^]
    c0 <+>|
    (constructCF (1) (2))

a2 =
--  [_-226,0.0^]"x"^2 + [_-3e-7,0.0^]"x" + [^230,3_]
    (constructCF (-226) (0)) |<*> (x<^>(2 :: Int)) 
    <+> 
    (constructCF (-3E-7) (0)) |<*> x 
    <+>|
    (constructCF (230) (3))

b2 =
--  [_-1e-54,0.0^]"x"^2 + [_-0.5,0^]"x" + [^3.5,1_]
    (constructCF (- 1E-54) (0)) |<*> (x<^>(2 :: Int)) 
    <+> 
    (constructCF (-0.5) (0.0)) |<*> x 
    <+>|
    (constructCF (3.5) (1))


    
fnmetaTest = (FV.defaultFnMetaData x)
    {
        FV.dataFnGroupNames = ["mul1","mul2"]
        ,
        FV.dataFnNames = 
            [["a1","b1","a1<*>b1"], -- ,"a1>*<b1"],
             ["a2","b2","a2<*>b2"]] -- ,"a2>*<b2"]]
        ,
        FV.dataFnStyles = 
            [replicate 4 black, replicate 4 blue]
    }

{- Error report being investigated:

IntPoly-DI >*< <*>:

TwoLEPairs 
((IntPoly{[_-1.0,0.0^]"x" + [_2.0,3.0000000000000004^]; 
        cfg{[("x",[_0.0,1.0^])];4/30}; V{"x"/[(0,C{[_2.0,3.0000000000000004^]}),(1,C{[_-1.0,0.0^]})]}},
  IntPoly{[_-226.68234077903497,0.0^]"x"^2 + [_-3.8597885883944825e-7,0.0^]"x" + [^230.6823411650138,3.0000000000000004_]; 
        cfg{[("x",[_0.0,1.0^])];4/30}; V{"x"/[(0,C{[^230.6823411650138,3.0000000000000004_]}),(1,C{[_-3.8597885883944825e-7,0.0^]}),(2,C{[_-226.68234077903497,0.0^]})]}}),(
  IntPoly{[_0.9999999999999999,2.0000000000000004^]; 
        cfg{[("x",[_0.0,1.0^])];4/30}; V{"x"/[(0,C{[_0.9999999999999999,2.0000000000000004^]})]}},
  IntPoly{[_-1.3749702075180186e-54,0.0^]"x"^2 + [_-0.46609761298834296,0.0^]"x" + [^3.4660976129883427,1.0000000000000002_]; cfg{[("x",[_0.0,1.0^])];4/30}; V{"x"/[(0,C{[^3.4660976129883427,1.0000000000000002_]}),(1,C{[_-0.46609761298834296,0.0^]}),(2,C{[_-1.3749702075180186e-54,0.0^]})]}}))
((),((),()))
((240,((),((),()))),())

  refinement monotone: [Failed]
Falsifiable with seed -1029358060, after 2 tests. Reason: Falsifiable

simplified polynomials:
  [_-1,0^]*x + [_2,3^]
  [_-226,0.0^]"x"^2 + [_-3e-7,0.0^]"x" + [^230,3_]
  
  [_1,2^]
  [_-1e-54,0.0^]"x"^2 + [_-0.5,0^]"x" + [^3.5,1]

---
  [_-1e-54,-1e-286^]*x^2 + [_-0.5,0.0^]*x + [_1.5,3.5^]
  [_-226,-1e-54^]*x^2 + [^0.0,-0.5_]*x + [^231,2.5_]

  [^-2.2,-2.5_]*x + [_3,9^]
  [_-1e-54,0.0^]*x^2 + [^-0.5,-2.5_]*x + [_4.7,8.2^]
  
-}
--    