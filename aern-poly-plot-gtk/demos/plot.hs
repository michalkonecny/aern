module Main where

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot ()

import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

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
type FnEndpt = IntPoly String CF
type Fn = Interval FnEndpt

main =
    do
    -- enable multithreaded GUI:
    Gtk.unsafeInitGUIForThreadedRTS
--    let (fns, fnmeta) = (fnsMinmaxOut, fnmetaMinmaxOut)
    let (fns, fnmeta) = (fnsTest, fnmetaTest)
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
--    putStrLn "plot main: calling FV.new"
    FV.new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
--    putStrLn "plot main: FV.new completed"
--    Concurrent.forkIO $ signalFn fnMetaTV
    Gtk.mainGUI
    
addPlotVar :: [[Fn]] -> [[(Fn, String)]]
addPlotVar fns =
    map (map addV) fns
    where
    addV fn = (fn, plotVar)
    (plotVar : _) = vars    
    

fnsMinmaxOut :: [[Fn]]    
fnsMinmaxOut = 
    [ 
     [x, cOneOver16, 
      NumOrd.maxOutEff effMinmaxInOut (x) cOneOver16
     ]
     ,
     [x, cSevenOver16, 
      NumOrd.maxOutEff effMinmaxInOut (x) cSevenOver16
     ]
     ,
     [x, cOneMinusOneOver16, 
      NumOrd.maxOutEff effMinmaxInOut (x) cOneMinusOneOver16
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
    
black :: FV.FnPlotStyle
black = FV.defaultFnPlotStyle
blue :: FV.FnPlotStyle
blue = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.1,0.1,0.8,1), 
        FV.styleFillColour = Just (0.1,0.1,0.8,0.1) 
    } 
red :: FV.FnPlotStyle
red = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.8,0.2,0.2,1), 
        FV.styleFillColour = Just (0.8,0.2,0.2,0.1) 
    } 
    
c1,c0,x,c01,c10,cHalf,cHalf1,cOneOver16,cSevenOver16,cOneMinusOneOver16, sampleFn :: Fn
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

sampleFn = x
sampleFnEndpt = xE 

xE = newProjection cfg dombox "x" :: FnEndpt
c0E = newConstFn cfg dombox 0 :: FnEndpt

--eff = (100, (100,())) -- MPFR
effCF = ArithInOut.roundedRealDefaultEffort (0:: CF)
effNumComp = NumOrd.pCompareDefaultEffort sampleFn
effRefComp = RefOrd.pCompareDefaultEffort sampleFn
effMinmaxInOut = 
    minmaxUpDnDefaultEffortIntPolyWithBezierDegree 10 sampleFnEndpt
effDrawFn = cairoDrawFnDefaultEffort sampleFn
effEval = evaluationDefaultEffort sampleFn

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
--fnsSinsin :: [[Fn]]
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
--  xTimes3D50, xTimes3D40, xTimes3D30, xTimes3D20, xTimes3D10, xTimes3D4 :: Fn    
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

--fnsMixed :: [[Fn]]
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

--fnsMinmaxUpDn :: [[Fn]]    
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

--fnsMinmaxInOut :: [[Fn]]    
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
-------------
    
fnsTest :: [[Fn]]
fnsTest = 
    [
        [
            a1,
            b1,
            a1 <*> b1
        ]
        ,
        [
            mkInterval a1E,
            mkInterval b1E,
            mkInterval $ a1E <*> b1E
        ]
    ]
    where
    mkInterval e = Interval e e
    
a1,b1 :: Fn
a1 =
--  (x-0.5) + [_0,0.5^]
    (x <+>| (-0.5 :: Double)) 
    <+>| 
    (constructCF 0 (0.5))
b1 = 
--  [_-1,1^]
    c0 <+>|
    (constructCF (-1) (1))

a1E,b1E :: FnEndpt
a1E =
--  -((x-0.5) + [_0,0.5^])
    neg $
        (xE <+>| (-0.5 :: Double)) 
        <+>| 
        (constructCF 0 (0.5))
b1E = 
--  [_-1,1^]
    c0E <+>|
    (constructCF (-1) (1))


fnmetaTest = (FV.defaultFnMetaData x)
    {
        FV.dataFnGroupNames = ["poly int" ,"int poly"]
        ,
        FV.dataFnNames = 
            [
                ["a1","b1","a1 <*> b1"]
            ,
                ["a1","b1","a1 <*> b1"]
            ]
        ,
        FV.dataFnStyles = 
            [
                replicate 3 blue
            , 
                replicate 3 red
            ]
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