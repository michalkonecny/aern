module Main where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly

import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.Basis.Double
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
    let (fns, fnmeta) = (fnsSinsin, fnmetaSinsin)
--    let (fns, fnmeta) = (fnsMinmaxInOut, fnmetaMinmaxInOut)
--    let (fns, fnmeta) = (fnsTest, fnmetaTest)
    fnDataTV <- atomically $ newTVar $ FV.FnData fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
--    putStrLn "plot main: calling FV.new"
    FV.new samplePoly effDrawFn effCF effCF (fnDataTV, fnMetaTV) Nothing
--    putStrLn "plot main: FV.new completed"
--    Concurrent.forkIO $ signalFn fnMetaTV
    Gtk.mainGUI
    
--signalFn fnMetaTV =
--    do
--    atomically $
--        do
--        fnmeta <- readTVar fnMetaTV
--        writeTVar fnMetaTV $ fnmeta { FV.dataFnsUpdated = True }

--showP p = showPoly id show p -- ++ " [" ++ show p ++ "]"

---------------------------------
fnmetaSinsin = (FV.defaultFnMetaData x)
    {
        FV.dataFnGroupNames = ["sin(3x+1)","sin(sin(3x)+1)"],
        FV.dataFnNames = 
            [
             [
                "sin(sin(3x)+1)",
                "sin(sin(3x)+1) approx"
             ]
             ,
             [
                "sin(3x+1)",
                "sin(3x+1) approx"
             ]
            ],
        FV.dataFnStyles = 
            [[black, black], [blue, blue]]
    }

fnsSinsin :: [[Poly]]
fnsSinsin = 
    [
     [
        sinsin3xPlus1Accurate,
        sinsin3xPlus1Thick
     ]
     ,
     [
        sin3xPlus1Accurate,
        sin3xPlus1Thick
     ]
    ]
    

sinsin3xPlus1Accurate =
    sineOutPoly ((),()) 4 effCF 35  $
        (sineOutPolyThin  ((),()) 4 effCF 35 xTimes3D200) <+>| (1 :: Int) 
    
sinsin3xPlus1Thick =
    sineOutPoly ((),()) 4 effCF 15  $ 
        (sineOutPolyThin  ((),()) 4 effCF 15 xTimes3D80) <+>| (1 :: Int) 

sin3xPlus1Accurate =
    sineOutPolyThin  ((),()) 4 effCF 15 $ xTimes3D30 <+>| (1 :: Int) 
    
sin3xPlus1Thick =
    sineOutPolyThin  ((),()) 4 effCF 5 $ xTimes3D4 <+>| (1 :: Int) 
    
xTimes3D200, 
  xTimes3D100, xTimes3D90, xTimes3D80, xTimes3D70, xTimes3D60, 
  xTimes3D50, xTimes3D40, xTimes3D30, xTimes3D20, xTimes3D10, xTimes3D4 :: Poly    
xTimes3D200 = (xD 200) <*>| (3 :: Int)
xTimes3D100 = (xD 100) <*>| (3 :: Int)
xTimes3D90 = (xD 90) <*>| (3 :: Int)
xTimes3D80 = (xD 80) <*>| (3 :: Int)
xTimes3D70 = (xD 70) <*>| (3 :: Int)
xTimes3D60 = (xD 60) <*>| (3 :: Int)
xTimes3D50 = (xD 50) <*>| (3 :: Int)
xTimes3D40 = (xD 40) <*>| (3 :: Int)
xTimes3D30 = (xD 30) <*>| (3 :: Int)
xTimes3D20 = (xD 20) <*>| (3 :: Int)
xTimes3D10 = (xD 10) <*>| (3 :: Int)
xTimes3D4 = (xD 4) <*>| (3 :: Int)
      
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
--    
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


evalOpsOutCf = evalOpsOut effCF x (0::CF)

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
    
--fnsTest :: [[Poly]]
--fnsTest = 
--    [[
--        a1,
--        a2,
--        v1,
--        v2
--    ]]
--    
--a1,a2,v1,v2 :: Poly
--a1 = 
--    x <*>| (constructCF (0.9929615885375352) (0.9929615885375354)) 
--        <+>| (constructCF (1.0000063533391987) (1.000006353339199)) 
--a2 = 
--    x <*>| (constructCF (-269.30527783337715) (-269.30527783337686)) 
--        <+>| (constructCF (272.29824577525386) (272.2982457752539)) 
--v1 = NumOrd.maxOutEff (minmaxInOutDefaultEffortIntPolyWithBezierDegree 6 x) a1 a2
--v2 = NumOrd.maxInEff (minmaxInOutDefaultEffortIntPolyWithBezierDegree 6 x) a2 a1  
--
--    
--fnmetaTest = (FV.defaultFnMetaData x)
--    {
--        FV.dataFnGroupNames = ["test"],
--        FV.dataFnNames = 
--            [["0.5-x","0","maxOut 0 _", "maxIn _ 0"]],
--        FV.dataFnStyles = 
--            [[black, black, blue, blue]]
--    }
--
--{- Error report being investigated:
--
--unsafe: roundedCommutative: leqIfDefined: val1 <= val2 failed:
-- val1 = IntPoly{[_-269.30527783337715,-269.30527783337686^]"x" + [_272.29824577525386,272.2982457752539^]; cfg{[("x",[_0.0,1.0^])];4/30}; V{"x"/[(0,C{[_272.29824577525386,272.2982457752539^]}),(1,C{[_-269.30527783337715,-269.30527783337686^]})]}}
-- val2 = IntPoly{[_-269.3052778333772,-269.3052778333768^]"x" + [_272.2982457752538,272.298245775254^]; cfg{[("x",[_0.0,1.0^])];4/30}; V{"x"/[(0,C{[_272.2982457752538,272.298245775254^]}),(1,C{[_-269.3052778333772,-269.3052778333768^]})]}}
--UniformlyOrderedPair (
--  IntPoly{[_0.9929615885375352,0.9929615885375354^]"x" + [_1.0000063533391987,1.000006353339199^]; cfg{[("x",[_0.0,1.0^])];4/30}; V{"x"/[(0,C{[_1.0000063533391987,1.000006353339199^]}),(1,C{[_0.9929615885375352,0.9929615885375354^]})]}},
--  IntPoly{[_-269.30527783337715,-269.30527783337686^]"x" + [_272.29824577525386,272.2982457752539^]; cfg{[("x",[_0.0,1.0^])];4/30}; V{"x"/[(0,C{[_272.29824577525386,272.2982457752539^]}),(1,C{[_-269.30527783337715,-269.30527783337686^]})]}})
--(((429,((),((),()))),()),((((((),((),())),()),((),((),())),()),((69,((),((),()))),((),((),()))),(((),((),())),(((),((),())),((),((),(),()),(((),(),((),())),()))),cfg{[("x1",[_1.9244607101950584e-166,1.2657998066848274e-17^]),("x2",[_0.0,1.3976435349666616^]),("x3",[_6.441276949365498e-107,0.3401295543363387^]),("x4",[_0.0,1.0^]),("x5",[_0.0,0.870610416865817^]),("x6",[_1.2158278549217246e-101,1.0087914398193338^])];1/809}),(((),((),())),(((),((),())),(((),(),()),((),(),()),(((),(),((),())),()))))),10,(),()))
--
--  join commutative: [Failed]
--Falsifiable with seed -1963584063, after 13 tests. Reason: Falsifiable
--
---}
--    