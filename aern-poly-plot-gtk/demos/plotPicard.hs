module Main where

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot

import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration

--import Numeric.AERN.RealArithmetic.Basis.Double
--import Numeric.AERN.RealArithmetic.Basis.MPFR
--import Numeric.AERN.Basics.Interval

import qualified Numeric.AERN.DoubleBasis.Interval as CF


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

import System.Environment


--type CF = Interval MPFR
type CF = CF.DI
type Poly = IntPoly String CF

main =
    do
--    hSetBuffering stdout LineBuffering
    [ivpName, maxdegS, itersS] <- getArgs
    let maxdeg = (read maxdegS) :: Int
    let iters = (read itersS) :: Int
    
    -- enable multithreaded GUI:
    Gtk.unsafeInitGUIForThreadedRTS
    let (fns, fnmeta) = getFns ivpName maxdeg iters
    fnDataTV <- atomically $ newTVar $ FV.FnData fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    FV.new samplePoly effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
--    Concurrent.forkIO $ signalFn fnMetaTV
    Gtk.mainGUI
    
getFns "smass-ev" maxdeg iters = 
    (fnsSpringMass_ev maxdeg iters, fnmetaSpringMass iters)
    
getFns "smass-uv" maxdeg iters = 
    (fnsSpringMass_uv maxdeg iters, fnmetaSpringMass iters)
    
getFns "smass-uvp" maxdeg iters = 
    (fnsSpringMass_uvp_mono maxdeg iters, fnmetaSpringMass iters)
    
--getFns "smass-b" maxdeg iters = 
--    (fnsSpringMass_b maxdeg iters, fnmetaSpringMass_b 1)
    

--showP p = showPoly id show p -- ++ " [" ++ show p ++ "]"


---------------------------------
fieldSpringMass [x,x'] = [x',(-1 :: Int) |<*> x]
--fieldSpringMass [x,x'] = [x',(-4 :: Int) |<*> x]
--fieldSpringMass [x,x'] = [x',((-4 :: Int) |<*> x) <+> (x' <*> x' </>| (4::Int))]

initValuesSpringMass_ev :: [CF]
initValuesSpringMass_ev = [1,0]

initValuesSpringMass_uv :: [CF]
initValuesSpringMass_uv = [0 </\> 1,0]

fnmetaSpringMass iters = 
    (FV.defaultFnMetaData samplePoly)
    {
        FV.dataFnGroupNames = ["segment 1"],
        FV.dataFnNames = 
            [
                concat $ map (\n -> ["x" ++ n, "x'" ++ n]) $ 
                    map show $ [1..iters]
            ],
        FV.dataFnStyles = 
            [ 
                concat $ replicate iters [blue,green]
            ]
        ,
        FV.dataDefaultActiveFns =
            [
                True : True : (replicate (2*(iters - 1)) False)
            ]
        ,
        FV.dataDomL = 0,
        FV.dataDomR = 1,
        FV.dataValLO = -1,
        FV.dataValHI = 2,
        FV.dataDefaultEvalPoint = 1,
        FV.dataDefaultCanvasParams =
            (FV.defaultCanvasParams (0::CF))
            {
                FV.cnvprmCoordSystem = 
                    FV.CoordSystemLinear $ 
                        FV.Rectangle  2 (-1) 0 (1)
                ,
                FV.cnvprmSamplesPerUnit = 200
            }
    }

black = FV.defaultFnPlotStyle
blue = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.1,0.1,0.8,1), 
        FV.styleFillColour = Just (0.1,0.1,0.8,0.1) 
    } 
green = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.1,0.8,0.1,1), 
        FV.styleFillColour = Just (0.1,0.8,0.1,0.1) 
    } 

fnsSpringMass_ev :: Int -> Int -> [[Poly]]
fnsSpringMass_ev maxdeg iters = 
    [
        concat $ take iters $ enclosures maxdeg
    ]
    where
    enclosures maxdeg = 
        iterate (picardOp fieldSpringMass $ initValsFns) initialApprox
        where
        initValsFns =
            map (newConstFn (cfg1D maxdeg) dombox1) initValuesSpringMass_ev
        initialApprox =
            map (<+>| (constructCF (-0.5) 0.5)) initValsFns
    
fnsSpringMass_uv :: Int -> Int -> [[Poly]]
fnsSpringMass_uv maxdeg iters = 
    [
        concat $ take iters $ enclosures maxdeg
    ]
    where
    enclosures maxdeg = 
        iterate (picardOp fieldSpringMass $ initValsFns) initialApprox
        where
        initValsFns =
            map (newConstFn (cfg1D maxdeg) dombox1) initValuesSpringMass_uv
        initialApprox =
            map (<+>| (constructCF (-0.5) 0.5)) initValsFns

fnsSpringMass_uvp_mono :: Int -> Int -> [[Poly]]
fnsSpringMass_uvp_mono maxdeg iters = 
    [
        concat $ take iters $ enclosures maxdeg
    ]
    where
    enclosures maxdeg =
        zipWith (zipWith (</\>)) (enclosures [0,0]) (enclosures [1,0])
        where 
        enclosures initVals =
            iterate (picardOp fieldSpringMass $ initValsFns) initialApprox
            where
            initValsFns =
                map (newConstFn (cfg1D maxdeg) dombox1) initVals
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns

fnsSpringMass_uvp :: Int -> Int -> [[Poly]]
fnsSpringMass_uvp maxdeg iters = 
    [
        map removeParams $ concat $ take iters $ enclosures maxdeg
    ]
    where
    removeParams fn = 
        pEvalAtPointOutEff (partialEvaluationDefaultEffort fn) dombox1POnly fn
    enclosures maxdeg = 
        iterate (picardOp fieldSpringMass $ initValsFns) initialApprox
        where
        initValsFns =
            map initValsFn paramVars
        initValsFn paramVar =
            newProjection (cfg1PD maxdeg) dombox1P paramVar
        initialApprox =
            map (<+>| (constructCF (-0.5) 0.5)) initValsFns

picardOp ::
    ([Poly] -> [Poly]) ->
    [Poly] {-^ initial values -} -> 
    [Poly] {-^ approximates of y -} -> 
    [Poly] {-^ improved approximates of y -}
picardOp field [y0, yDer0] [yPrev, yDerPrev] =
    zipWith picardOneFn [y0, yDer0] $ field [yPrev, yDerPrev]
    where
    picardOneFn initValFn deriveApproxFn =
        initValFn <+> primitFn deriveApproxFn
    primitFn xd =
        primitiveFunctionOutEff effCF xd "t"
    
    
--c1,c0,x,c01,c10,cHalf,cHalf1,cOneOver16,cSevenOver16,cOneMinusOneOver16, samplePoly :: Poly
samplePoly = newProjection cfg1 dombox1 "x"
--xD maxdeg = newProjection (cfgD maxdeg) dombox "x"
--c0 = newConstFn cfg dombox 0
--c1 = newConstFn cfg dombox 1
--cHalf = newConstFn cfg dombox 0.5
--cHalf1 = newConstFn cfg dombox $ 0.5 </\> 1
--cOneOver16 = newConstFn cfg dombox $ 0.5^4
--cSevenOver16 = newConstFn cfg dombox $ 7 * 0.5^4
--cOneMinusOneOver16 = newConstFn cfg dombox $ 15 * 0.5^4
--c01 = newConstFn cfg dombox $ 0 </\> 1
--c10 = newConstFn cfg dombox $ 1 <\/> 0

----eff = (100, (100,())) -- MPFR
effCF = ArithInOut.roundedRealDefaultEffort (0:: CF)
effEval = evaluationDefaultEffort samplePoly
effNumComp = NumOrd.pCompareDefaultEffort samplePoly
effRefComp = RefOrd.pCompareDefaultEffort samplePoly
minmaxUpDnEff = minmaxUpDnDefaultEffortIntPolyWithBezierDegree 10 samplePoly
minmaxInOutEff = minmaxInOutDefaultEffortIntPolyWithBezierDegree 10 samplePoly
effDrawFn = cairoDrawFnDefaultEffort samplePoly
--
--
--evalOpsOutCf = evalOpsOut effCF x (0::CF)

cfg1 = cfg1D 10
cfg1D maxdeg =
    IntPolyCfg
        {
            ipolycfg_vars = vars,
            ipolycfg_domsLZ = doms1LZ,
            ipolycfg_domsLE = doms1LE,
            ipolycfg_sample_cf = 0 :: CF,
            ipolycfg_maxdeg = maxdeg,
            ipolycfg_maxsize = 100
        }

cfg1PD maxdeg =
    IntPolyCfg
        {
            ipolycfg_vars = vars ++ paramVars,
            ipolycfg_domsLZ = doms1PLZ,
            ipolycfg_domsLE = doms1PLE,
            ipolycfg_sample_cf = 0 :: CF,
            ipolycfg_maxdeg = maxdeg,
            ipolycfg_maxsize = 100
        }

dombox1 = Map.fromList $ zip ["t"] doms1
dombox1P = Map.fromList $ zip ("t" : paramVars) doms1P
dombox1POnly = Map.fromList $ zip paramVars [constructCF 0 1,0] -- initValuesSpringMass_uv

vars = ["t"]
paramVars = ["xi", "x'i"]

doms1LE :: [CF]
doms1LZ = [constructCF 0 1]
doms1LZ :: [CF]
doms1LE = [constructCF 0 0]
doms1 :: [CF]
doms1 = [constructCF 0 1]

doms1PLE :: [CF]
doms1PLZ = [constructCF 0 1, constructCF 0 1, 0]
doms1PLZ :: [CF]
doms1PLE = [0, 0, 0]
doms1P :: [CF]
doms1P = [constructCF 0 1] ++ initValuesSpringMass_uv

--segs = [CF]
--segs = 

constructCF :: Double -> Double -> CF
constructCF l r =
    RefOrd.fromEndpointsOutWithDefaultEffort (cf0 <+>| l, cf0 <+>| r)
cf0 = 0 :: CF
    
