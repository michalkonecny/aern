{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot ()

import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import Numeric.AERN.RmToRn

--import Numeric.AERN.RealArithmetic.Basis.Double
--import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.Basics.Interval

import qualified Numeric.AERN.DoubleBasis.Interval as CF


import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators

--import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.Operators

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort

--import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.Consistency
--import Numeric.AERN.Basics.ShowInternals

import qualified Graphics.UI.Gtk as Gtk

--import qualified Control.Concurrent as Concurrent
import Control.Concurrent.STM

import qualified Data.Map as Map
--import qualified Data.List as List

import System.Environment

import Numeric.AERN.Misc.Debug
_ = unsafePrint

--type CF = Interval MPFR
type CF = CF.DI
type FnEndPt = IntPoly String CF
type Fn = Interval FnEndPt

main :: IO ()
main =
    do
--    hSetBuffering stdout LineBuffering

    -- process command-line parameters:
    args <- getArgs
    let [ivpName, maxdegS, itersS] = args 
    let maxdeg = (read maxdegS) :: Int
    let iters = (read itersS) :: Int
    
    -- enable multithreaded GUIs:
    _ <- Gtk.unsafeInitGUIForThreadedRTS
    
    -- set up the function view window:
    let (fns, fnmeta) = getFns ivpName maxdeg iters
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    _ <- FV.new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
    
    -- fire up the GUI:
    Gtk.mainGUI
    
addPlotVar :: [[Fn]] -> [[(Fn, String)]]
addPlotVar fns =
    map (map addV) fns
    where
    addV fn = (fn, tVar)
    
getFns :: String -> Int -> Int -> ([[Fn]], FV.FnMetaData Fn)
getFns name maxdeg iters =
    case Map.lookup name functionMap of
        Just (mkFns, mkFnMeta) ->
            (mkFns maxdeg iters, mkFnMeta iters)
        _ ->
            error $ "unknown IVP " ++ show name ++ ", known IVPs:\n" ++ unlines names 
    where
    functionMap = Map.fromList functions 
    names = map fst functions
    
functions :: 
    [(
       String
     ,
      (Int -> Int -> [[Fn]], Int -> FV.FnMetaData Fn)
     )
    ]
functions =
    [
        ("exp-uv", (fnsExp_uv, fnmetaExp)),
        ("exp-uvp", (fnsExp_uvp, fnmetaExp)),
        ("smass-ev", (fnsSpringMass_ev, fnmetaSpringMass)),
        ("smass-uv", (fnsSpringMass_uv, fnmetaSpringMass))
    ]


--showP p = showPoly id show p -- ++ " [" ++ show p ++ "]"


---------------------------------
fieldExp :: [Fn] -> [Fn]
fieldExp [x] = [(-1 :: Int) |<*> x]
fieldExp _ = error "fieldExp"

initValuesExp_uv :: [CF]
initValuesExp_uv = [(-1) </\> 1]

fnmetaExp :: Int -> FV.FnMetaData Fn
fnmetaExp iters = 
    (FV.defaultFnMetaData sampleFn)
    {
        FV.dataFnGroupNames = ["segment 1"],
        FV.dataFnNames = 
            [
                concat $ map (\n -> ["x" ++ n]) $ 
                    map show $ [1..iters]
            ],
        FV.dataFnStyles = 
            [ 
                concat $ replicate iters [blue]
            ]
        ,
        FV.dataDefaultActiveFns =
            [
                True : (replicate (iters - 1) False)
            ]
        ,
        FV.dataDomL = 0,
        FV.dataDomR = 1,
        FV.dataValLO = -1,
        FV.dataValHI = 1,
        FV.dataDefaultEvalPoint = 1,
        FV.dataDefaultCanvasParams =
            (FV.defaultCanvasParams (0::CF))
            {
                FV.cnvprmCoordSystem = 
                    FV.CoordSystemLinear $ 
                        FV.Rectangle  1 (-1) 0 (1)
                ,
                FV.cnvprmSamplesPerUnit = 200
            }
    }

fnsExp_uv :: Int -> Int -> [[Fn]]
fnsExp_uv maxdeg iters = 
    [
        concat $ take iters $ enclosures
    ]
    where
    enclosures = 
        iterate (picardOp fieldExp $ initValsFns) initialApprox
        where
        initValsFns =
            map (newConstFn (limitsD maxdeg) varDoms) initValuesExp_uv
        initialApprox =
            map (<+>| (constructCF (-0.5) 0.5)) initValsFns

fnsExp_uvp :: Int -> Int -> [[Fn]]
fnsExp_uvp maxdeg iters =
    unsafePrint
    (
        "fnsExp_uvp:\n"
        ++ unlines (map show $ zip resultWithParams result)
    ) $ 
    [
        result
    ]
    where
    result = map removeParams resultWithParams 
    resultWithParams = concat $ take iters $ enclosures
    removeParams fn = 
        pEvalAtPointOutEff (partialEvaluationDefaultEffort fn) domboxP1Only fn
    enclosures = 
        iterate (picardOp fieldExp $ initValsFns) initialApprox
        where
        initValsFns =
            map initValsFn paramVars1
        initValsFn paramVar =
            newProjection (limitsD maxdeg) varDomsP1 paramVar
        initialApprox =
            map (<+>| (constructCF (-0.5) 0.5)) initValsFns

fnsExp_uvp_mono :: Int -> Int -> [[CF.Interval Fn]]
fnsExp_uvp_mono maxdeg iters = 
    [
        concat $ take iters $ enclosures
    ]
    where
    enclosures =
        zipWith (zipWith CF.Interval) (enclosuresFromInit [-1]) (enclosuresFromInit [1])
        where 
        enclosuresFromInit initVals =
            iterate (picardOp fieldExp $ initValsFns) initialApprox
            where
            initValsFns =
                map (newConstFn (limitsD maxdeg) varDoms) initVals
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns

---------------------------------
fieldSpringMass :: 
    ArithInOut.RoundedMixedMultiply t Int =>
    [t] -> [t]
fieldSpringMass [x,x'] = [x',(-1 :: Int) |<*> x]
--fieldSpringMass [x,x'] = [x',(-4 :: Int) |<*> x]
--fieldSpringMass [x,x'] = [x',((-4 :: Int) |<*> x) <+> (x' <*> x' </>| (4::Int))]
fieldSpringMass _ = error "fieldSpringMass"

initValuesSpringMass_ev :: [CF]
initValuesSpringMass_ev = [1,0]

initValuesSpringMass_uv :: [CF]
initValuesSpringMass_uv = [0 </\> 1,0]

fnmetaSpringMass :: 
    Num (Domain f) =>
    Int -> FV.FnMetaData f
fnmetaSpringMass iters = 
    (FV.defaultFnMetaData sampleFn)
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

black :: FV.FnPlotStyle
blue :: FV.FnPlotStyle
black = FV.defaultFnPlotStyle
blue = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.1,0.1,0.8,1), 
        FV.styleFillColour = Just (0.1,0.1,0.8,0.1) 
    } 
green :: FV.FnPlotStyle
green = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.1,0.8,0.1,1), 
        FV.styleFillColour = Just (0.1,0.8,0.1,0.1) 
    } 

fnsSpringMass_ev :: Int -> Int -> [[Fn]]
fnsSpringMass_ev maxdeg iters = 
    [
        concat $ take iters $ enclosures
    ]
    where
    enclosures = 
        iterate (picardOp fieldSpringMass $ initValsFns) initialApprox
        where
        initValsFns =
            map (newConstFn (limitsD maxdeg) varDoms) initValuesSpringMass_ev
        initialApprox =
            map (<+>| (constructCF (-0.5) 0.5)) initValsFns
    
fnsSpringMass_uv :: Int -> Int -> [[Fn]]
fnsSpringMass_uv maxdeg iters = 
    [
        concat $ take iters $ enclosures
    ]
    where
    enclosures = 
        iterate (picardOp fieldSpringMass $ initValsFns) initialApprox
        where
        initValsFns =
            map (newConstFn (limitsD maxdeg) varDoms) initValuesSpringMass_uv
        initialApprox =
            map (<+>| (constructCF (-0.5) 0.5)) initValsFns

fnsSpringMass_uvp_mono :: Int -> Int -> [[CF.Interval Fn]]
fnsSpringMass_uvp_mono maxdeg iters = 
    [
        concat $ take iters $ enclosures
    ]
    where
    enclosures =
        zipWith (zipWith CF.Interval) (enclosuresFromInit [0,0]) (enclosuresFromInit [1,0])
        where 
        enclosuresFromInit initVals =
            iterate (picardOp fieldSpringMass $ initValsFns) initialApprox
            where
            initValsFns =
                map (newConstFn (limitsD maxdeg) varDoms) initVals
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns

fnsSpringMass_uvp :: Int -> Int -> [[Fn]]
fnsSpringMass_uvp maxdeg iters = 
    [
        map removeParams $ concat $ take iters $ enclosures
    ]
    where
    removeParams fn = 
        pEvalAtPointOutEff (partialEvaluationDefaultEffort fn) domboxP2Only fn
    enclosures = 
        iterate (picardOp fieldSpringMass $ initValsFns) initialApprox
        where
        initValsFns =
            map initValsFn paramVars2
        initValsFn paramVar =
            newProjection (limitsD maxdeg) varDomsP2 paramVar
        initialApprox =
            map (<+>| (constructCF (-0.5) 0.5)) initValsFns

picardOp ::
    ([Fn] -> [Fn]) ->
    [Fn] {-^ initial values -} -> 
    [Fn] {-^ approximates of y -} -> 
    [Fn] {-^ improved approximates of y -}
picardOp field y0 yPrev =
    zipWith picardOneFn y0 $ field yPrev
    where
    picardOneFn initValFn deriveApproxFn =
        initValFn <+> primitFn deriveApproxFn
    primitFn xd =
        primitiveFunctionOutEff effInteg xd "t"
    
    
--c1,c0,x,c01,c10,cHalf,cHalf1,cOneOver16,cSevenOver16,cOneMinusOneOver16, sampleFn :: Fn

sampleFnEndpt :: FnEndPt
sampleFnEndpt = newProjection limits varDoms "x"

sampleFn :: Fn
sampleFn = newProjection limits varDoms "x"

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

----effCF = (100, (100,())) -- MPFR
effCF :: ArithInOut.RoundedRealEffortIndicator CF
effCF = ArithInOut.roundedRealDefaultEffort (0:: CF)

effEval :: EvaluationEffortIndicator Fn
effEval = evaluationDefaultEffort sampleFn
effNumComp :: NumOrd.PartialCompareEffortIndicator Fn
effNumComp = NumOrd.pCompareDefaultEffort sampleFn
effRefComp :: RefOrd.PartialCompareEffortIndicator Fn
effRefComp = RefOrd.pCompareDefaultEffort sampleFn
minmaxInOutEff :: NumOrd.MinmaxInOutEffortIndicator Fn
minmaxInOutEff = minmaxUpDnDefaultEffortIntPolyWithBezierDegree 10 sampleFnEndpt
effDrawFn :: CairoDrawFnEffortIndicator Fn
effDrawFn = cairoDrawFnDefaultEffort sampleFn
effInteg :: IntegrationEffortIndicator Fn
effInteg = integrationDefaultEffort sampleFn
--
--
--evalOpsOutCf = evalOpsOut effCF x (0::CF)

limits :: IntPolySizeLimits CF
limits = limitsD 10
limitsD :: Int -> IntPolySizeLimits CF
limitsD maxdeg =
    IntPolySizeLimits
        {
            ipolylimits_cf_limits = (),
            ipolylimits_maxdeg = maxdeg,
            ipolylimits_maxsize = 100
        } 

dombox :: DomainBox Fn
dombox = Map.fromList varDoms
domboxP1 :: DomainBox Fn
domboxP1 = Map.fromList varDomsP1
domboxP1Only :: DomainBox Fn
domboxP1Only = Map.fromList varDomsP1Only
domboxP2 :: DomainBox Fn
domboxP2 = Map.fromList varDomsP2
domboxP2Only :: DomainBox Fn
domboxP2Only = Map.fromList varDomsP2Only

varDoms :: [(Var Fn, CF)]
varDoms = zip vars doms
varDomsP1 :: [(Var Fn, CF)]
varDomsP1 = zip (vars ++ paramVars1) (doms ++ domsP1Only)
varDomsP1Only :: [(Var Fn, CF)]
varDomsP1Only = zip paramVars1 domsP1Only
varDomsP2 :: [(Var Fn, CF)]
varDomsP2 = zip (vars ++ paramVars2) (doms ++ domsP2Only)
varDomsP2Only :: [(Var Fn, CF)]
varDomsP2Only = zip paramVars2 domsP2Only  -- initValuesSpringMass_uv

vars :: [Var Fn]
vars = [tVar]
tVar :: Var Fn
tVar = "t"

doms :: [CF]
doms = [constructCF 0 1]

paramVars1 :: [Var Fn]
paramVars1 = ["xi"]
paramVars2 :: [Var Fn]
paramVars2 = ["xi", "x'i"]

domsP1Only :: [CF]
domsP1Only = [constructCF (-1) 1] -- [[-1,1]]

domsP2Only :: [CF]
domsP2Only = [0] -- [[0,1], 0]

--segs = [CF]
--segs = 

constructCF :: Double -> Double -> CF
constructCF l r =
    RefOrd.fromEndpointsOut (cf0 <+>| l, cf0 <+>| r)
cf0 :: CF
cf0 = 0
    
