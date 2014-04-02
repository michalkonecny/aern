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
    let ivpName : maxdegS : maxsizeS : otherArgs = args 
    let maxdeg = (read maxdegS) :: Int
    let maxsize = (read maxsizeS) :: Int
    
    -- enable multithreaded GUIs:
    _ <- Gtk.unsafeInitGUIForThreadedRTS
    
    -- set up the function view window:
    let (fns, fnmeta) = getFns ivpName maxdeg maxsize otherArgs
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
    
getFns :: String -> Int -> Int -> [String] ->([[Fn]], FV.FnMetaData Fn)
getFns name maxdeg maxsize otherArgs =
    case Map.lookup name functionMap of
        Just (mkFns, mkFnMeta) ->
            (mkFns maxdeg maxsize otherArgs, mkFnMeta otherArgs)
        _ ->
            error $ "unknown IVP " ++ show name ++ ", known IVPs:\n" ++ unlines names 
    where
    functionMap = Map.fromList functions 
    names = map fst functions
    
functions :: 
    [(
       String
     ,
      (Int -> Int -> [String] -> [[Fn]], [String] -> FV.FnMetaData Fn)
     )
    ]
functions =
    [
        ("expdwindle-exact-initval", functionsExpDwindle "ev"),
        ("expdwindle-exact-initval-flow", functionsExpDwindle "evp"),
        ("expdwindle-exact-initval-piecewise", functionsExpDwindle "ev-pw"),
        ("expdwindle-uncertain-initval-naive", functionsExpDwindle "uv"),
        ("expdwindle-uncertain-initval-flow", functionsExpDwindle "uvp"),
        ("expmirror-exact-initval", functionsExpMirror "ev"),
        ("springmass-exact-initval", functionsSpringMass "ev"),
        ("springmass-uncertain-initval", functionsSpringMass "uv")
    ]


--showP p = showPoly id show p -- ++ " [" ++ show p ++ "]"


---------------------------------

functionsExpDwindle ::
    String -> (Int -> Int -> [String] -> [[Fn]], [String] -> FV.FnMetaData Fn)
functionsExpDwindle subname =
    case subname of
        "ev" -> (fnsExp_ev, fnmetaExp)
        "evp" -> (fnsExp_evp, fnmetaExp)
        "ev-pw" -> (fnsExp_ev_pw, fnmetaExp_pw)
        "uv" -> (fnsExp_uv, fnmetaExp)
        "uvp" -> (fnsExp_uvp, fnmetaExp)
        _ -> error "functionsExp: internal error"
    where
    fnsExp_ev :: Int -> Int -> [String] -> [[Fn]]
    fnsExp_ev maxdeg maxsize otherArgs = 
        [
            concat $ take iters $ enclosures
        ]
        where
        [itersS] = otherArgs
        iters = read itersS
        enclosures = 
            iterate (picardOp fieldExp initValsFns) initialApprox
            where
            initValsFns =
                map (newConstFn (limitsDS maxdeg maxsize) tVarDoms) initValuesExp_ev
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns
    
    fnsExp_uv :: Int -> Int -> [String] -> [[Fn]]
    fnsExp_uv maxdeg maxsize otherArgs = 
        [
            concat $ take iters $ enclosures
        ]
        where
        [itersS] = otherArgs
        iters = read itersS
        enclosures = 
            iterate (picardOp fieldExp initValsFns) initialApprox
            where
            initValsFns =
                map (newConstFn (limitsDS maxdeg maxsize) tVarDoms) initValuesExp_uv
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns
    
    fnsExp_evp :: Int -> Int -> [String] -> [[Fn]]
    fnsExp_evp maxdeg maxsize otherArgs =
        picardOnPartitionWrapping 
            fieldExp
            paramVars initValuesExp_ev 
            [tDom] 
            (limitsDS maxdeg maxsize) iters
        where
        [itersS] = otherArgs
        iters = read itersS
        
    fnsExp_ev_pw :: Int -> Int -> [String] -> [[Fn]]
    fnsExp_ev_pw maxdeg maxsize otherArgs =
        picardOnPartitionWrapping 
            fieldExp
            paramVars initValuesExp_ev 
            (partitionDom tDom bisectDepth)
            (limitsDS maxdeg maxsize) iters
        where
        [itersS, bisectDepthS] = otherArgs
        iters = read itersS
        bisectDepth = read bisectDepthS
        
        
    fnsExp_uvp :: Int -> Int -> [String] -> [[Fn]]
    fnsExp_uvp maxdeg maxsize otherArgs =
        picardOnPartitionWrapping 
            fieldExp
            paramVars initValuesExp_uv 
            [tDom] 
            (limitsDS maxdeg maxsize) iters
        where
        [itersS] = otherArgs
        iters = read itersS
        
    fieldExp :: [Fn] -> [Fn]
    fieldExp [y] = [(-1 :: Int) |<*> y] -- y' = -y
    fieldExp _ = error "fieldExp: internal error"

    initValuesExp_ev :: [CF]
    initValuesExp_ev = [1]

    initValuesExp_uv :: [CF]
    initValuesExp_uv = [(-1) </\> 1]

    paramVars :: [Var Fn]
    paramVars = ["yi"]

    tDom :: CF
    tDom = constructCF 0 1
    
    tVarDoms :: [(Var Fn, CF)]
    tVarDoms = [(tVar, tDom)]
    
    fnmetaExp :: [String] -> FV.FnMetaData Fn
    fnmetaExp otherArgs =
        FV.simpleFnMetaData
            sampleFn 
            (FV.Rectangle  1 (-1) 0 (1)) -- initial plotting region
            200 -- samplesPerUnit
            [("segment 1", functionInfos)]
        where
        [itersS] = otherArgs
        iters = read itersS
        functionInfos =
            zip3 
                fnNames 
                (repeat blue) --styles
                (True : repeat False) -- initial activations
        fnNames =
            map ("y" ++) $ map show ([1..iters] :: [Int])

    fnmetaExp_pw :: [String] -> FV.FnMetaData Fn
    fnmetaExp_pw otherArgs =
        FV.simpleFnMetaData
            sampleFn 
            (FV.Rectangle  1 (-1) 0 (1)) -- initial plotting region
            200 -- samplesPerUnit
            [("segment " ++ show i, functionInfos) | i <- [1..2^bisectDepth] :: [Int]]
        where
        [itersS, bisectDepthS] = otherArgs
        iters = read itersS
        bisectDepth :: Int
        bisectDepth = read bisectDepthS
        functionInfos =
            zip3 
                fnNames 
                (repeat blue) --styles
                (True : repeat False) -- initial activations
        fnNames =
            map ("y" ++) $ map show ([1..iters] :: [Int])


picardOnPartitionWrapping :: 
    ([Fn] -> [Fn]) -- ^ field
    -> [Var Fn] -- ^ variables
    -> [CF] -- ^ initial values
    -> [CF] -- ^ partition of time
    -> IntPolySizeLimits CF -- ^ limits on polynomials
    -> Int -- ^ number of iterations of the Picard operator
    -> [[Fn]]
picardOnPartitionWrapping 
        field 
        paramVars initValues 
        partition 
        limits iters 
    =
    fst $
    foldl picardOnSegment ([],initValues) partition
    where
    picardOnSegment (resultsForPrevSegments, segmentInitValues) segment =
        (resultsForPrevSegments ++ [concat enclosures], nextSegmentInitValues)
        where
        nextSegmentInitValues =
            intersectBoxes $ 
                map (map evaluateAtEndpoint) $ 
                    pickOnlySurelyValidEnclosures enclosures
            where
            pickOnlySurelyValidEnclosures enclosures2@(y1 : y2 : rest) =
                if y2InsideY1
                    then enclosures2 -- all enclosures are valid
                    else pickOnlySurelyValidEnclosures (y2 : rest) -- drop y1 and try again
                where
                y2InsideY1 =
                    and $ map (== (Just True)) comparisonResults
                comparisonResults =
                    zipWith (RefOrd.pLeqEff effRefComp) y1 y2  -- is y1 below y2, ie does y1 enclose y2?
            pickOnlySurelyValidEnclosures _ =
                error "Failed to find a certain enclosure, try increasing the number of iterations."
            evaluateAtEndpoint fn =
                evalAtPointOutEff effEval (Map.fromList varDomsTEndParams) fn
            varDomsTEndParams =
                (tVar, segmentR) : zip paramVars segmentInitValues
            (_, segmentR) = RefOrd.getEndpointsOut segment
            intersectBoxes boxes =
                unsafePrint ("boxes =\n" ++ (unlines $ map show boxes)) $
                foldl1 (zipWith (RefOrd.<\/>)) boxes 
        enclosures =
            take iters $  
            iterate (picardOp field initValsFns) initialApprox
            where
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns
            initValsFns =
                map initValsFn paramVars
            initValsFn paramVar =
                newProjection limits varDomsTDomParam paramVar
            varDomsTDomParam =
                (tVar, segment) : zip paramVars segmentInitValues

partitionDom :: CF -> Int -> [CF]
partitionDom tDom bisectDepth =
    (iterate bisectAll [tDom]) !! bisectDepth
    where
    bisectAll = concat . map bisectIntoList
    bisectIntoList a = [aL, aR]
        where
        (aL, aR) = CF.bisect Nothing a 


functionsExpMirror ::
    String -> (Int -> Int -> [String] -> [[Fn]], [String] -> FV.FnMetaData Fn)
functionsExpMirror subname =
    case subname of
        "ev" -> (fnsExpMirror_ev, fnmetaExpMirror)
        "ev-pw" -> (fnsExpMirror_ev_pw, fnmetaExpMirror_pw)
--        "uv" -> (fnsExp_uv, fnmetaExp)
--        "uvp" -> (fnsExp_uvp, fnmetaExp)
        _ -> error "functionsExp: internal error"
    where
    fnsExpMirror_ev :: Int -> Int -> [String] -> [[Fn]]
    fnsExpMirror_ev maxdeg maxsize otherArgs = 
        [
            concat $ take iters $ enclosures
        ]
        where
        [bezdegS, itersS] = otherArgs
        iters = read itersS
        bezdeg = read bezdegS
        enclosures = 
            iterate (picardOp (fieldExpMirror bezdeg) initValsFns) initialApprox
            where
            initValsFns =
                map (newConstFn (limitsDS maxdeg maxsize) tVarDoms) initValuesExpMirror_ev
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns
    
    fnsExpMirror_ev_pw :: Int -> Int -> [String] -> [[Fn]]
    fnsExpMirror_ev_pw maxdeg maxsize otherArgs = 
        [
            concat $ take iters $ enclosures -- TODO
        ]
        where
        [bezdegS, itersS, bisectDepthS] = otherArgs
        iters = read itersS
        bezdeg = read bezdegS
--        bisectDepth = read bisectDepthS

        enclosures = 
            iterate (picardOp (fieldExpMirror bezdeg) initValsFns) initialApprox
            where
            initValsFns =
                map (newConstFn (limitsDS maxdeg maxsize) tVarDoms) initValuesExpMirror_ev
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns
    
--    fnsExp_uv :: Int -> Int -> Int -> Int -> [[Fn]]
--    fnsExp_uv maxdeg maxsize bezdeg iters = 
--        [
--            concat $ take iters $ enclosures
--        ]
--        where
--        enclosures = 
--            iterate (picardOp fieldExp initValsFns) initialApprox
--            where
--            initValsFns =
--                map (newConstFn (limitsDS maxdeg maxsize) tVarDoms) initValuesExp_uv
--            initialApprox =
--                map (<+>| (constructCF (-0.5) 0.5)) initValsFns
--    
--    fnsExp_uvp :: Int -> Int -> Int -> Int -> [[Fn]]
--    fnsExp_uvp maxdeg maxsize bezdeg iters =
--        [
--            resultWithParams
--        ]
--        where
--        resultWithParams = concat $ take iters $ enclosures
--        enclosures = 
--            iterate (picardOp fieldExp initValsFns) initialApprox
--            where
--            initValsFns =
--                map initValsFn paramVars1
--            initValsFn paramVar =
--                newProjection (limitsDS maxdeg maxsize) varDomsP1 paramVar
--            initialApprox =
--                map (<+>| (constructCF (-0.5) 0.5)) initValsFns

    fieldExpMirror :: Int -> [Fn] -> [Fn]
    fieldExpMirror bd [y] = 
        [negate $ 
            (ArithInOut.absOutEff (effAbs bd) (y <+>| (-1 :: Int))) 
            <+>| (1 :: Int)] -- y' = -(abs(y - 1) + 1)
    fieldExpMirror _ _ = error "fieldExpMirror: internal error"

--    initValuesExpMirror_uv :: [CF]
--    initValuesExpMirror_uv = [(-1) </\> 1]

    initValuesExpMirror_ev :: [CF]
    initValuesExpMirror_ev = [2]

--    varDomsParam :: [(Var Fn, CF)]
--    varDomsParam = tVarDoms ++ zip paramVars initValuesExpMirror_uv
--    
--    paramVars :: [Var Fn]
--    paramVars = ["yi"]

    tDom :: CF
    tDom = constructCF 0 tEndDbl
    
    tVarDoms :: [(Var Fn, CF)]
    tVarDoms = [(tVar, tDom)]
    
    tEndDbl :: Double
    tEndDbl = 1
    tEnd :: CF
    tEnd = constructCF tEndDbl tEndDbl
    
    fnmetaExpMirror :: [String] -> FV.FnMetaData Fn
    fnmetaExpMirror otherArgs =
        FV.simpleFnMetaData
            sampleFn 
            (FV.Rectangle  3 (-1) 0 (tEnd)) -- initial plotting region
            200 -- samplesPerUnit
            [("segment 1", functionInfos)]
        where
        [_bezdegS, itersS] = otherArgs
        iters = read itersS
        functionInfos =
            zip3 
                fnNames
                (repeat blue) -- styles 
                (True : repeat False) -- initial activations  
        fnNames =
            map ("y" ++) $ map show ([1..iters] :: [Int])

    fnmetaExpMirror_pw :: [String] -> FV.FnMetaData Fn
    fnmetaExpMirror_pw otherArgs =
        FV.simpleFnMetaData
            sampleFn 
            (FV.Rectangle  3 (-1) 0 (tEnd)) -- initial plotting region
            200 -- samplesPerUnit
            segments 
        where
        [_bezdegS, itersS, bisectDepthS] = otherArgs
        iters = read itersS
--        bisectDepth = read bisectDepthS
        segments =
            [("segment 1", functionInfos)] -- TODO
        functionInfos =
            zip3 
                fnNames
                (repeat blue) -- styles 
                (True : repeat False) -- initial activations  
        fnNames =
            map ("y" ++) $ map show ([1..iters] :: [Int])

---------------------------------
functionsSpringMass :: 
    String -> (Int -> Int -> [String] -> [[Fn]], [String] -> FV.FnMetaData Fn)
functionsSpringMass subname =
    case subname of
        "ev" -> (fnsSpringMass_ev, fnmetaSpringMass)
        "uv" -> (fnsSpringMass_uv, fnmetaSpringMass)
        "uvp" -> (fnsSpringMass_uvp, fnmetaSpringMass)
        _ -> error "functionsSpringMass: internal error"
    where
    fnsSpringMass_ev :: Int -> Int -> [String] -> [[Fn]]
    fnsSpringMass_ev maxdeg maxsize otherArgs = 
        [
            concat $ take iters $ enclosures
        ]
        where
        [itersS] = otherArgs
        iters = read itersS
        enclosures = 
            iterate (picardOp fieldSpringMass initValsFns) initialApprox
            where
            initValsFns =
                map (newConstFn (limitsDS maxdeg maxsize) tVarDoms) initValuesSpringMass_ev
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns
        
    fnsSpringMass_uv :: Int -> Int -> [String] -> [[Fn]]
    fnsSpringMass_uv maxdeg maxsize otherArgs = 
        [
            concat $ take iters $ enclosures
        ]
        where
        [itersS] = otherArgs
        iters = read itersS
        enclosures = 
            iterate (picardOp fieldSpringMass initValsFns) initialApprox
            where
            initValsFns =
                map (newConstFn (limitsDS maxdeg maxsize) tVarDoms) initValuesSpringMass_uv
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns
    
    fnsSpringMass_uvp :: Int -> Int -> [String] -> [[Fn]]
    fnsSpringMass_uvp maxdeg maxsize otherArgs = 
        [
            map removeParams $ concat $ take iters $ enclosures
        ]
        where
        [itersS] = otherArgs
        iters = read itersS
        removeParams fn = 
            pEvalAtPointOutEff (partialEvaluationDefaultEffort fn) domboxP2Only fn
        enclosures = 
            iterate (picardOp fieldSpringMass initValsFns) initialApprox
            where
            initValsFns =
                map initValsFn paramVars2
            initValsFn paramVar =
                newProjection (limitsDS maxdeg maxsize) varDomsP2 paramVar
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns

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
    
    domboxP2Only :: DomainBox Fn
    domboxP2Only = Map.fromList varDomsP2Only
    
    varDomsP2 :: [(Var Fn, CF)]
    varDomsP2 = zip (tVar : paramVars2) (tDom : domsP2Only)
    varDomsP2Only :: [(Var Fn, CF)]
    varDomsP2Only = zip paramVars2 domsP2Only  -- initValuesSpringMass_uv
    
    paramVars2 :: [Var Fn]
    paramVars2 = ["xi", "x'i"]
    
    domsP2Only :: [CF]
    domsP2Only = [0] -- [[0,1], 0]

    tDom :: CF
    tDom = constructCF 0 1
    
    tVarDoms :: [(Var Fn, CF)]
    tVarDoms = [(tVar, tDom)]
    

    fnmetaSpringMass :: 
        Num (Domain f) =>
        [String] -> FV.FnMetaData f
    fnmetaSpringMass otherArgs = 
        FV.simpleFnMetaData
            sampleFn 
            (FV.Rectangle  2 (-1) 0 1) -- initial plotting region
            200 -- samplesPerUnit
            segments 
        where
        [itersS] = otherArgs
        iters = read itersS
        segments =
            [("segment 1", functionInfos)]
        functionInfos =
            zip3 
                fnNames
                (concat $ repeat [blue, green]) -- styles 
                (True : True : repeat False) -- initial activations  
        fnNames =
            concat $ map (\nS -> ["y" ++ nS, "y'" ++ nS]) $ map show ([1..iters] :: [Int])


    
--    fnsSpringMass_uvp_mono :: Int -> Int -> [[CF.Interval Fn]]
--    fnsSpringMass_uvp_mono maxdeg maxsize bezdeg iters = 
--        [
--            concat $ take iters $ enclosures
--        ]
--        where
--        enclosures =
--            zipWith (zipWith CF.Interval) (enclosuresFromInit [0,0]) (enclosuresFromInit [1,0])
--            where 
--            enclosuresFromInit initVals =
--                iterate (picardOp fieldSpringMass $ initValsFns) initialApprox
--                where
--                initValsFns =
--                    map (newConstFn (limitsDS maxdeg maxsize) varDoms) initVals
--                initialApprox =
--                    map (<+>| (constructCF (-0.5) 0.5)) initValsFns
    

picardOp ::
    ([Fn] -> [Fn]) ->
    [Fn] {-^ initial values -} -> 
    [Fn] {-^ approximates of y -} -> 
    [Fn] {-^ improved approximates of y -}
picardOp field y0 yPrev =
    zipWith picardOneFn y0 $ field yPrev
    where
    picardOneFn y0_i xd_i =
        y0_i <+> primitFn xd_i
    primitFn xd =
        primitiveFunctionOutEff effInteg xd tVar
    
tVar :: Var Fn
tVar = "t"

    


constructCF :: Double -> Double -> CF
constructCF l r =
    RefOrd.fromEndpointsOut (cf0 <+>| l, cf0 <+>| r)
cf0 :: CF
cf0 = 0
    
{--- efforts for various AP functions ---}

limitsDefault :: IntPolySizeLimits CF
limitsDefault = limitsD 10
limitsD :: Int -> IntPolySizeLimits CF
limitsD maxdeg = limitsDS maxdeg 100
limitsDS :: Int -> Int -> IntPolySizeLimits CF
limitsDS maxdeg maxsize =
    IntPolySizeLimits
        {
            ipolylimits_cf_limits = (),
            ipolylimits_maxdeg = maxdeg,
            ipolylimits_maxsize = maxsize
        } 

----effCF = (100, (100,())) -- MPFR
effCF :: ArithInOut.RoundedRealEffortIndicator CF
effCF = ArithInOut.roundedRealDefaultEffort (0:: CF)

effEval :: EvaluationEffortIndicator Fn
effEval = evaluationDefaultEffort sampleFn
effNumComp :: NumOrd.PartialCompareEffortIndicator Fn
effNumComp = NumOrd.pCompareDefaultEffort sampleFn
effRefComp :: RefOrd.PartialCompareEffortIndicator Fn
effRefComp = RefOrd.pCompareDefaultEffort sampleFn
minmaxInOutEff :: Int -> NumOrd.MinmaxInOutEffortIndicator Fn
minmaxInOutEff bd = minmaxUpDnDefaultEffortIntPolyWithBezierDegree bd sampleFnEndpt
effAbs :: Int -> ArithInOut.AbsEffortIndicator Fn
effAbs bd = (effNumComp, minmaxInOutEff bd)
effDrawFn :: CairoDrawFnEffortIndicator Fn
effDrawFn = cairoDrawFnDefaultEffort sampleFn
effInteg :: IntegrationEffortIndicator Fn
effInteg = integrationDefaultEffort sampleFn
--
--
--evalOpsOutCf = evalOpsOut effCF x (0::CF)

sampleFnEndpt :: FnEndPt
sampleFnEndpt = newProjection limitsDefault [("x",1)] "x"

sampleFn :: Fn
sampleFn = newProjection limitsDefault  [("x",1)] "x"

{----- colours ---}

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

    
