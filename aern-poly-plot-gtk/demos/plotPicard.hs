{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot ()

import qualified 
       Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import Numeric.AERN.RmToRn

--import Numeric.AERN.RealArithmetic.Basis.Double
--import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.Basics.Interval

import qualified Numeric.AERN.DoubleBasis.Interval as CF


import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators

import Numeric.AERN.RealArithmetic.ExactOps

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
    let ivpName : tEndS : maxdegS : maxsizeS : otherArgs = args 
    let tEndDbl = (read tEndS) :: Double
    let maxdeg = (read maxdegS) :: Int
    let maxsize = (read maxsizeS) :: Int
    
    -- enable multithreaded GUIs:
    _ <- Gtk.unsafeInitGUIForThreadedRTS
    
    -- compute the enclosures:
    let ((fns, result), fnmeta) = getFns ivpName tEndDbl maxdeg maxsize otherArgs
    putStrLn $ "result = " ++ show result

    -- set up the function view window:
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    _ <- FV.new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
    
    -- fire up the GUI:
    Gtk.mainGUI
    
addPlotVar :: [[Fn]] -> [[(FV.GraphOrParamPlotFn Fn, String)]]
addPlotVar fns =
    map (map addV) fns
    where
    addV fn = (FV.GraphPlotFn fn, tVar)
    
getFns :: String -> Double -> Int -> Int -> [String] ->(([[Fn]], [CF]), FV.FnMetaData Fn)
getFns name tEndDbl maxdeg maxsize otherArgs =
    case Map.lookup name functionMap of
        Just mkFns ->
            mkFns tEndDbl maxdeg maxsize otherArgs
        _ ->
            error $ "unknown IVP " ++ show name ++ ", known IVPs:\n" ++ unlines names 
    where
    functionMap = Map.fromList functions 
    names = map fst functions
    
functions :: 
    [(
       String
     ,
       Double -> Int -> Int -> [String] -> (([[Fn]], [CF]), FV.FnMetaData Fn)
     )
    ]
functions =
    [
        ("expdwindle-exact-initval-naive", functionsExpDwindle "ev"),
        ("expdwindle-exact-initval-flow", functionsExpDwindle "evp"),
        ("expdwindle-uncertain-initval-naive", functionsExpDwindle "uv"),
        ("expdwindle-uncertain-initval-flow", functionsExpDwindle "uvp"),
        ("expmirror-exact-initval-naive", functionsExpMirror "ev"),
        ("expmirror-exact-initval-flow", functionsExpMirror "evp"),
        ("expmirror-uncertain-initval-naive", functionsExpMirror "uv"),
        ("expmirror-uncertain-initval-flow", functionsExpMirror "uvp"),
        ("vanderpol-exact-initval-naive", functionsVanDerPol "ev"),
        ("vanderpol-exact-initval-flow", functionsVanDerPol "evp"),
        ("vanderpol-uncertain-initval-naive", functionsVanDerPol "uv"),
        ("vanderpol-uncertain-initval-flow", functionsVanDerPol "uvp"),
        ("springmass-exact-initval-naive", functionsSpringMass "ev"),
        ("springmass-exact-initval-flow", functionsSpringMass "evp"),
        ("springmass-uncertain-extrema-naive", functionsSpringMass "ue"),
        ("springmass-uncertain-extrema-flow", functionsSpringMass "uep"),
        ("springmass-uncertain-speed-naive", functionsSpringMass "us"),
        ("springmass-uncertain-speed-flow", functionsSpringMass "usp")
    ]


--showP p = showPoly id show p -- ++ " [" ++ show p ++ "]"


---------------------------------

functionsExpDwindle ::
    String -> Double -> Int -> Int -> [String] -> (([[Fn]], [CF]), FV.FnMetaData Fn)
functionsExpDwindle subname =
    functionsExpDwindleSub
    where
    functionsExpDwindleSub =
        case subname of
            "ev" -> fnsExp False initValuesExp_ev
            "evp" -> fnsExp True initValuesExp_ev
            "uv" -> fnsExp False initValuesExp_uv
            "uvp" -> fnsExp True initValuesExp_uv
            _ -> error "functionsExp: internal error"

    initValuesExp_ev :: [CF]
    initValuesExp_ev = [1]

    initValuesExp_uv :: [CF]
    initValuesExp_uv = [(-1) </\> 1]

    fieldExp :: [Fn] -> [Fn]
    fieldExp [y] = [(-1 :: Int) |<*> y] -- y' = -y
    fieldExp _ = error "fieldExp: internal error"

    fnsExp useFlow initValues tEndDbl maxdeg maxsize otherArgs =
        (functions2, fnmeta) 
        where
        [itersS, bisectDepthS] = otherArgs
        iters = read itersS
        bisectDepth = read bisectDepthS
        tDom = constructCF 0 tEndDbl
        tEnd = constructCF tEndDbl tEndDbl

        functions2 =
            picardOnPartitionWrapping
                useFlow
                fieldExp
                paramVars initValues
                (partitionDom tDom bisectDepth)
                (limitsDS maxdeg maxsize) iters
            where
            paramVars = ["yi"]
        
        fnmeta =
            FV.simpleFnMetaData
                sampleFn 
                (FV.Rectangle  1 (-1) 0 (tEnd)) -- initial plotting region
                Nothing
                200 -- samplesPerUnit
                tVar
                [("segment " ++ show i, functionInfos) | i <- [1..2^bisectDepth] :: [Int]]
            where
            functionInfos =
                zip3 
                    fnNames 
                    (repeat blue) --styles
                    (reverse $ True : replicate (iters - 1) False) -- show only the last iteration by default
            fnNames =
                map ("y" ++) $ map show ([1..iters] :: [Int])
    


---------------------------------
functionsExpMirror ::
    String -> Double -> Int -> Int -> [String] -> (([[Fn]], [CF]), FV.FnMetaData Fn)
functionsExpMirror subname =
    functionsExpMirrorSub
    where
    functionsExpMirrorSub =
        case subname of
            "ev" -> fnsExpMirror False initValuesExpMirror_ev
            "evp" -> fnsExpMirror True initValuesExpMirror_ev
            "uv" -> fnsExpMirror False initValuesExpMirror_uv
            "uvp" -> fnsExpMirror True initValuesExpMirror_uv
            _ -> error "functionsExp: internal error"

    initValuesExpMirror_ev :: [CF]
    initValuesExpMirror_ev = [2]

    initValuesExpMirror_uv :: [CF]
    initValuesExpMirror_uv = [1 </\> 2]

    fieldExpMirror :: Int -> [Fn] -> [Fn]
    fieldExpMirror bd [y] = 
        [negate $ 
            (ArithInOut.absOutEff (effAbs bd) (y <+>| (-1 :: Int))) 
            <+>| (1 :: Int)] -- y' = -(abs(y - 1) + 1)
    fieldExpMirror _ _ = error "fieldExpMirror: internal error"

    fnsExpMirror useFlow initValues tEndDbl maxdeg maxsize otherArgs = 
        (functions2, fnmeta)
        where
        [bezdegS, itersS, bisectDepthS] = otherArgs
        bezdeg = read bezdegS
        iters = read itersS
        bisectDepth = read bisectDepthS
        tDom = constructCF 0 tEndDbl
        tEnd = constructCF tEndDbl tEndDbl
        
        functions2 =
            picardOnPartitionWrapping
                useFlow
                (fieldExpMirror bezdeg) 
                paramVars initValues
                (partitionDom tDom bisectDepth)
                (limitsDS maxdeg maxsize) iters
            where
            paramVars = ["yi"]

        fnmeta :: FV.FnMetaData Fn
        fnmeta =
            FV.simpleFnMetaData
                sampleFn 
                (FV.Rectangle  2.05 (-0.1) (-0.05) (tEnd+0.05)) -- initial plotting region
                Nothing
                200 -- samplesPerUnit
                tVar
                [("segment " ++ show i, functionInfos) | i <- [1..2^bisectDepth] :: [Int]]
            where
            functionInfos =
                zip3 
                    fnNames
                    (repeat blue) -- styles 
                    (reverse $ True : replicate (iters - 1) False) -- show only the last iteration by default
            fnNames =
                map ("y" ++) $ map show ([1..iters] :: [Int])



---------------------------------
functionsSpringMass :: 
    String -> Double -> Int -> Int -> [String] -> (([[Fn]], [CF]), FV.FnMetaData Fn)
functionsSpringMass subname =
    functionsSpringMassSub
    where
    functionsSpringMassSub =
        case subname of
            "ev" -> fnsSpringMass False initValuesSpringMass_ev
            "evp" -> fnsSpringMass True initValuesSpringMass_ev
            "ue" -> fnsSpringMass False initValuesSpringMass_ue
            "uep" -> fnsSpringMass True initValuesSpringMass_ue
            "us" -> fnsSpringMass False initValuesSpringMass_us
            "usp" -> fnsSpringMass True initValuesSpringMass_us
            _ -> error "functionsSpringMass: internal error"
            
    initValuesSpringMass_ev :: [CF]
    initValuesSpringMass_ev = [1,0]
    
    initValuesSpringMass_ue :: [CF]
    initValuesSpringMass_ue = [0 </\> 1,0]

    initValuesSpringMass_us :: [CF]
    initValuesSpringMass_us = [0.5,0 </\> 0.25]

    fieldSpringMass [y,y'] = [y',(-1 :: Int) |<*> y]
    --fieldSpringMass [y,y'] = [y',(-4 :: Int) |<*> y]
    --fieldSpringMass [y,y'] = [y',((-4 :: Int) |<*> y) <+> (y' <*> y' </>| (4::Int))]
    fieldSpringMass _ = error "fieldSpringMass"
    
    fnsSpringMass useFlow initValues tEndDbl maxdeg maxsize otherArgs = 
        (functions2, fnmeta)
        where
        [itersS, bisectDepthS] = otherArgs
        iters = read itersS
        bisectDepth = read bisectDepthS

        functions2 =
            picardOnPartitionWrapping
                useFlow
                fieldSpringMass 
                paramVars initValues
                (partitionDom tDom bisectDepth)
                (limitsDS maxdeg maxsize) iters
            where
            paramVars = ["yi", "y'i"]

        tDom = constructCF 0 tEndDbl
        tEnd = constructCF tEndDbl tEndDbl
    
        fnmeta = 
            FV.simpleFnMetaData
                sampleFn
                (FV.Rectangle  1.125 (-1.125) (-0.5) (tEnd <+> 0.5)) -- initial plotting region
                Nothing
                200 -- samplesPerUnit
                tVar
                [("segment " ++ show i, functionInfos) | i <- [1..2^bisectDepth] :: [Int]]
            where
            functionInfos =
                zip3 
                    fnNames
--                    (concat $ repeat [blue, green]) -- styles 
                    (repeat black) -- styles 
                    (reverse $ True : True : replicate (2*iters - 2) False) -- show only the last iteration by default
            fnNames =
                concat $ map (\nS -> ["y" ++ nS, "y'" ++ nS]) $ map show ([1..iters] :: [Int])

---------------------------------
functionsVanDerPol :: 
    String -> Double -> Int -> Int -> [String] -> (([[Fn]], [CF]), FV.FnMetaData Fn)
functionsVanDerPol subname =
    functionsVanDerPolSub
    where
    functionsVanDerPolSub =
        case subname of
            "ev" -> fnsVanDerPol False initValuesVanDerPol_ev
            "evp" -> fnsVanDerPol True initValuesVanDerPol_ev
            "uv" -> fnsVanDerPol False initValuesVanDerPol_uv
            "uvp" -> fnsVanDerPol True initValuesVanDerPol_uv
            _ -> error "functionsVanDerPol: internal error"

    initValuesVanDerPol_ev :: [CF]
    initValuesVanDerPol_ev = [1,1]
    
    initValuesVanDerPol_uv :: [CF]
    initValuesVanDerPol_uv = [1 <+> pmDelta,1]
    pmDelta = constructCF (-delta) delta
    delta = 0.125
    
    fieldVanDerPol [x,y] = [y,((mu |<*> y) <*> ((1::Double) |<+> (neg x <*> x))) <+> (neg x)]
    fieldVanDerPol _ = error "fieldVanDerPol"
    mu = 1 :: Double
    
    fnsVanDerPol useFlow initValues tEndDbl maxdeg maxsize otherArgs =
        (functions2, fnmeta)
        where
        [itersS, bisectDepthS] = otherArgs
        iters = read itersS
        bisectDepth = read bisectDepthS

        functions2 = 
            picardOnPartitionWrapping
                useFlow
                fieldVanDerPol 
                paramVars initValues
                (partitionDom tDom bisectDepth)
                (limitsDS maxdeg maxsize) iters
            where
            paramVars = ["xi", "yi"]

        tDom = constructCF 0 tEndDbl
        tEnd = constructCF tEndDbl tEndDbl
    
        fnmeta = 
            FV.simpleFnMetaData
                sampleFn 
                (FV.Rectangle  2 (-2) 0 tEnd) -- initial plotting region
                Nothing
                200 -- samplesPerUnit
                tVar
                [("segment " ++ show i, functionInfos) | i <- [1..2^bisectDepth] :: [Int]]
            where
            functionInfos =
                zip3 
                    fnNames
                    (concat $ repeat [blue, green]) -- styles 
                    (reverse $ True : True : replicate (2*iters - 2) False) -- show only the last iteration by default
            fnNames =
                concat $ map (\nS -> ["y" ++ nS, "y'" ++ nS]) $ map show ([1..iters] :: [Int])

    
    

{-|
    Apply the interval Picard operator solver on a partition of time, step by step.
    The initial values for each segment are computed by box-wrapping
    the enclosures on the previous segment and taking their intersection.
-}
picardOnPartitionWrapping :: 
    Bool -- ^ whether to parametrise the solving over uncertainty in initial values 
    -> ([Fn] -> [Fn]) -- ^ field
    -> [Var Fn] -- ^ variables
    -> [CF] -- ^ initial values
    -> [CF] -- ^ partition of time
    -> IntPolySizeLimits CF -- ^ limits on polynomials
    -> Int -- ^ number of iterations of the Picard operator
    -> ([[Fn]], [CF])
picardOnPartitionWrapping
        useFlow
        field 
        paramVars initValues 
        partition 
        limits iters 
    =
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
            varDomsTEndParams 
                | useFlow = (tVar, segmentR) : zip paramVars segmentInitValues
                | otherwise = [(tVar, segmentR)]
            (_, segmentR) = RefOrd.getEndpointsOut segment
            intersectBoxes boxes =
                foldl1 (zipWith (RefOrd.<\/>)) boxes 
        enclosures =
            take iters $  
            iterate (picardOp field initValsFns) initialApprox
            where
            initialApprox =
                map (<+>| (constructCF (-0.5) 0.5)) initValsFns
            initValsFns
                | useFlow
                    = map initValProjection paramVars
                | otherwise
                    = map initValConstant segmentInitValues
                where
                initValProjection paramVar = 
                    newProjection limits varDomsTDomParam paramVar
                initValConstant initVal =
                    newConstFn limits [(tVar, segment)] initVal
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
    (defaultIntPolySizeLimits 0 () 1)
    {
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

    
