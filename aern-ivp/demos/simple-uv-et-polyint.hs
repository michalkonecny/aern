{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.AERN.IVP.Examples.ODE.Simple

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Bisection
import Numeric.AERN.IVP.Solver.Picard.UncertainValue

import Numeric.AERN.IVP.Solver.ShrinkWrap -- only for testing

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration
import Numeric.AERN.RmToRn.Differentiation

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot ()

import Numeric.AERN.RealArithmetic.Basis.Double ()
import qualified Numeric.AERN.DoubleBasis.Interval as CF
--import Numeric.AERN.RealArithmetic.Basis.MPFR
--import qualified Numeric.AERN.MPFRBasis.Interval as MI

import Numeric.AERN.Basics.Interval
import Numeric.AERN.RealArithmetic.Interval ()

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
--import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort

--import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.ShowInternals

--import Data.List (intercalate)

import System.IO
import System.Environment
import System.Directory
import System.CPUTime

import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable

import qualified Graphics.UI.Gtk as Gtk
--import qualified Control.Concurrent as Concurrent
import Control.Concurrent.STM

import Numeric.AERN.Misc.Debug (unsafePrint)
_ = unsafePrint -- stop the unused warning

--import qualified Data.Map as Map
--import qualified Data.List as List

type CF = CF.DI
type FnEndpt = IntPoly String CF
type Fn = Interval FnEndpt

sampleCf :: CF
sampleCf = 0

sampleFn :: Fn
sampleFn = makeSampleWithVarsDoms 10 10 ["x"] [sampleCf]

main :: IO ()
main =
    do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    runWithArgs args
        
usage :: IO ()
usage =
    do
    putStrLn "Usage A: simple-uv-et-polyint <ivp name> <output file name> <True|False-should wrap?>"
    putStrLn "Usage B: simple-uv-et-polyint <ivp name> <True|False-plot enclosures?> <True|False-should wrap?> <maxDeg> <minStepSize> <maxStepSize> [<tEnd>]"

runWithArgs [ivpName, shouldPlotStepsS, shouldWrapS, maxDegS, maxDepthS, minDepthS] =
    runWithArgs [ivpName, shouldPlotStepsS, shouldWrapS, maxDegS, maxDepthS, minDepthS, "default"]
runWithArgs [ivpName, shouldPlotStepsS, shouldWrapS, maxDegS, maxDepthS, minDepthS, tEndS] =
    do
    _ <- solveVtPrintSteps shouldWrap shouldPlotSteps ivpTEnd (maxDeg, maxDepth, minDepth)
    return ()
    where
    ivpTEnd =
        case tEndS of
            "default" -> ivp
            _ -> ivp
                {
                    odeivp_tEnd = dblToDom sampleCf $ read tEndS
                }
    ivp = ivpByNameReportError ivpName sampleFn
    shouldWrap = read shouldWrapS :: Bool
    shouldPlotSteps = read shouldPlotStepsS :: Bool
    maxDeg = read maxDegS :: Int
    maxDepth = read maxDepthS :: Int
    minDepth = read minDepthS :: Int
runWithArgs [ivpName, outputFileName, shouldWrapS] =
    writeCSV ivp outputFileName shouldWrap
    where
    shouldWrap = read shouldWrapS :: Bool
    ivp = ivpByNameReportError ivpName sampleFn
runWithArgs _ = usage
    
writeCSV :: ODEIVP Fn -> FilePath -> Bool -> IO ()
writeCSV ivp outputFileName shouldWrap =
    do
    isClash <- doesFileExist outputFileName
    case isClash of
        True -> putStrLn $ "file " ++ outputFileName ++ " exists"
        False ->
            withFile outputFileName WriteMode $ \ handle ->
                do
                hSetBuffering handle LineBuffering
                writeCSVheader handle
                mapM_ (runSolverMeasureTimeMSwriteLine handle) paramCombinations
    where
    paramCombinations = 
        [(maxDegree, depth) | 
            maxDegree <- [0..15], depth <- [0..10]]
--            maxDegree <- [0..10], depth <- [0..5]]
    writeCSVheader handle =
        do
        hPutStrLn handle $ "ivp: " ++ description
--        hPutStrLn handle $ "polynomial degree, min step size (2^(-n)), time (microseconds), error upper bound at t=1, error at t = 1"
        hPutStrLn handle $ "polynomial degree, min step size (2^(-n)), time (microseconds), error at t = 1"
    runSolverMeasureTimeMSwriteLine handle (maxDegree, maxDepth) =
        do
        resultsAndTimes <- mapM solveAndMeasure ([1..1] :: [Int])
        let ((result, _) : _)  = resultsAndTimes
        let averageTime = average $ map snd resultsAndTimes
        hPutStrLn handle $ makeCSVLine ((result, averageTime), (maxDegree, maxDepth))
        where
        average list = (2 * (sum list) + n) `div` (2 * n)
            where
            n = fromIntegral $ length list
        solveAndMeasure _ =
            do
            starttime <- getCPUTime
            solverResult <- solveVtPrintSteps shouldWrap False ivp (maxDegree, maxDepth, 0)
            endtime <- getCPUTime
            return $ (solverResult, (endtime - starttime) `div` 1000000000)
        
    description = odeivp_description ivp
    maybeVecExact = odeivp_maybeExactValuesAtTEnd ivp
    makeCSVLine (((maybeVec, _), execTimeMS), (maxDegree, depth)) =
        show maxDegree ++ "," 
        ++ show depth ++ ","
        ++ show execTimeMS ++ ","
--        ++ enclosureErrorBoundS ++ ","
        ++ enclosureErrorS
        where
        enclosureErrorS =
            case maybeVec of
                Nothing -> show "no solution"
                Just vecOut ->
                        case maybeVecExact of
                            Just vecExact 
                                | not (refinesVec vecExact vecOut) -> --  && refinesVec vecIn vecExact) ->
                                    error $ 
                                        "enclosure error:"
                                        ++ "\n vecOut = " ++ show vecOut
                                        ++ "\n vecExact = " ++ show vecExact
--                                        ++ "\n vecIn = " ++ show vecIn
                                | otherwise -> 
                                    computeMaxDiff vecOut vecExact
                            _ -> show "exact solution not known"
                where
                computeMaxDiff vecOut vecOther = 
                    removeBracks $
                    show $ 
                        snd $ RefOrd.getEndpointsOutWithDefaultEffort $ 
                            foldl1 max $ zipWith (CF.<->) (map CF.width vecOut) (map CF.width vecOther)
        removeBracks ('<': rest1 ) =
            reverse $ removeR $ reverse rest1
            where
            removeR ('>' : rest2 ) = rest2

refinesVec :: [CF] -> [CF] -> Bool
refinesVec vec1 vec2 =
    and $ zipWith refines vec1 vec2
refines :: CF -> CF -> Bool
refines a1 a2 = (a2 CF.|<=? a1) == Just True
   
solveVtPrintSteps ::
    (solvingInfo ~ (Maybe ([Fn],[Fn]), (CF, Maybe [CF])))
    => 
    Bool
    ->
    Bool
    ->
    ODEIVP Fn 
    -> 
    (Int, Int, Int) 
    -> 
    IO 
    (
        Maybe [CF]
    ,
        (
            BisectionInfo solvingInfo (solvingInfo, Maybe CF)
        )
    )
solveVtPrintSteps shouldWrap shouldPlotSteps ivp (maxdegParam, minDepthParam, maxDepthParam) =
    do
    putStrLn "--------------------------------------------------"
    putStrLn "demo of solve-Vt from (Konecny, Taha, Duracz 2012)"
    putStrLn "--------------------------------------------------"
    putStrLn $ "solving: " ++ description
    putStrLn "----------  parameters: -------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "m = " ++ show m
    putStrLn $ "substSplitSizeLimit = " ++ show substSplitSizeLimit
    putStrLn $ "minimum step size = 2^{" ++ show minStepSizeExp ++ "}"
    putStrLn $ "maximum step size = 2^{" ++ show maxStepSizeExp ++ "}"
    putStrLn $ "split improvement threshold = " ++ show splitImprovementThreshold
    case maybeExactResult of
        Just exactResult ->
            putStr $ showSegInfo "(almost) exact result = " (Nothing, (tEnd, Just exactResult))
        _ -> return ()
    putStrLn "----------  steps: ---------------------------"
    _ <- printStepsInfo (1:: Int) bisectionInfoOut
    putStrLn "----------  step summary: -----------------------"
    putStrLn $ "number of atomic segments = " ++ (show $ bisectionInfoCountLeafs bisectionInfoOut)
    putStrLn $ "smallest segment size: " ++ (show smallestSegSize)  
    putStrLn "----------  result: -----------------------------"
    putStr $ showSegInfo ">>> " (Nothing, (tEnd, endValues))
    case shouldShowSteps of
        True ->
            do
            putStrLn "----------  splitting info: -----------------"
            putStrLn $ showBisectionInfo showSegInfo showSplitReason "" bisectionInfoOut
        False -> return ()
    putStrLn "-------------------------------------------------"
    case shouldPlotSteps of
        False -> return ()
        True ->
            do
--            _ <- Concurrent.forkIO $ 
            plotEnclosures effCf (2^^(-8 :: Int) :: CF) "t" componentNames bisectionInfoOut
            return ()
    return (endValues, bisectionInfoOut)
    where
    shouldShowSteps = False
    
    -- solver call:
    (endValues, bisectionInfoOut) =
        solveIVPWithUncertainValue shouldWrap
            sizeLimits substSplitSizeLimit delta m 
                minStepSize maxStepSize splitImprovementThreshold
                    ivp
    -- parameters:
    delta = 1
    maxdeg = maxdegParam
    maxsize = 1000
    m = 100
    substSplitSizeLimit = 100
    minStepSizeExp = - minDepthParam
    maxStepSizeExp = - maxDepthParam
    minStepSize = 2^^minStepSizeExp
    maxStepSize = 2^^maxStepSizeExp
    splitImprovementThreshold = 2^^(-50 :: Int)

    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
    
    -- auxiliary:
    description = odeivp_description ivp
    tStart = odeivp_tStart ivp
    tEnd = odeivp_tEnd ivp
    maybeExactResult = odeivp_maybeExactValuesAtTEnd ivp
    componentNames = odeivp_componentNames ivp
    
    sizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms maxdeg maxsize [] []
            
    (smallestSegSize, _) =
        aux tStart (tEnd CF.<-> tStart) bisectionInfoOut
        where
        aux tPrev tSmallestSoFar (BisectionNoSplit (_,(tNow,_))) =
            (CF.minOut tSmallestSoFar (tNow CF.<-> tPrev), tNow)
        aux tPrev tSmallestSoFar (BisectionSplit _ left Nothing) =
            aux tPrev tSmallestSoFar left
        aux tPrev tSmallestSoFar (BisectionSplit _ left (Just right)) =
            aux tPrevL tSmallestSoFarL right
            where
            (tSmallestSoFarL, tPrevL) =
                aux tPrev tSmallestSoFar left
    
    showStepInfo (n, segInfo@(_, (t, _))) =
        "step " ++ show n ++ ": t = " ++ show t ++ "\n" ++ (showSegInfo "    " segInfo)
    printStepsInfo n (BisectionNoSplit segInfo) =
        do
        putStrLn $ showStepInfo (n, segInfo)
        return $ n + 1
    printStepsInfo n (BisectionSplit _ left maybeRight) =
        do
        n2 <- printStepsInfo n left
        case maybeRight of
            Just right -> printStepsInfo n2 right
            Nothing -> return $ n2 + 1
    
    showSegInfo indent (_, (t, maybeValues)) =
        unlines $ map showComponent $ zip componentNames valueSs
        where
        showComponent (name, valueS) =
            indent ++ name ++ "("  ++ show t ++ ") ∊ " ++ valueS
--        showVec [e] = e
--        showVec list = "(" ++ (intercalate "," list) ++ ")"
        valueSs =
            case maybeValues of
                Just valuesOut -> map showValue valuesOut
                _ -> replicate (length componentNames) "<no result computed>"
        showValue valueOut =
            show valueOut 
--            ++ "(err<=" ++ show err ++ ")" 
--            ++ "; valueIn = " ++ show valueIn
--            where
--            err = snd $ RefOrd.getEndpointsOutWithDefaultEffort $ wOut CF.<-> wIn
--            wOut = CF.width valueOut     
--            wIn = CF.width valueIn     
    showSplitReason indent (segInfo, (Just improvement)) =
        showSegInfo indent segInfo ++ 
        "; but splitting improves by " ++ show improvement ++ ":"
    showSplitReason indent (segInfo, Nothing) =
        showSegInfo indent segInfo ++ 
        " - thus splitting"


solveIVPWithUncertainValue ::
    (solvingInfo ~ (Maybe ([Fn],[Fn]), (CF, Maybe [CF])))
    =>
    Bool ->
    SizeLimits Fn -> 
    Int -> 
    CF -> 
    Int -> 
    CF -> 
    CF -> 
    CF -> 
    ODEIVP Fn
    -> 
    (
     Maybe ([CF])
    ,
        (
            BisectionInfo solvingInfo (solvingInfo, Maybe CF)
        )
    )
solveIVPWithUncertainValue
        shouldWrap 
            sizeLimits _substSplitSizeLimit
                delta m minStepSize maxStepSize splitImprovementThreshold
                    odeivp
    =
    solveUncertainValueExactTimeBisect2
        delta m minStepSize maxStepSize splitImprovementThreshold
            odeivp
    where
    solveUncertainValueExactTimeBisect2 =
        solveODEIVPUncertainValueExactTime_UsingPicard_Bisect shouldWrap True
            sizeLimits effSizeLims effCompose effEval effInteg effDeriv effInclFn 
            effAddFn effMultFn effAbsFn effMinmaxFn 
            effDivFnInt effAddFnDom effMultFnDom effCf

--    substituteInitialValueUncertainty fn =
--        pEvalAtPointOutEff effEval initValDomBox fn
--        where
--        initValDomBox =
--            fromList $ zip componentNames initialValues
--        
    effSizeLims = sizeLimitsChangeDefaultEffort sampleFn
    effCompose =  compositionDefaultEffort sampleFn -- (effCf, Int1To10 substSplitSizeLimit)
    effEval = evaluationDefaultEffort sampleFn -- (effCf, Int1To10 substSplitSizeLimit)
    effInteg = integrationDefaultEffort sampleFn
    effDeriv = fakeDerivativeDefaultEffort sampleFn
    effAddFn = ArithInOut.addDefaultEffort sampleFn
    effMultFn = ArithInOut.multDefaultEffort sampleFn
    effAbsFn = ArithInOut.absDefaultEffort sampleFn
    effMinmaxFn = NumOrd.minmaxInOutDefaultEffort sampleFn
    effAddFnDom = ArithInOut.mixedAddDefaultEffort sampleFn sampleCf
    effMultFnDom = ArithInOut.mixedMultDefaultEffort sampleFn sampleCf
--        ArithInOut.fldEffortMult sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effDivFnInt = ArithInOut.mixedDivDefaultEffort sampleFn (0::Int)
    effInclFn = RefOrd.pCompareDefaultEffort sampleFn -- ((Int1To1000 0, (effCf, Int1To10 20)), ())
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf

makeSampleWithVarsDoms :: 
     Int -> Int -> [Var Fn] -> [CF] -> Fn
makeSampleWithVarsDoms maxdeg maxsize vars doms =
    newConstFn cfg dombox sampleCf
    where
    domsLE = 
        map (fst . RefOrd.getEndpointsOutWithDefaultEffort) doms
    dombox = fromList $ zip vars doms 
    cfg =
        IntPolyCfg
        {
            ipolycfg_vars = vars,
            ipolycfg_domsLZ = zipWith (CF.<->) doms domsLE,
            ipolycfg_domsLE = domsLE,
            ipolycfg_sample_cf = sampleCf,
            ipolycfg_maxdeg = maxdeg,
            ipolycfg_maxsize = maxsize
        }

plotEnclosures :: 
    (Num (Domain f),
     CanEvaluate f,
     CairoDrawableFn f,
     HasSizeLimits f,
     RefOrd.RoundedLattice f,
     HasConstFns f,
     HasDomainBox f,
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f),
     Show f, Show (Var f), Show (Domain f)
    ) 
    =>
    ArithInOut.RoundedRealEffortIndicator (Domain f)
    -> Domain f
    -> Var f
    -> [String]
    -> BisectionInfo (Maybe ([f],[f]), t) splitReason
    -> IO ()
plotEnclosures effCF plotMinSegSize tVar componentNames bisectionInfo =
    do
    _ <- Gtk.unsafeInitGUIForThreadedRTS
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    _ <- FV.new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
    Gtk.mainGUI
    where
    ((sampleFn : _) : _) = fns 
    effDrawFn = cairoDrawFnDefaultEffort sampleFn
    effEval = evaluationDefaultEffort sampleFn
    
    addPlotVar = map $ map addV
        where
        addV fn = (fn, tVar)
    (fns, fnNames, segNames) = 
        aggregateSequencesOfTinySegments fnsAndNames 
    fnsAndNames = 
        map getFnsFromSegInfo $
            bisectionInfoGetLeafSegInfoSequence bisectionInfo
        where
        getFnsFromSegInfo (Just (fnVec, _), _) =
            zip fnVec componentNames
        getFnsFromSegInfo _ = []
    fnmeta = 
        (FV.defaultFnMetaData sampleFn)
        {
            FV.dataFnGroupNames = segNames, -- map ("segment " ++) (map show [1..segs]),
            FV.dataFnNames = fnNames,
            FV.dataFnStyles = map giveColours fnNames,
            FV.dataDomL = 0,
            FV.dataDomR = 4,
            FV.dataValLO = -2,
            FV.dataValHI = 2,
            FV.dataDomName = "t",
            FV.dataDefaultActiveFns = map whichActive fnNames,
            FV.dataDefaultEvalPoint = 0,
            FV.dataDefaultCanvasParams =
                (FV.defaultCanvasParams (0::CF))
                {
                    FV.cnvprmCoordSystem = 
                        FV.CoordSystemLinear $ 
                            FV.Rectangle  2 (-2) 0 (4)
                    ,
                    FV.cnvprmSamplesPerUnit = 200
                    ,
                    FV.cnvprmBackgroundColour = Just (1,1,1,1)
                }
        }
    aggregateSequencesOfTinySegments fnsAndNames2 = 
        aggrNewSegm [] [] [] $ zip ([1..]::[Int]) fnsAndNames2
        where
        aggrNewSegm 
                prevFns prevFnNames prevSegNames 
                segs@((segNoFirstSeg, fnsNamesFirstSeg@((fn1FirstSeg,_) : _)) : restSegs)
            | noAggregation =
                aggrNewSegm 
                    (fnsFirstSeg : prevFns) 
                    (fnNamesFirstSeg : prevFnNames) 
                    (("segment " ++ show segNoFirstSeg) : prevSegNames) 
                    restSegs 
            | otherwise =
                aggrNewSegm
                    (fnsAggregated : prevFns) 
                    (fnNamesAggregated : prevFnNames) 
                    (("segments " ++ show segNoFirstSeg ++ "-" ++ show segNoLastAggrSeg) : prevSegNames) 
                    restSegsAfterAggr
            where
            noAggregation = length smallSegmentsToAggregate <= 1
            (smallSegmentsToAggregate, restSegsAfterAggr) = 
                span segEndsBeforeLimit segs
                where
                segEndsBeforeLimit (_, ((fn1ThisSeg,_) : _)) =
                    (tEndThisSeg <=? tAggrLimit) == Just True
                    where
                    (_, tEndThisSeg) = getTVarDomEndpoints fn1ThisSeg
                    tAggrLimit = tStartFirstSeg <+> plotMinSegSize
            fnNamesAggregated =
                map (++ "(aggr)") componentNames
            fnsAggregated =
                foldl1 (zipWith (</\>)) $
                    chunksOf (length componentNames) $
                        map makeConstFnOverAggrDom $
                            concat $ map getFnsFromSeg smallSegmentsToAggregate
                where
                chunksOf _ [] = []
                chunksOf n list = firstN : (chunksOf n rest)
                    where
                    (firstN, rest) = splitAt n list
                getFnsFromSeg (_, fnsNames) = map fst fnsNames
                makeConstFnOverAggrDom fn =
                    newConstFn sizeLimitsNew domboxNew range
                    where
                    domboxNew = fromList [(tVar, aggrDom)]
                    sizeLimitsNew =
                        adjustSizeLimitsToVarsAndDombox fn [tVar] domboxNew sizeLimits
                    range = evalAtPointOutEff effEval dombox fn
                    sizeLimits = getSizeLimits fn         
                    dombox = getDomainBox fn
                    
            aggrDom = RefOrd.fromEndpointsOutWithDefaultEffort (tStartFirstSeg, tEndLastAggrSeg) 
            (tStartFirstSeg, _) = getTVarDomEndpoints fn1FirstSeg
            (_, tEndLastAggrSeg) = getTVarDomEndpoints fn1LastAggrSeg
            (segNoLastAggrSeg, ((fn1LastAggrSeg,_) : _)) = last smallSegmentsToAggregate 
            getTVarDomEndpoints fn =
                case lookupVar (getDomainBox fn) tVar of 
                    Just tDom -> RefOrd.getEndpointsOutWithDefaultEffort tDom 
            (fnsFirstSeg, fnNamesFirstSeg) = unzip fnsNamesFirstSeg
        aggrNewSegm prevFns prevFnNames prevSegNames _ =
            (reverse prevFns, reverse prevFnNames, reverse prevSegNames)
    whichActive list =
        take (length list) activityCycle 
        where
        activityCycle = cycle $ map snd $ zip componentNames $ 
            True : (repeat True) 
--            True : (repeat False) 
--            True : False : False : True : (repeat False) 
--            True : False : False : False : True : (repeat False) 
    
    giveColours list =
        take (length list) colourCycle
        where
        colourCycle = cycle $ map snd $ 
            zip componentNames 
                (cycle [blue, green, red, black])
--                (cycle [black]) 

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
    red = FV.defaultFnPlotStyle 
        { 
            FV.styleOutlineColour = Just (0.8,0.1,0.1,1), 
            FV.styleFillColour = Just (0.8,0.1,0.1,0.1) 
        } 


testShrinkWrap :: IO ()
testShrinkWrap =
    do
    mapM_ putStrLn $ map show $ getSides result
    return ()
    where
    getSides [x,y] =
        [(xSL,ySL),(xTL,yTL),(xSR,ySR),(xTR,yTR)]
        where
        xSL = pEvalAtPointOutEff effPEval sL x
        xSR = pEvalAtPointOutEff effPEval sR x
        xTL = pEvalAtPointOutEff effPEval tL x
        xTR = pEvalAtPointOutEff effPEval tR x
        ySL = pEvalAtPointOutEff effPEval sL y
        ySR = pEvalAtPointOutEff effPEval sR y
        yTL = pEvalAtPointOutEff effPEval tL y
        yTR = pEvalAtPointOutEff effPEval tR y
        sL = fromList [("s", -1)]
        sR = fromList [("s", 1)]
        tL = fromList [("t", -1)]
        tR = fromList [("t", 1)]
    
    Just result =
        shrinkWrap 
            effCompose effEval effDeriv effAddFn effAbsFn effMinmaxFn 
                effDivFnInt effAddFnDom effMultFnDom effCf 
                    [xUsingTS, yUsingTS]
    xUsingTS = s <*> tPlus2 <+> pmeps -- s(t+2) +- eps
    yUsingTS = (c1 <-> s <*> s) <*> tPlus2 <+> pmeps -- (1-s^2)(t+2) +- eps
    tPlus2 = (c1 <+> c1 <+> t)
    t = newProjectionFromSample c1 "t"
    s = (0.5 :: CF) |<*> newProjectionFromSample c1 "s"
    pmeps = newConstFnFromSample c1 $ ((-eps) CF.</\> eps)
        where
        eps = 0.125
    c1 :: Fn
    c1 = newConstFn cfg dombox (1 :: CF)
    dombox = fromList [("t", unitDom),  ("s", unitDom)]
    unitDom = (-1) CF.</\> 1
    cfg =
        IntPolyCfg
        {
            ipolycfg_vars = ["t","s"],
            ipolycfg_domsLZ = [0 CF.</\> 2, 0 CF.</\> 2],
            ipolycfg_domsLE = [-1,-1],
            ipolycfg_sample_cf = 0 :: CF,
            ipolycfg_maxdeg = 3,
            ipolycfg_maxsize = 1000
        }
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
    effEval = evaluationDefaultEffort sampleFn -- (effCf, Int1To10 substSplitSizeLimit)
    effPEval = partialEvaluationDefaultEffort sampleFn -- (effCf, Int1To10 substSplitSizeLimit)
    effCompose =  compositionDefaultEffort sampleFn -- (effCf, Int1To10 substSplitSizeLimit)
    effDeriv = fakeDerivativeDefaultEffort sampleFn
    effAddFn = ArithInOut.addDefaultEffort sampleFn
    effAbsFn = ArithInOut.absDefaultEffort sampleFn
    effMinmaxFn = NumOrd.minmaxInOutDefaultEffort sampleFn
--    effMinmaxFn = minmaxInOutDefaultEffortIntPolyWithBezierDegree 4 sampleFn
    effAddFnDom = ArithInOut.mixedAddDefaultEffort sampleFn sampleCf
    effMultFnDom = ArithInOut.mixedMultDefaultEffort sampleFn sampleCf
    effDivFnInt = ArithInOut.mixedDivDefaultEffort sampleFn (0::Int)
                
        