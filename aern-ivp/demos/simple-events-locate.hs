{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Numeric.AERN.IVP.Examples.Hybrid.Simple

import Numeric.AERN.IVP.Specification.Hybrid
--import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Bisection
import Numeric.AERN.IVP.Solver.Events.Aggregate
import Numeric.AERN.IVP.Solver.Events.SplitNearEvents
import Numeric.AERN.IVP.Plot.UsingFnView (plotHybIVPListEnclosures)

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot ()

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.Basis.Double ()
import qualified Numeric.AERN.DoubleBasis.Interval as CF
--import Numeric.AERN.RealArithmetic.Basis.MPFR
--import qualified Numeric.AERN.MPFRBasis.Interval as MI

--import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsDefaultEffort


import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.ShowInternals

--import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Data.Map as Map


import System.IO
import System.Environment
import System.Directory
import System.CPUTime
import System.Timeout

--import qualified Control.Concurrent as Concurrent

import Numeric.AERN.Misc.Debug
_ = unsafePrint -- stop the unused warning

--import qualified Data.Map as Map
--import qualified Data.List as List

type CF = CF.DI
type Poly = IntPoly String CF

sampleCf :: CF
sampleCf = 0

samplePoly :: Poly
samplePoly = makeSampleWithVarsDoms 10 10 ["x"] [sampleCf]

main :: IO ()
main =
    do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case length args of
        3 -> writeCSV args
        8 -> runOnce args
        _ -> usage
        
usage :: IO ()
usage =
    do
    putStrLn "Usage A: simple-events <ivp name> <end time> <output file name>"
    putStrLn "Usage B: simple-events <ivp name> <end time> <maxDeg> <minStepSize> <True|False-print steps?> <maxEvalSplitSize>"


{--- END OF HYBRID SYSTEM DEFINITIONS ---}

runOnce :: [String] -> IO ()
runOnce [ivpName, endTimeS, maxDegS, depthS, minDepthS, shouldPlotStepsS, shouldShowStepsS, maxSplitSizeS] =
    do
    let maxDeg = read maxDegS :: Int
    let depth = read depthS :: Int
    let minDepth = read minDepthS :: Int
    let maxSplitSize = read maxSplitSizeS :: Int
    let shouldShowSteps = read shouldShowStepsS :: Bool
    let shouldPlotSteps = read shouldPlotStepsS :: Bool
    _ <- solveEventsPrintSteps shouldPlotSteps shouldShowSteps ivp (maxDeg, depth, minDepth, maxSplitSize)
    return ()
    where
    ivp = ivpByNameReportError ivpName endTimeDbl samplePoly
    endTimeDbl = read endTimeS :: Double
    

writeCSV :: [String] -> IO ()
writeCSV [ivpName, endTimeS, outputFileName] =
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
    ivp = ivpByNameReportError ivpName endTimeDbl samplePoly
    endTimeDbl = read endTimeS :: Double
    paramCombinations = 
        [(maxDegree, depth) | 
            maxDegree <- [0..10], depth <- [0,5..60]]
--            maxDegree <- [0..10], depth <- [0..5]]
    writeCSVheader handle =
        do
        hPutStrLn handle $ "ivp: " ++ description
--        hPutStrLn handle $ "polynomial degree, min step size (2^(-n)), time (microseconds), error upper bound at t=1, error at t = 1"
        hPutStrLn handle $ "polynomial degree, min step size (2^(-n)), time (microseconds), error at t = 1"
    runSolverMeasureTimeMSwriteLine handle (maxDegree, depth) =
        do
        resultsAndTimes <- mapM solveAndMeasure ([1..1] :: [Int])
        let ((result, _) : _)  = resultsAndTimes
        let averageTime = average $ map snd resultsAndTimes
        hPutStrLn handle $ makeCSVLine ((result, averageTime), (maxDegree, depth))
        where
        average list = (2 * (sum list) + n) `div` (2 * n)
            where
            n = fromIntegral $ length list
        solveAndMeasure _ =
            do
            starttime <- getCPUTime
            maybeSolverResult <- timeout (10 * oneMinuteInMicroS) $ solveEventsPrintSteps False False ivp (maxDegree, depth, minDepth, 4*maxDegree*maxDegree)
            endtime <- getCPUTime
            let solverResult = tweakSolverResult maybeSolverResult 
            return $ (solverResult, (endtime - starttime) `div` 1000000000)
            where
            tweakSolverResult (Just solverResult2) = solverResult2
            tweakSolverResult Nothing = (Nothing, undefined)
--            oneHourInMicroS = 60 * oneMinuteInMicroS
            oneMinuteInMicroS = 60 * oneSecondInMicroS
            oneSecondInMicroS = 1000000
        minDepth = 1
    
    description = hybivp_description ivp
    maybeStateExact = hybivp_maybeExactStateAtTEnd ivp
    makeCSVLine (((maybeState, _), execTimeMS), (maxDegree, depth)) =
        show maxDegree ++ "," 
        ++ show depth ++ ","
        ++ show execTimeMS ++ ","
--        ++ enclosureErrorBoundS ++ ","
        ++ enclosureErrorS
        where
        enclosureErrorS =
            case maybeState of
                Nothing -> show "no solution"
                Just stateOut ->
                    case maybeStateExact of
                        Just stateExact -> 
                                computeDiff stateOut stateExact
                        _ -> show "exact solution not known"
                where
                computeDiff stateOut stateOther = 
                    removeBracks $
                    show $ measureImprovementState sampleCf effCf stateOut stateOther
--                        snd $ RefOrd.getEndpointsOutWithDefaultEffort $ 
----                            foldl1 min $ -- assuming that the components are interdependent - some may be bad due to dependency errors in the projection 
--                            foldl1 max $ 
--                                zipWith (CF.<->) (map CF.width vecOut) (map CF.width vecOther)
        removeBracks ('<': rest1 ) =
            reverse $ removeR $ reverse rest1
            where
            removeR ('>' : rest2 ) = rest2
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
--    effImprCf = imprecisionDefaultEffort sampleCf

refinesVec :: [CF] -> [CF] -> Bool
refinesVec vec1 vec2 =
    and $ zipWith refines vec1 vec2
refines :: CF -> CF -> Bool
refines a1 a2 = 
    (a2 CF.|<=? a1) == Just True
    where
--    tolerance = 2 ^^ (-50)

solveEventsPrintSteps :: 
    (
     solvingInfoODESegment ~ (Maybe ([Poly],[Poly]), (CF, Maybe [CF])),
     solvingInfoODE ~ BisectionInfo solvingInfoODESegment (solvingInfoODESegment, Maybe CF),
     solvingInfoEvents ~ (CF, Maybe (HybridSystemUncertainState CF), EventInfo Poly)
    )
    =>
    Bool
    ->
    Bool
    ->
    HybridIVP Poly 
    -> 
    (Int, Int, Int, Int) 
    -> 
    IO 
    (
        Maybe (HybridSystemUncertainState CF)
    ,
        [(
            CF
            -- end time of this segment (including the event resolution sub-segment)  
         ,
            Maybe (HybridSystemUncertainState CF)
         ,
            Map.Map HybSysMode 
                (
                    solvingInfoODE,
                    Maybe (HybridSystemUncertainState CF),
                    Maybe solvingInfoEvents
                )
         )
        ]
    )
solveEventsPrintSteps shouldPlotSteps shouldShowSteps ivp (maxdegParam, depthParam, minDepthParam, maxSplitSizeParam) =
    do
    putStrLn "--------------------------------------------------------"
    putStrLn "demo of simulation based on (Konecny, Taha, Duracz 2012)" 
    putStrLn "with experimental splitting guided by event localisation"
    putStrLn "--------------------------------------------------------"
    putStrLn $ "solving: " ++ description
    putStrLn "-------------------------------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "m = " ++ show m
    putStrLn $ "substSplitSizeLimit = " ++ show substSplitSizeLimit
    putStrLn $ "minimum step size = 2^{" ++ show minStepSizeExp ++ "}"
    putStrLn $ "split improvement threshold = " ++ show splitImprovementThreshold
    case maybeExactResult of
        Just exactResult ->
            do
            putStrLn "(almost) exact result = "
            putStr $ showState "   " (tEnd, Just exactResult)
        _ -> return ()

    putStrLn "----------  steps: ---------------------------"
    _ <- printStepsInfo (1::Int) segmentsInfo

    putStrLn "----------  step summary: -----------------------"
    putStrLn $ "number of atomic segments = " ++ (show $ length segmentsInfo)
    putStrLn $ "smallest segment size: " ++ (show smallestSegSize)  
        
    putStrLn "----------  result: -----------------------------"
    putStr $ showState ">>> " (tEnd, maybeEndState)
    case (maybeExactResult, maybeEndState) of
        (Just exactResult, Just resultOut) ->
            putStrLn $ "error = " ++ show (getErrorState exactResult resultOut)
        _ -> return ()
    putStrLn $ "event count = " ++ show eventCount
    putStrLn "-------------------------------------------------"

    case shouldPlotSteps of
        False -> return ()
        True -> plotHybIVPListEnclosures effCf (2^^(-12 :: Int) :: CF) ivp segmentsInfo

    return (maybeEndState, segmentsInfo)
    where
    (maybeEndState, segmentsInfo) =
        solveHybridIVP
            sizeLimits effCf substSplitSizeLimit
                delta m minStepSize maxStepSize splitImprovementThreshold
                    "t0" 
                        ivp
    -- parameters:
    delta = 1
    maxdeg = maxdegParam
    maxsize = 500
    m = 20
    substSplitSizeLimit = maxSplitSizeParam -- 2^t0maxdeg
--    minStepSizeExp = -4 :: Int
    minStepSizeExp = - depthParam
    minStepSize = 2^^minStepSizeExp
    maxStepSizeExp = - minDepthParam
    maxStepSize = 2^^maxStepSizeExp
    splitImprovementThreshold = 2^^(-48 :: Int)
    
    -- auxiliary:
    description = hybivp_description ivp
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    maybeExactResult = hybivp_maybeExactStateAtTEnd ivp
    hybsys = hybivp_system ivp
    componentNames = hybsys_componentNames hybsys

    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
    effJoinCf = RefOrd.joinmeetDefaultEffort sampleCf
    sizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms maxdeg maxsize [] []
            
    getErrorState exactState approxState
        | not (exactModeSet `Set.isSubsetOf` approxModeSet) =
            error $ 
                "enclosure error:"
                ++ "\n approxModeSet = " ++ show approxModeSet
                ++ "\n exactModeSet = " ++ show exactModeSet
        | not (refinesVec exactVec approxVec) = -- && refinesVec vecIn vecExact) ->
            error $ 
                "enclosure error:"
                ++ "\n approxVec = " ++ show approxVec
                ++ "\n exactVec = " ++ show exactVec
        | otherwise =
            map getError $ zip exactVec approxVec
        where
        (exactModeSet, exactVec) = getHybridStateUnion effJoinCf exactState
        (approxModeSet, approxVec) = getHybridStateUnion effJoinCf approxState
        getError (valueIn, valueOut) =
            err
            where
            err = snd $ RefOrd.getEndpointsOutWithDefaultEffort $ wOut CF.<-> wIn
            wOut = CF.width valueOut     
            wIn = CF.width valueIn     
    
    eventCount =
        foldl (<+>) 0 $ map getSegmentEventCount segmentsInfo
        where
        getSegmentEventCount (_, Nothing, _) =  -- something failed on this segment 
            0 CF.</\> (plusInfinity 0)
        getSegmentEventCount (_, Just _, modeMap) = 
            foldl1 (CF.</\>) $ map getModeEventCount $ Map.elems modeMap
        getModeEventCount (_, _, Nothing) = 0 -- no events here
        getModeEventCount (_, _, Just (_, _, eventInfo)) = 
            eventInfoCountEvents 0 effCf eventInfo
    smallestSegSize =
        aux tStart (tEnd CF.<-> tStart) segmentsInfo
        where
        aux _tPrev tSmallestSoFar [] = tSmallestSoFar
        aux tPrev tSmallestSoFar ((tNow,_,_) : rest) =
            aux tNow tSmallestUpdated rest
            where
            tSmallestUpdated =
                CF.minOut tSmallestSoFar (tNow CF.<-> tPrev)
            
    showStepInfo (n, t) =
        "step " ++ show n ++ ": t = " ++ show t
    printStepsInfo _ [] = return ()
    printStepsInfo n ((t,_,_) : rest) =
        do
        putStrLn $ showStepInfo (n, t)
        printStepsInfo (n+1) rest
    showState indent (t, maybeState) =
        indent ++ "mode(" ++ show t ++ ") ∊ " ++ modesS ++ "\n" ++
        (unlines $ map (showComponent indent) $ zip componentNames valueSs)
        where
        showComponent indent2 (name, valueS) =
            indent2 ++ name ++ "("  ++ show t ++ ") ∊ " ++ valueS
        (modesS, valueSs) =
            case maybeState of
                Just state ->
                    (show modeSet, map showValue values)
                    where
                    (modeSet, values) = getHybridStateUnion effJoinCf state
                _ ->
                    ("<no result computed>", 
                     replicate (length componentNames) "<no result computed>")
        showValue valueOut =
            show valueOut 
        
    showSegInfo indent (t, maybeState, modeSolvingInfoMap) =
        maybeEventsCountS
        ++ (showState indent (t, maybeState))
--        ++ (unlines $ map showModeEventInfo modeEventInfoList)
        where
        eventInfoList = map getEventInfo $ filter hasEventInfo $ Map.elems modeSolvingInfoMap
        hasEventInfo (_, _, Nothing) = False
        hasEventInfo _ = True
        getEventInfo (_, _, Just eventInfo) = eventInfo
        maybeEventsCountS 
            | null eventInfoList = ""
            | otherwise =
                indent ++ "events on this time segment: " ++ eventsS ++ "\n" 
        eventsS =
            show $
            foldl1 (CF.</\>) $ map (eventInfoCountEvents 0 effCf) eventInfoList 
--        showModeEventInfo (mode, eventInfo) =
--            indent ++ "events assuming mode at the start of segment = " ++ show mode ++ ":\n" ++
--            showEventInfo (indent ++ "  ") (show . fst) eventInfo

solveHybridIVP ::
    (
     solvingInfoODESegment ~ (Maybe ([Poly],[Poly]), (CF, Maybe [CF])),
     solvingInfoODE ~ BisectionInfo solvingInfoODESegment (solvingInfoODESegment, Maybe CF),
     solvingInfoEvents ~ (CF, Maybe (HybridSystemUncertainState CF), EventInfo Poly)
    )
    =>
    SizeLimits Poly -> 
    ArithInOut.RoundedRealEffortIndicator CF ->
    Int -> 
    CF ->
    Int ->
    CF ->
    CF ->
    CF ->
    Var Poly ->
    HybridIVP Poly 
    ->
    (
        Maybe (HybridSystemUncertainState CF)
    ,
        [(
            CF
            -- end time of this segment (including the event resolution sub-segment)  
         ,
            Maybe (HybridSystemUncertainState CF)
         ,
            Map.Map HybSysMode 
                (
                    solvingInfoODE,
                    Maybe (HybridSystemUncertainState CF),
                    Maybe solvingInfoEvents
                )
         )
        ]
    )
solveHybridIVP 
        sizeLimits effCf substSplitSizeLimit
            delta m minStepSize maxStepSize splitImprovementThreshold 
                t0Var
                    hybivp
    =
    result
    where
    result =
        solveHybridIVP_UsingPicardAndEventTree_SplitNearEvents
            sizeLimits effSizeLims effPEval effCompose effEval effInteg effDeriv effInclFn 
                effAddFn effMultFn effAbsFn effMinmaxFn 
                effDivFnInt effAddFnDom effMultFnDom effCf
                delta m t0Var minStepSize maxStepSize splitImprovementThreshold
                    hybivp

    effSizeLims = effCf
    effCompose = (effCf, Int1To10 substSplitSizeLimit)
    effEval = (effCf, Int1To10 substSplitSizeLimit)
    effPEval = (effCf, Int1To10 substSplitSizeLimit)
    effInteg = effCf
    effDeriv = effCf
    effAddFn = effCf
    effMultFn = effCf
    effAbsFn = ArithInOut.absDefaultEffort samplePoly
    effMinmaxFn = NumOrd.minmaxInOutDefaultEffort samplePoly
    effAddFnDom =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effMultFnDom =
        ArithInOut.mixedMultDefaultEffort samplePoly sampleCf
--        ArithInOut.fldEffortMult sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effDivFnInt =
        ArithInOut.mxfldEffortDiv sampleCf (0::Int) $ ArithInOut.rrEffortIntMixedField sampleCf effCf
    effInclFn = ((Int1To1000 0, (effCf, Int1To10 20)), ())


makeSampleWithVarsDoms :: 
     Int -> Int -> [Var Poly] -> [CF] -> Poly
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
     
    