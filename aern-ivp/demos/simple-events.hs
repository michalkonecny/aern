{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.IVP.Specification.Hybrid
--import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Splitting
import Numeric.AERN.IVP.Solver.Events

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.Basis.Double ()
import qualified Numeric.AERN.DoubleBasis.Interval as CF
--import Numeric.AERN.RealArithmetic.Basis.MPFR
--import qualified Numeric.AERN.MPFRBasis.Interval as MI

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.ExactOps

--import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

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

import Numeric.AERN.Misc.Debug (unsafePrint)
_ = unsafePrint -- stop the unused warning

--import qualified Data.Map as Map
--import qualified Data.List as List

type CF = CF.DI
type Poly = IntPoly String CF

main :: IO ()
main =
    do
    args <- getArgs
    case length args of
        2 -> writeCSV args
        5 -> runOnce args
        _ -> usage
        
usage :: IO ()
usage =
    do
    putStrLn "Usage A: simple-events <ivp name> <output file name>"
    putStrLn "Usage B: simple-events <ivp name> <maxDeg> <minStepSize> <True|False-print steps?> <maxEvalSplitSize>"

ivpByName :: String -> HybridIVP Poly
ivpByName "ivpExpDecay-resetOnce" = ivpExpDecay_resetTHalf
----ivpByName "ivpSpringMass-reset" = ivpSpringMass_reset     
--
ivpExpDecay_resetTHalf :: HybridIVP Poly
ivpExpDecay_resetTHalf =
    ivp
    where
    system =
        HybridSystem
        {
            hybsys_componentNames = ["x","time"],
            hybsys_modeFields = Map.fromList [(modeNormal, odeNormal)],
            hybsys_eventModeSwitchesAndResetFunctions =
                Map.fromList [(eventReset, (modeNormal, switchReset))],
            hybsys_eventDetector = eventDetector
        }
    modeNormal = HybSysMode "normal"
    odeNormal :: [Poly] -> [Poly]
    odeNormal [x,time] = [neg x, newConstFnFromSample time (1)]
    eventReset = HybSysEventKind "reset"
    switchReset :: [Poly] -> [Poly]
    switchReset [x,time] = [newConstFnFromSample x initValue, time]
    eventDetector :: HybSysMode -> [Poly] -> Set.Set (HybSysEventKind, Bool)
    eventDetector _mode [_x,time] =
--        let ?pCompareEffort = NumOrd.pCompareDefaultEffort x in
        case (time <? tEventPoly, tEventPoly <? time) of
            (Just True, _) -> Set.empty
            (_, Just True) -> Set.empty
            (Just False, Just False) -> Set.singleton (eventReset, True)
            _ -> Set.singleton (eventReset, False)
        where
        tEventPoly = newConstFnFromSample time $ 1 <*>| tEventDbl
    tEventDbl = 0.5 :: Double

   
    
    ivp :: HybridIVP Poly
    ivp =
        HybridIVP
        {
            hybivp_description = description,
            hybivp_system = system,
            hybivp_tVar = "t",
            hybivp_tStart = 0,
            hybivp_tEnd = 1,
            hybivp_initialStateEnclosure = 
                HybridSystemUncertainState 
                { 
                    hybstate_modes = Set.singleton modeNormal,
                    hybstate_values = [initValue, tStart]
                },
            hybivp_maybeExactStateAtTEnd = Just $
                HybridSystemUncertainState 
                {
                    hybstate_modes = Set.singleton modeNormal,
                    hybstate_values = [xEnd, tEnd]
                }
        }
    description =
        "x' = -x; if t = " ++ show tEventDbl ++ " then x := " ++ show initValue 
        ++ "; x(" ++ show tStart ++ ") = " ++ show initValue
    initValue = 1 :: CF
    tStart = hybivp_tStart ivp
    tEnd = hybivp_tEnd ivp
    xEnd = 1 CF.<*>| (exp (-tEndDbl+tEventDbl) :: Double)
    tEndDbl :: Double
    (Just tEndDbl) = ArithUpDn.convertUpEff () tEnd

--ivpSpringMass_ut :: Bool -> ODEIVP Poly
--ivpSpringMass_ut withInitialValueUncertainty =
--    ivp
--    where
--    ivp =
--        ODEIVP
--        {
--            odeivp_description = description,
--            odeivp_field = \ [x,x'] -> [x',neg x],
--            odeivp_componentNames = ["x","x'"],
--            odeivp_tVar = "t",
--            odeivp_tStart = -0.125,
--            odeivp_t0End = 0.125,
----            odeivp_tEnd = 0.125,
--            odeivp_tEnd = 1,
--            odeivp_makeInitialValueFnVec = makeIV,
--            odeivp_maybeExactValuesAtTEnd = Just $
--                case withInitialValueUncertainty of
--                    True ->
--                        [
--                            ((0.75 * cosTEndPlusEps) - 0.125 * sinTEndPlusEps)  
--                            CF.</\>
--                            ((1.25 * cosTEndMinusEps) + 0.125 * sinTEndMinusEps)
--                        ,
--                            foldl1 (CF.</\>) $
--                                [ 
--                                    (-(sinTEndPlusEps) - 0.125 * cosTEndPlusEps),  
--                                    (-(sinTEndPlusEps) + 0.125 * cosTEndPlusEps),  
--                                    (-0.75*(sinTEndPlusEps) - 0.125 * cosTEndPlusEps),  
--                                    (-0.75*(sinTEndPlusEps) + 0.125 * cosTEndPlusEps),  
--                                    (-(sinTEndMinusEps) + 0.125 * cosTEndMinusEps),
--                                    (-(sinTEndMinusEps) - 0.125 * cosTEndMinusEps),
--                                    (-1.25*(sinTEndMinusEps) + 0.125 * cosTEndMinusEps),
--                                    (-1.25*(sinTEndMinusEps) - 0.125 * cosTEndMinusEps)
--                                ]
--                        ]
--                    False ->
--                        [
--                            ((0.875 * cosTEndPlusEps))  
--                            CF.</\>
--                            ((1.125 * cosTEndMinusEps))
--                        ,
--                            (-(0.875 * sinTEndPlusEps))  
--                            CF.</\>
--                            (-(1.125 * sinTEndMinusEps))
--                        ]
--        }
--    cosTEndPlusEps = 1 CF.<*>| (cos (tEndDbl + 0.125) :: Double)
--    cosTEndMinusEps = 1 CF.<*>| (cos (tEndDbl - 0.125) :: Double)
--    sinTEndPlusEps = 1 CF.<*>| (sin (tEndDbl + 0.125) :: Double)
--    sinTEndMinusEps = 1 CF.<*>| (sin (tEndDbl - 0.125) :: Double)
--    description =
--        "x'' = -x; " 
--        ++ show tStart ++ " < t_0 < " ++ show t0End
--        ++ "; (x,x')(t_0) ∊ " ++ (show $ makeIV dummySizeLimits "t_0" tStart)
--    tStart = odeivp_tStart ivp
--    t0End = odeivp_t0End ivp
--    tEndDbl :: Double
--    (Just tEndDbl) = ArithUpDn.convertUpEff () $ odeivp_tEnd ivp
--    componentNames = odeivp_componentNames ivp
--    dummySizeLimits =
--        getSizeLimits $
--            makeSampleWithVarsDoms 10 10 [] []
--    makeIV sizeLimits t0Var t0Dom =
--        case withInitialValueUncertainty of
--            True ->
--                [
--                    ((1 :: Int) |<+> t0VarFn) <+> (xUnitFn </>| (8::Int))
--                ,
--                    (xdUnitFn </>| (8::Int))
--                ]
--            False ->
--                [
--                    (1 :: Int) |<+> t0VarFn
--                ,
--                    (0 :: Int) |<*> t0VarFn
--                ]
--        where
--        t0VarFn = newProjectionFromSample sampleInitialValueFn t0Var
--        xUnitFn = newProjectionFromSample sampleInitialValueFn "x"
--        xdUnitFn = newProjectionFromSample sampleInitialValueFn "x'"
--        sampleInitialValueFn =
--            makeSampleWithVarsDoms 
--                maxdeg maxsize 
--                (t0Var : componentNames) (t0Dom : componentUncertaintyDomains)
--            where
--            componentUncertaintyDomains =
--                map snd $ zip componentNames $ repeat unitDom
--            unitDom = (-1) CF.</\> 1 
--        maxdeg = ipolycfg_maxdeg sizeLimits
--        maxsize = ipolycfg_maxsize sizeLimits


runOnce :: [String] -> IO ()
runOnce [ivpName, maxDegS, depthS, shouldShowStepsS, maxSplitSizeS] =
    do
    let maxDeg = read maxDegS :: Int
    let depth = read depthS :: Int
    let maxSplitSize = read maxSplitSizeS :: Int
    let shouldShowSteps = read shouldShowStepsS :: Bool
    putStrLn "---------------------------------------------------"
    putStrLn "demo of solve-VtE from (Konecny, Taha, Duracz 2012)"
    putStrLn "---------------------------------------------------"
    _ <- solveEventsPrintSteps shouldShowSteps ivp (maxDeg, depth, maxSplitSize)
    return ()
    where
    ivp = ivpByName ivpName

writeCSV :: [String] -> IO ()
writeCSV [ivpName, outputFileName] =
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
    ivp = ivpByName ivpName
    paramCombinations = 
        [(maxDegree, depth) | 
            maxDegree <- [0..20], depth <- [0,5..50]]
--            maxDegree <- [0..10], depth <- [0..5]]
    writeCSVheader handle =
        do
        hPutStrLn handle $ "ivp: " ++ description
--        hPutStrLn handle $ "polynomial degree, min step size (2^(-n)), time (microseconds), error upper bound at t=1, error at t = 1"
        hPutStrLn handle $ "polynomial degree, min step size (2^(-n)), time (microseconds), error at t = 1"
    runSolverMeasureTimeMSwriteLine handle (maxDegree, depth) =
        do
        resultsAndTimes <- mapM solveAndMeasure ([1..1] :: [Int])
        let 
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
            maybeSolverResult <- timeout oneHourInMicroS $ solveEventsPrintSteps False ivp (maxDegree, depth, 4*maxDegree*maxDegree)
            endtime <- getCPUTime
            let solverResult = (case maybeSolverResult of Just solverResult2 -> solverResult2; Nothing -> (Nothing, undefined)) 
            return $ (solverResult, (endtime - starttime) `div` 1000000000)
        oneHourInMicroS = 3600 * oneSecondInMicroS
        oneSecondInMicroS = 1000000
        
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
                Just (HybridSystemUncertainState _ vecOut) ->
                    case maybeStateExact of
                        Just (HybridSystemUncertainState _ vecExact) -> 
                                computeDiff vecOut vecExact
                        _ -> show "exact solution not known"
                where
                computeDiff vecOut vecOther = 
                    removeBracks $
                    show $
                        snd $ RefOrd.getEndpointsOutWithDefaultEffort $ 
--                            foldl1 min $ -- assuming that the components are interdependent - some may be bad due to dependency errors in the projection 
                            foldl1 max $ 
                                zipWith (CF.<->) (map CF.width vecOut) (map CF.width vecOther)
        removeBracks ('<': rest1 ) =
            reverse $ removeR $ reverse rest1
            where
            removeR ('>' : rest2 ) = rest2

refinesVec :: [CF] -> [CF] -> Bool
refinesVec vec1 vec2 =
    and $ zipWith refines vec1 vec2
refines :: CF -> CF -> Bool
refines a1 a2 = 
    (a2 CF.|<=? a1) == Just True
    where
--    tolerance = 2 ^^ (-50)

solveEventsPrintSteps :: 
    (solvingInfo ~ (CF, Maybe (HybridSystemUncertainState Poly), [(HybSysMode, EventInfo Poly)]))
    =>
    Bool
    ->
    HybridIVP Poly 
    -> 
    (Int, Int, Int) 
    -> 
    IO (Maybe (HybridSystemUncertainState Poly), SplittingInfo solvingInfo (solvingInfo, Maybe CF))
solveEventsPrintSteps shouldShowSteps ivp (maxdegParam, depthParam, maxSplitSizeParam) =
    do
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
            putStr $ showSegInfo "   " (tEnd, Just exactResult, [])
        _ -> return ()
    putStrLn "----------  result: -----------------------------"
    putStr $ showSegInfo ">>> " (tEnd, maybeEndState, [])
    case (maybeExactResult, maybeEndState) of
        (Just exactResult, Just resultOut) ->
            putStrLn $ "error = " ++ show (getErrorState exactResult resultOut)
        _ -> return ()
    case shouldShowSteps of
        True ->
            do
            putStrLn "----------  steps: ------------------------------"
            putStrLn $ showSplittingInfo showSegInfo showSplitReason "" splittingInfo
        False -> return ()
    putStrLn "-------------------------------------------------"
    return (maybeEndState, splittingInfo)
    where
    (maybeEndState, splittingInfo) =
        solveHybridIVP
            sizeLimits effCf substSplitSizeLimit
                delta m minStepSize splitImprovementThreshold
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
    splitImprovementThreshold = 2^^(-48 :: Int)
    
    -- auxiliary:
    description = hybivp_description ivp
    tEnd = hybivp_tEnd ivp
    maybeExactResult = hybivp_maybeExactStateAtTEnd ivp
    hybsys = hybivp_system ivp
    componentNames = hybsys_componentNames hybsys

    sampleCf = 0 :: CF
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
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
        (HybridSystemUncertainState exactModeSet exactVec) = exactState
        (HybridSystemUncertainState approxModeSet approxVec) = approxState
        getError (valueIn, valueOut) =
            err
            where
            err = snd $ RefOrd.getEndpointsOutWithDefaultEffort $ wOut CF.<-> wIn
            wOut = CF.width valueOut     
            wIn = CF.width valueIn     
    showSegInfo indent (t, maybeState, modeEventInfoList) =
        indent ++ "mode(" ++ show t ++ ") ∊ " ++ modesS ++ "\n" ++
        (unlines $ map (showComponent indent) $ zip componentNames valueSs)
        ++ (unlines $ map showModeEventInfo modeEventInfoList)
        where
        showModeEventInfo (mode, eventInfo) =
            indent ++ "events assuming mode at the start of segment = " ++ show mode ++ ":\n" ++
            showEventInfo (indent ++ "  ") (show . fst) eventInfo
        showComponent indent2 (name, valueS) =
            indent2 ++ name ++ "("  ++ show t ++ ") ∊ " ++ valueS
--        showVec [e] = e
--        showVec list = "(" ++ (intercalate "," list) ++ ")"
        (modesS, valueSs) =
            case maybeState of
                Just (HybridSystemUncertainState modeSet values) -> 
                    (show modeSet, map showValue values)
                _ ->
                    ("<no result computed>", 
                     replicate (length componentNames) "<no result computed>")
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
        indent ++ "; but splitting improves by " ++ show improvement ++ ":"
    showSplitReason indent (segInfo, Nothing) =
        showSegInfo indent segInfo ++ 
        indent ++ "; but splitting helps :"

solveHybridIVP ::
    (solvingInfo ~ (CF, Maybe (HybridSystemUncertainState Poly), [(HybSysMode, EventInfo Poly)]))
    =>
    SizeLimits Poly -> 
    ArithInOut.RoundedRealEffortIndicator CF ->
    Int -> 
    CF ->
    Int ->
    CF ->
    CF ->
    Var Poly ->
    HybridIVP Poly 
    ->
    (
     Maybe (HybridSystemUncertainState Poly)
    ,
     SplittingInfo solvingInfo (solvingInfo, Maybe CF)
    )
solveHybridIVP 
        sizeLimits effCf substSplitSizeLimit
            delta m minStepSize splitImprovementThreshold 
                t0Var
                    hybivp
    =
    result
    where
    result =
        solveEventsTimeSplit
            sizeLimits effCompose effEval effInteg effInclFn effAddFn effAddFnDom effCf
                delta m t0Var minStepSize splitImprovementThreshold
                    hybivp

    sampleCf = delta 
    
--    effSizeLims = effCf
    effCompose = (effCf, Int1To10 substSplitSizeLimit)
    effEval = (effCf, Int1To10 substSplitSizeLimit)
    effInteg = effCf
    effAddFn = effCf
--    effMultFn = effCf
    effAddFnDom =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effInclFn = ((Int1To1000 0, (effCf, Int1To10 20)), ())


makeSampleWithVarsDoms :: 
     Int -> Int -> [Var Poly] -> [CF] -> Poly
makeSampleWithVarsDoms maxdeg maxsize vars doms =
    newConstFn cfg dombox sampleCf
    where
    sampleCf = 0 :: CF
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
     
