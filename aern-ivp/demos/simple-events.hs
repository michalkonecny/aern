{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Numeric.AERN.IVP.Examples.Hybrid.Simple

import Numeric.AERN.IVP.Specification.Hybrid
--import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Bisection
import Numeric.AERN.IVP.Solver.Events.EventTree
import Numeric.AERN.IVP.Solver.Events.Bisection
import Numeric.AERN.IVP.Solver.Events.SplitNearEvents
import Numeric.AERN.IVP.Plot.UsingFnView 
    (plotHybIVPBisectionEnclosures, plotHybIVPListEnclosures)

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot ()

import Numeric.AERN.RmToRn

import Numeric.AERN.RealArithmetic.Basis.Double ()
import qualified Numeric.AERN.DoubleBasis.Interval as CF
--import Numeric.AERN.RealArithmetic.Basis.MPFR
--import qualified Numeric.AERN.MPFRBasis.Interval as MI

--import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators
import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsDefaultEffort


import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.SizeLimits
--import Numeric.AERN.Basics.ShowInternals

--import Data.List (intercalate)
import qualified Data.Set as Set
import qualified Data.Map as Map


import System.IO
import System.Environment
--import System.Directory
--import System.CPUTime
--import System.Timeout

--import qualified Numeric.AERN.RmToRn.Plot.FnView as FV
--import Numeric.AERN.RmToRn.Plot.CairoDrawable

--import qualified Graphics.UI.Gtk as Gtk
--import qualified Control.Concurrent as Concurrent
--import Control.Concurrent.STM

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
        9 -> runOnce args
        _ -> usage
        
usage :: IO ()
usage =
    do
--    putStrLn "Usage A: simple-events [locate|bisect] [evtree|pwl] <ivp name> <end time> <output file name>"
    putStrLn "Usage: simple-events [locate|bisect] [evtree|pwl] <ivp name> <end time> <maxDeg> <maxUnitSplitDepth> <minUnitSplitDepth> <True|False-plot steps?> <maxEvalSplitSize>"

data TopLevelStrategy =
    TopLevelBisect | TopLevelLocate

topLevelStrategyFromS :: String -> TopLevelStrategy
topLevelStrategyFromS "bisect" = TopLevelBisect
topLevelStrategyFromS "locate" = TopLevelLocate
topLevelStrategyFromS s = error $ "unknown top level strategy: " ++ s 

data BasicStepType =
    BasicStepEvTree | BasicStepPWL

basicStepTypeFromS :: String -> BasicStepType
basicStepTypeFromS "evtree" = BasicStepEvTree 
basicStepTypeFromS "pwl" = BasicStepPWL 
basicStepTypeFromS s = error $ "unknown basic step type: " ++ s 

runOnce :: [String] -> IO ()
runOnce [topLevelStrategyS, basicStepTypeS, ivpName, endTimeS, maxDegS, depthS, minDepthS, shouldPlotStepsS, maxSplitSizeS] =
    do
    putStrLn $ hybivp_description ivp
    let maxDeg = read maxDegS :: Int
    let depth = read depthS :: Int
    let minDepth = read minDepthS :: Int
    let maxSplitSize = read maxSplitSizeS :: Int
    let shouldPlotSteps = read shouldPlotStepsS :: Bool
    let topLevelStrategy = topLevelStrategyFromS topLevelStrategyS
    let basicStepType = basicStepTypeFromS basicStepTypeS
    solveEventsPrintSteps topLevelStrategy basicStepType shouldPlotSteps ivp (maxDeg, depth, minDepth, maxSplitSize)
    return ()
    where
    ivp = ivpByNameReportError ivpName endTimeDbl samplePoly
    endTimeDbl = read endTimeS :: Double
    
refinesVec :: [CF] -> [CF] -> Bool
refinesVec vec1 vec2 =
    and $ zipWith refines vec1 vec2
refines :: CF -> CF -> Bool
refines a1 a2 = 
    (a2 CF.|<=? a1) == Just True
    where
--    tolerance = 2 ^^ (-50)

solveEventsPrintSteps :: 
--    (solvingInfo ~ (CF, Maybe (HybridSystemUncertainState CF), [(HybSysMode, EventInfo Poly)]))
--    =>
--    (
--     solvingInfoODESegment ~ (Maybe ([Poly],[Poly]), (CF, Maybe [CF])),
--     solvingInfoODE ~ BisectionInfo solvingInfoODESegment (solvingInfoODESegment, Maybe CF),
--     solvingInfoEvents ~ (CF, Maybe (HybridSystemUncertainState CF), EventInfo Poly)
--    )
--    =>
    TopLevelStrategy ->
    BasicStepType ->
    Bool
    ->
    HybridIVP Poly 
    -> 
    (Int, Int, Int, Int) 
    -> 
    IO ()
solveEventsPrintSteps 
        topLevelStrategy basicStepType
        shouldPlotSteps 
        ivp (maxdegParam, depthParam, minDepthParam, maxSplitSizeParam) =
    do
    putStrLn "---------------------------------------------------------"
    putStrLn "enclosure semantics based on (Konecny, Taha, Duracz 2012)" 
    putStrLn "---------------------------------------------------------"
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
    case topLevelStrategy of
        TopLevelBisect -> 
            do
            _ <- printStepsInfoBisect (1:: Int) bisectionInfo
            return ()
        TopLevelLocate -> 
            do
            _ <- printStepsInfoLocate (1:: Int) segmentsInfo
            return ()
    putStrLn "----------  step summary: -----------------------"
    putStrLn $ "number of atomic segments = " ++ 
        case topLevelStrategy of
            TopLevelBisect ->
                (show $ bisectionInfoCountLeafs bisectionInfo)
            TopLevelLocate ->
                (show $ length segmentsInfo)
    putStrLn $ "smallest segment size: " ++ (show smallestSegSize) 
        
    putStrLn "----------  result: -----------------------------"
    putStr $ showState ">>> " (tEnd, maybeEndState)
    case (maybeExactResult, maybeEndState) of
        (Just exactResult, Just resultOut) ->
            putStrLn $ "error = " ++ show (getErrorState exactResult resultOut)
        _ -> return ()
    putStrLn $ "event count = " ++ show eventCount
    putStrLn "-------------------------------------------------"

    case (shouldPlotSteps, topLevelStrategy) of
        (False, _) -> return ()
        (_, TopLevelBisect) -> plotHybIVPBisectionEnclosures effCf False (2^^(-8 :: Int) :: CF) ivp bisectionInfo
        (_, TopLevelLocate) -> plotHybIVPListEnclosures effCf (2^^(-12 :: Int) :: CF) ivp segmentsInfo
    return ()
--    return (maybeEndState, segmentsInfo)
--    return (maybeEndState, bisectionInfo)
    where
    maybeEndState = case topLevelStrategy of
        TopLevelBisect -> maybeEndStateBisect
        TopLevelLocate -> maybeEndStateLocate
    (maybeEndStateBisect, bisectionInfo) =
        solveHybridIVPBisect
            sizeLimits effCf substSplitSizeLimit
                delta m minStepSize maxStepSize splitImprovementThreshold
                    "t0" 
                        ivp
    (maybeEndStateLocate, segmentsInfo) =
        solveHybridIVPLocate
            sizeLimits effCf substSplitSizeLimit
                maxNodes
                delta m minStepSize maxStepSize splitImprovementThreshold
                    "t0" 
                        ivp
    -- parameters:
    delta = 1
    maxdeg = maxdegParam
    maxsize = 50
    m = 200
--    m = 20
    maxNodes = 100
    substSplitSizeLimit = maxSplitSizeParam -- 2^t0maxdeg
--    minStepSizeExp = -4 :: Int
    minStepSizeExp = - depthParam
    minStepSize = 2^^minStepSizeExp
    maxStepSizeExp = - minDepthParam
    maxStepSize = 2^^maxStepSizeExp
--        fst $ RefOrd.getEndpointsOut $ 10^^(-3::Int)
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
            err = snd $ RefOrd.getEndpointsOut $ wOut CF.<-> wIn
            wOut = CF.width valueOut     
            wIn = CF.width valueIn     
    
    eventCount = case topLevelStrategy of
        TopLevelBisect -> eventCountBisect
        TopLevelLocate -> eventCountLocate
    smallestSegSize = case topLevelStrategy of
        TopLevelBisect -> smallestSegSizeBisect  
        TopLevelLocate -> smallestSegSizeLocate  
    eventCountBisect =
        aux bisectionInfo
        where
        aux (BisectionNoSplit (_,_,modeEventInfoList)) 
            | null modeEventInfoList = error "solveEventsPrintSteps: BisectionNoSplit with empty modeEventInfoList" 
            | otherwise = foldl1 (CF.</\>) $ map (eventInfoCountEvents 0 effCf . snd) modeEventInfoList
        aux (BisectionSplit _ left Nothing) =
            aux left
        aux (BisectionSplit _ left (Just right)) =
            (aux left) CF.<+> (aux right)
    (smallestSegSizeBisect, _) =
        aux tStart (tEnd CF.<-> tStart) bisectionInfo
        where
        aux tPrev tSmallestSoFar (BisectionNoSplit (tNow,_,_)) =
            (CF.minOut tSmallestSoFar (tNow CF.<-> tPrev), tNow)
        aux tPrev tSmallestSoFar (BisectionSplit _ left Nothing) =
            aux tPrev tSmallestSoFar left
        aux tPrev tSmallestSoFar (BisectionSplit _ left (Just right)) =
            aux tPrevL tSmallestSoFarL right
            where
            (tSmallestSoFarL, tPrevL) =
                aux tPrev tSmallestSoFar left
    eventCountLocate =
        foldl (<+>) 0 $ map getSegmentEventCount segmentsInfo
        where
        getSegmentEventCount (_, Nothing, _) =  -- something failed on this segment 
            0 CF.</\> (plusInfinity 0)
        getSegmentEventCount (_, Just _, modeMap) = 
            foldl1 (CF.</\>) $ map getModeEventCount $ Map.elems modeMap
        getModeEventCount (_, _, Nothing) = 0 -- no events here
        getModeEventCount (_, _, Just (_, _, eventInfo)) = 
            eventInfoCountEvents 0 effCf eventInfo
    smallestSegSizeLocate =
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
    printStepsInfoBisect n (BisectionNoSplit (t, _maybeState, _modeEventInfoList)) =
        do
        putStrLn $ showStepInfo (n, t)
        return $ n + 1
    printStepsInfoBisect n (BisectionSplit _ left maybeRight) =
        do
        n2 <- printStepsInfoBisect n left
        case maybeRight of
            Just right -> printStepsInfoBisect n2 right
            Nothing -> return $ n2 + 1
    printStepsInfoLocate _ [] = return ()
    printStepsInfoLocate n ((t,_,_) : rest) =
        do
        putStrLn $ showStepInfo (n, t)
        printStepsInfoLocate (n+1) rest
        
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
--    showSegInfoBisect indent (t, maybeState, modeEventInfoList) =
--        maybeEventsCountS ++
--        indent ++ "mode(" ++ show t ++ ") ∊ " ++ modesS ++ "\n" ++
--        (unlines $ map (showComponent indent) $ zip componentNames valueSs)
--        ++ (unlines $ map showModeEventInfo modeEventInfoList)
--        where
--        maybeEventsCountS =
--            case modeEventInfoList of
--                [] -> ""
--                _ ->  indent ++ "events on this time segment: " ++ eventsS ++ "\n" 
--        eventsS =
--            show $
--            foldl1 (CF.</\>) $ map (eventInfoCountEvents 0 effCf . snd) modeEventInfoList 
--        showModeEventInfo (mode, eventInfo) =
--            indent ++ "events assuming mode at the start of segment = " ++ show mode ++ ":\n" ++
--            showEventInfo (indent ++ "  ") (show . fst) eventInfo
--        showComponent indent2 (name, valueS) =
--            indent2 ++ name ++ "("  ++ show t ++ ") ∊ " ++ valueS
--        (modesS, valueSs) =
--            case maybeState of
--                Just state ->
--                    (show modeSet, map showValue values)
--                    where
--                    (modeSet, values) = getHybridStateUnion effJoinCf state
--                _ ->
--                    ("<no result computed>", 
--                     replicate (length componentNames) "<no result computed>")
--        showValue valueOut =
--            show valueOut 
----            ++ "(err<=" ++ show err ++ ")"
----            ++ "; valueIn = " ++ show valueIn
----            where
----            err = snd $ RefOrd.getEndpointsOut $ wOut CF.<-> wIn
----            wOut = CF.width valueOut     
----            wIn = CF.width valueIn     
--    showSegInfoLocate indent (t, maybeState, modeSolvingInfoMap) =
--        maybeEventsCountS
--        ++ (showState indent (t, maybeState))
----        ++ (unlines $ map showModeEventInfo modeEventInfoList)
--        where
--        eventInfoList = map getEventInfo $ filter hasEventInfo $ Map.elems modeSolvingInfoMap
--        hasEventInfo (_, _, Nothing) = False
--        hasEventInfo _ = True
--        getEventInfo (_, _, Just eventInfo) = eventInfo
--        maybeEventsCountS 
--            | null eventInfoList = ""
--            | otherwise =
--                indent ++ "events on this time segment: " ++ eventsS ++ "\n" 
--        eventsS =
--            show $
--            foldl1 (CF.</\>) $ map (eventInfoCountEvents 0 effCf) eventInfoList 
----        showModeEventInfo (mode, eventInfo) =
----            indent ++ "events assuming mode at the start of segment = " ++ show mode ++ ":\n" ++
----            showEventInfo (indent ++ "  ") (show . fst) eventInfo

solveHybridIVPBisect ::
    (solvingInfo ~ (CF, Maybe (HybridSystemUncertainState CF), [(HybSysMode, EventInfo Poly)]))
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
     BisectionInfo solvingInfo (solvingInfo, Maybe CF)
    )
solveHybridIVPBisect 
        sizeLimits effCf substSplitSizeLimit
            delta m minStepSize maxStepSize splitImprovementThreshold 
                t0Var
                    hybivp
    =
    result
    where
    result =
        solveHybridIVP_UsingPicardAndEventTree_Bisect
            sizeLimits effPEval effCompose effEval effInteg effInclFn effAddFn effMultFn effAddFnDom effCf
                delta m t0Var minStepSize maxStepSize splitImprovementThreshold
                    hybivp

    effCompose = effIntPoly
    effEval = effIntPoly
    effPEval = effIntPoly
    effInteg = effCf
    effAddFn = effCf
    effMultFn = effCf
    effAddFnDom =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effInclFn = (effIntPoly, ())
    effIntPoly =
        (defaultIntPolyEffort sampleCf (1 + ivpArity) sizeLimits)
        {
            ipolyeff_cfRoundedRealEffort = effCf,
            ipolyeff_evalMaxSplitSize = Int1To100 20
        }
        where
        ivpArity = length $ hybsys_componentNames $ hybivp_system hybivp

solveHybridIVPLocate ::
    (
     solvingInfoODESegment ~ (Maybe ([Poly],[Poly]), (CF, Maybe [CF])),
     solvingInfoODE ~ BisectionInfo solvingInfoODESegment (solvingInfoODESegment, Maybe CF),
     solvingInfoEvents ~ (CF, Maybe (HybridSystemUncertainState CF), EventInfo Poly)
    )
    =>
    SizeLimits Poly -> 
    ArithInOut.RoundedRealEffortIndicator CF ->
    Int -> 
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
solveHybridIVPLocate
        sizeLimits effCf substSplitSizeLimit
            maxNodes
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
                maxNodes
                delta m t0Var minStepSize maxStepSize splitImprovementThreshold
                    hybivp

    effSizeLims = effCf
    effDeriv = effCf
    effAbsFn = ArithInOut.absDefaultEffort samplePoly
    effMinmaxFn = NumOrd.minmaxInOutDefaultEffort samplePoly

    effCompose = effIntPoly
    effEval = effIntPoly
    effPEval = effIntPoly
    effInteg = effCf
    effAddFn = effCf
    effMultFn = effCf

    effAddFnDom =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effMultFnDom =
        ArithInOut.mixedMultDefaultEffort samplePoly sampleCf
--        ArithInOut.fldEffortMult sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effDivFnInt =
        ArithInOut.mxfldEffortDiv sampleCf (0::Int) $ ArithInOut.rrEffortIntMixedField sampleCf effCf

    effInclFn = (effIntPoly, ())
    effIntPoly =
        (defaultIntPolyEffort sampleCf (1 + ivpArity) sizeLimits)
        {
            ipolyeff_cfRoundedRealEffort = effCf,
            ipolyeff_evalMaxSplitSize = Int1To100 20
        }
        where
        ivpArity = length $ hybsys_componentNames $ hybivp_system hybivp



makeSampleWithVarsDoms :: 
     Int -> Int -> [Var Poly] -> [CF] -> Poly
makeSampleWithVarsDoms maxdeg maxsize vars doms =
    newConstFn sizeLimits varDoms sampleCf
    where
    varDoms = zip vars doms 
    sizeLimits =
        IntPolySizeLimits
        {
            ipolylimits_cf_limits = defaultSizeLimits sampleCf,
            ipolylimits_maxdeg = maxdeg,
            ipolylimits_maxsize = maxsize
        }
     
    