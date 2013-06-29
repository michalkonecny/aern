{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.AERN.IVP.Examples.ODE.Simple

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Bisection
import Numeric.AERN.IVP.Solver.Picard.UncertainValue
import Numeric.AERN.IVP.Plot.UsingFnView (plotODEIVPBisectionEnclosures)

import Numeric.AERN.IVP.Solver.ShrinkWrap -- only for testing

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.Poly.IntPoly
import Numeric.AERN.Poly.IntPoly.Plot ()

import Numeric.AERN.RealArithmetic.Basis.Double ()
import qualified Numeric.AERN.DoubleBasis.Interval as CF
--import Numeric.AERN.RealArithmetic.Basis.MPFR
--import qualified Numeric.AERN.MPFRBasis.Interval as MI

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding (dblToReal)
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators
--import Numeric.AERN.RealArithmetic.ExactOps

--import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.Operators

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.Operators

import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.ShowInternals

--import Data.List (intercalate)

import System.IO
import System.Environment
import System.Directory
import System.CPUTime

--import qualified Control.Concurrent as Concurrent

import Numeric.AERN.Misc.Debug (unsafePrint)
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
    runWithArgs args
        
usage :: IO ()
usage =
    do
    putStrLn "Usage A: simple-uv-et <ivp name> <output file name> <True|False-should wrap?>"
    putStrLn "Usage B: simple-uv-et <ivp name> <True|False-plot enclosures?> <True|False-should wrap?> <maxDeg> <minStepSize> <maxStepSize> [<tEnd>]"

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
                    odeivp_tEnd = dblToReal sampleCf $ read tEndS,
                    odeivp_maybeExactValuesAtTEnd = Nothing
                }
    ivp = ivpByNameReportError ivpName samplePoly
    shouldWrap = read shouldWrapS :: Bool
    shouldPlotSteps = read shouldPlotStepsS :: Bool
    maxDeg = read maxDegS :: Int
    maxDepth = read maxDepthS :: Int
    minDepth = read minDepthS :: Int
runWithArgs [ivpName, outputFileName, shouldWrapS] =
    writeCSV ivp outputFileName shouldWrap
    where
    shouldWrap = read shouldWrapS :: Bool
    ivp = ivpByNameReportError ivpName samplePoly
runWithArgs _ = usage
    
writeCSV :: ODEIVP Poly -> FilePath -> Bool -> IO ()
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
                        snd $ RefOrd.getEndpointsOut $ 
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
    (solvingInfo ~ (Maybe ([Poly],[Poly]), (CF, Maybe [CF])))
    => 
    Bool
    ->
    Bool
    ->
    ODEIVP Poly 
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
        True -> plotODEIVPBisectionEnclosures effCf (2^^(-8 :: Int) :: CF) ivp bisectionInfoOut
    return (endValues, bisectionInfoOut)
    where
    shouldShowSteps = False
    -- solver call:
    (endValues, bisectionInfoOut) =
        solveIVPWithUncertainValue shouldWrap
            sizeLimits effCf substSplitSizeLimit delta m 
                minStepSize maxStepSize splitImprovementThreshold
                    ivp
    -- parameters:
    delta = 1
    maxdeg = maxdegParam
    maxsize = 1000
    m = 100
    substSplitSizeLimit = 100
--    minStepSizeExp = -4 :: Int
    minStepSizeExp = - minDepthParam
    maxStepSizeExp = - maxDepthParam
    minStepSize = 2^^minStepSizeExp
    maxStepSize = 2^^maxStepSizeExp
    splitImprovementThreshold = 2^^(-50 :: Int)
    
    -- auxiliary:
    description = odeivp_description ivp
    tStart = odeivp_tStart ivp
    tEnd = odeivp_tEnd ivp
    maybeExactResult = odeivp_maybeExactValuesAtTEnd ivp
    componentNames = odeivp_componentNames ivp
    
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
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
--            err = snd $ RefOrd.getEndpointsOut $ wOut CF.<-> wIn
--            wOut = CF.width valueOut     
--            wIn = CF.width valueIn     
    showSplitReason indent (segInfo, (Just improvement)) =
        showSegInfo indent segInfo ++ 
        "; but splitting improves by " ++ show improvement ++ ":"
    showSplitReason indent (segInfo, Nothing) =
        showSegInfo indent segInfo ++ 
        " - thus splitting"


solveIVPWithUncertainValue ::
    (solvingInfo ~ (Maybe ([Poly],[Poly]), (CF, Maybe [CF])))
    =>
    Bool ->
    SizeLimits Poly -> 
    ArithInOut.RoundedRealEffortIndicator CF -> 
    Int -> 
    CF -> 
    Int -> 
    CF -> 
    CF -> 
    CF -> 
    ODEIVP Poly
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
            sizeLimits effCf substSplitSizeLimit
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
    effSizeLims = effCf
    effCompose = (effCf, Int1To10 substSplitSizeLimit)
    effEval = (effCf, Int1To10 substSplitSizeLimit)
    effInteg = effCf
    effDeriv = effCf
    effAddFn = effCf
    effMultFn = effCf
    effInclFn = ((Int1To1000 0, (effCf, Int1To10 100)), ())
    effAbsFn = (effMinmaxFn, effAbsCf)
    effAbsCf = ArithInOut.rrEffortAbs sampleCf effCf
    effMinmaxFn = error "effMinmaxUpDnFn undefined"
    effAddFnDom = effAddCf
    effMultFnDom = (((), ()), (), ((), (), ()))
    effDivFnInt =
        ArithInOut.mxfldEffortDiv sampleCf (0::Int) $ ArithInOut.rrEffortIntMixedField sampleCf effCf
    effAddCf =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf

makeSampleWithVarsDoms :: 
     Int -> Int -> [Var Poly] -> [CF] -> Poly
makeSampleWithVarsDoms maxdeg maxsize vars doms =
    newConstFn cfg dombox sampleCf
    where
    domsLE = 
        map (fst . RefOrd.getEndpointsOut) doms
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



testShrinkWrap :: IO ()
testShrinkWrap =
    do
    mapM_ putStrLn $ map show $ getSides result
    return ()
    where
    getSides [x,y] =
        [(xSL,ySL),(xTL,yTL),(xSR,ySR),(xTR,yTR)]
        where
        xSL = pEvalAtPointOutEff effComp sL x
        xSR = pEvalAtPointOutEff effComp sR x
        xTL = pEvalAtPointOutEff effComp tL x
        xTR = pEvalAtPointOutEff effComp tR x
        ySL = pEvalAtPointOutEff effComp sL y
        ySR = pEvalAtPointOutEff effComp sR y
        yTL = pEvalAtPointOutEff effComp tL y
        yTR = pEvalAtPointOutEff effComp tR y
        sL = fromList [("s", -1)]
        sR = fromList [("s", 1)]
        tL = fromList [("t", -1)]
        tR = fromList [("t", 1)]
    
    Just result =
        shrinkWrap 
            effComp effEval effDeriv effAddFn effAbsFn effMinmaxFn 
                effDivFnInt effAddPolyCf effMultPolyCf effCf 
                    [xUsingTS, yUsingTS]
    xUsingTS = s <*> tPlus2 <+> pmeps -- s(t+2) +- eps
    yUsingTS = (c1 <-> s <*> s) <*> tPlus2 <+> pmeps -- (1-s^2)(t+2) +- eps
    tPlus2 = (c1 <+> c1 <+> t)
    t = newProjectionFromSample c1 "t"
    s = (0.5 :: CF) |<*> newProjectionFromSample c1 "s"
    pmeps = newConstFnFromSample c1 $ ((-eps) CF.</\> eps)
        where
        eps = 0.125
    c1 :: Poly
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
    effEval = (effCf, Int1To10 10)
    effComp = (effCf, Int1To10 10)
    effDeriv = effCf
    effAddFn = effCf
    effAbsFn = (effMinmaxFn, effAbsCf)
    effAbsCf = ArithInOut.rrEffortAbs sampleCf effCf
    effMinmaxFn = minmaxInOutDefaultEffortIntPolyWithBezierDegree 4 s
    effAddPolyCf = effAddCf
    effMultPolyCf = (((), ()), (), ((), (), ()))
    effDivFnInt =
        ArithInOut.mxfldEffortDiv sampleCf (0::Int) $ ArithInOut.rrEffortIntMixedField sampleCf effCf
    effAddCf =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
        
