{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Bisection
import Numeric.AERN.IVP.Solver.Picard.UncertainTime

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
--import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort


import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.ShowInternals

--import Data.List (intercalate)

import System.IO
import System.Environment
import System.Directory
import System.CPUTime

import Numeric.AERN.Misc.Debug (unsafePrint)
_ = unsafePrint -- stop the unused warning

--import qualified Data.Map as Map
--import qualified Data.List as List

type CF = CF.DI
type Poly = IntPoly String CF

main :: IO ()
main =
    do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case length args of
        2 -> writeCSV args
        7 -> runOnce args
        _ -> usage
        
usage :: IO ()
usage =
    do
    putStrLn "Usage A: simple-uv-ut <ivp name> <output file name>"
    putStrLn "Usage B: simple-uv-ut <ivp name> <maxDeg> <minStepSize> <True|False-print steps?> <t0maxDeg> <minT0StepSize> <maxSplitSize>"

ivpByName :: String -> ODEIVP Poly
ivpByName "ivpExpDecay-ev-ut" = ivpExpDecay_ut False     
ivpByName "ivpExpDecay-uv-ut" = ivpExpDecay_ut True     
ivpByName "ivpSpringMass-ev-ut" = ivpSpringMass_ut False     
ivpByName "ivpSpringMass-uv-ut" = ivpSpringMass_ut True     

ivpExpDecay_ut :: Bool -> ODEIVP Poly
ivpExpDecay_ut withInitialValueUncertainty =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = description,
            odeivp_field = \ [x] -> [neg x],
            odeivp_componentNames = ["x"],
            odeivp_tVar = "t",
            odeivp_tStart = -0.125,
            odeivp_t0End = 0.125, 
            odeivp_tEnd = 1,
            odeivp_makeInitialValueFnVec = makeIV,
            odeivp_maybeExactValuesAtTEnd = Just $
                case withInitialValueUncertainty of
                    True ->
                        [(0.750 * expMOnePlusEps) CF.</\> (1.25 * expMOneMinusEps)]
                    False ->
                        [(0.875 * expMOnePlusEps) CF.</\> (1.125 * expMOneMinusEps)]
        }
    expMOnePlusEps = 1 CF.<*>| (exp (-1.125) :: Double)
    expMOneMinusEps = 1 CF.<*>| (exp (-0.875) :: Double)
    description =
        "x' = -x; " 
        ++ show tStart ++ " < t_0 < " ++ show t0End
        ++ "; x(t_0) ∊ " ++ (show $ makeIV dummySizeLimits "t_0" tStart)
    tStart = odeivp_tStart ivp
    t0End = odeivp_t0End ivp
    componentNames = odeivp_componentNames ivp
    dummySizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms 10 10 [] []
    makeIV sizeLimits t0Var t0Dom =
        case withInitialValueUncertainty of
            True ->
                [((1 :: Int) |<+> t0VarFn) <+> (xUnitFn </>| (8::Int))]
            False ->
                [ (1 :: Int) |<+> t0VarFn]
        where
        t0VarFn = newProjectionFromSample sampleInitialValueFn t0Var
        xUnitFn = newProjectionFromSample sampleInitialValueFn "x"
        sampleInitialValueFn =
            makeSampleWithVarsDoms 
                maxdeg maxsize 
                (t0Var : componentNames) (t0Dom : componentUncertaintyDomains)
            where
            componentUncertaintyDomains =
                map snd $ zip componentNames $ repeat unitDom
            unitDom = (-1) CF.</\> 1 
        maxdeg = ipolycfg_maxdeg sizeLimits
        maxsize = ipolycfg_maxsize sizeLimits

ivpSpringMass_ut :: Bool -> ODEIVP Poly
ivpSpringMass_ut withInitialValueUncertainty =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = description,
            odeivp_field = \ [x,x'] -> [x',neg x],
            odeivp_componentNames = ["x","x'"],
            odeivp_tVar = "t",
            odeivp_tStart = -0.125,
            odeivp_t0End = 0.125,
--            odeivp_tEnd = 0.125,
            odeivp_tEnd = 1,
            odeivp_makeInitialValueFnVec = makeIV,
            odeivp_maybeExactValuesAtTEnd = Just $
                case withInitialValueUncertainty of
                    True ->
                        [
                            ((0.75 * cosTEndPlusEps) - 0.125 * sinTEndPlusEps)  
                            CF.</\>
                            ((1.25 * cosTEndMinusEps) + 0.125 * sinTEndMinusEps)
                        ,
                            foldl1 (CF.</\>) $
                                [ 
                                    (-(sinTEndPlusEps) - 0.125 * cosTEndPlusEps),  
                                    (-(sinTEndPlusEps) + 0.125 * cosTEndPlusEps),  
                                    (-0.75*(sinTEndPlusEps) - 0.125 * cosTEndPlusEps),  
                                    (-0.75*(sinTEndPlusEps) + 0.125 * cosTEndPlusEps),  
                                    (-(sinTEndMinusEps) + 0.125 * cosTEndMinusEps),
                                    (-(sinTEndMinusEps) - 0.125 * cosTEndMinusEps),
                                    (-1.25*(sinTEndMinusEps) + 0.125 * cosTEndMinusEps),
                                    (-1.25*(sinTEndMinusEps) - 0.125 * cosTEndMinusEps)
                                ]
                        ]
                    False ->
                        [
                            ((0.875 * cosTEndPlusEps))  
                            CF.</\>
                            ((1.125 * cosTEndMinusEps))
                        ,
                            (-(0.875 * sinTEndPlusEps))  
                            CF.</\>
                            (-(1.125 * sinTEndMinusEps))
                        ]
        }
    cosTEndPlusEps = 1 CF.<*>| (cos (tEndDbl + 0.125) :: Double)
    cosTEndMinusEps = 1 CF.<*>| (cos (tEndDbl - 0.125) :: Double)
    sinTEndPlusEps = 1 CF.<*>| (sin (tEndDbl + 0.125) :: Double)
    sinTEndMinusEps = 1 CF.<*>| (sin (tEndDbl - 0.125) :: Double)
    description =
        "x'' = -x; " 
        ++ show tStart ++ " < t_0 < " ++ show t0End
        ++ "; (x,x')(t_0) ∊ " ++ (show $ makeIV dummySizeLimits "t_0" tStart)
    tStart = odeivp_tStart ivp
    t0End = odeivp_t0End ivp
    tEndDbl :: Double
    (Just tEndDbl) = ArithUpDn.convertUpEff () $ odeivp_tEnd ivp
    componentNames = odeivp_componentNames ivp
    dummySizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms 10 10 [] []
    makeIV sizeLimits t0Var t0Dom =
        case withInitialValueUncertainty of
            True ->
                [
                    ((1 :: Int) |<+> t0VarFn) <+> (xUnitFn </>| (8::Int))
                ,
                    (xdUnitFn </>| (8::Int))
                ]
            False ->
                [
                    (1 :: Int) |<+> t0VarFn
                ,
                    (0 :: Int) |<*> t0VarFn
                ]
        where
        t0VarFn = newProjectionFromSample sampleInitialValueFn t0Var
        xUnitFn = newProjectionFromSample sampleInitialValueFn "x"
        xdUnitFn = newProjectionFromSample sampleInitialValueFn "x'"
        sampleInitialValueFn =
            makeSampleWithVarsDoms 
                maxdeg maxsize 
                (t0Var : componentNames) (t0Dom : componentUncertaintyDomains)
            where
            componentUncertaintyDomains =
                map snd $ zip componentNames $ repeat unitDom
            unitDom = (-1) CF.</\> 1 
        maxdeg = ipolycfg_maxdeg sizeLimits
        maxsize = ipolycfg_maxsize sizeLimits


runOnce :: [String] -> IO ()
runOnce [ivpName, maxDegS, depthS, shouldShowStepsS, t0MaxDegS, t0DepthS, maxSplitSizeS] =
    do
    let maxDeg = read maxDegS :: Int
    let depth = read depthS :: Int
    let t0MaxDeg = read t0MaxDegS :: Int
    let t0depth = read t0DepthS :: Int
    let maxSplitSize = read maxSplitSizeS :: Int
    let shouldShowSteps = read shouldShowStepsS :: Bool
    putStrLn "--------------------------------------------------"
    putStrLn "demo of solve-VT from (Konecny, Taha, Duracz 2012)"
    putStrLn "--------------------------------------------------"
    _ <- solveVTPrintSteps shouldShowSteps ivp (maxDeg, depth, t0MaxDeg, t0depth, maxSplitSize)
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
            maxDegree <- [0..20], depth <- [0..10]]
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
            solverResult <- solveVTPrintSteps False ivp (20, 0, maxDegree, depth, 4*maxDegree*maxDegree)
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
--        (_enclosureErrorBoundS, 
        enclosureErrorS =
            case maybeVec of
                Nothing -> show "no solution"
                Just vecOut ->
--                    (computeDiff vecOut vecIn, 
                        case maybeVecExact of
                            Just vecExact -> 
                                    computeDiff vecOut vecExact
                            _ -> show "exact solution not known"
--                            )
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

solveVTPrintSteps :: 
    (solvingInfo1 ~ (CF, Maybe [CF]),
     solvingInfo2 ~ BisectionInfo solvingInfo1 (solvingInfo1, Maybe CF),
     solvingInfo3 ~ (solvingInfo1, (solvingInfo1, Maybe solvingInfo2))
    )
    =>
    Bool
    ->
    ODEIVP Poly 
    -> 
    (Int, Int, Int, Int, Int) 
    -> 
    IO (Maybe [CF], BisectionInfo solvingInfo3 (solvingInfo3, Maybe CF))
solveVTPrintSteps shouldShowSteps ivp (maxdegParam, depthParam, t0MaxDegParam, t0depthParam, maxSplitSizeParam) =
    do
    putStrLn $ "solving: " ++ description
    putStrLn "-------------------------------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "t0maxdeg = " ++ show t0maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "m = " ++ show m
    putStrLn $ "substSplitSizeLimit = " ++ show substSplitSizeLimit
    putStrLn $ "minimum step size = 2^{" ++ show minStepSizeExp ++ "}"
    putStrLn $ "T0 minimum step size = 2^{" ++ show minT0StepSizeExp ++ "}"
    putStrLn $ "split improvement threshold = " ++ show splitImprovementThreshold
    case maybeExactResult of
        Just exactResult ->
            putStr $ showSegInfo1 "(almost) exact result = " (tEnd, Just exactResult)
        _ -> return ()
    putStrLn "----------  result: -----------------------------"
    putStr $ showSegInfo1 ">>> " (tEnd, endValues)
    case (maybeExactResult, endValues) of
        (Just exactResult, Just resultOut) ->
            putStrLn $ "error = " ++ show (getErrorVec exactResult resultOut)
        _ -> return ()
    case shouldShowSteps of
        True ->
            do
            putStrLn "----------  steps: ------------------------------"
            putStrLn $ showBisectionInfo showSegInfo3 showSplitReason3 "" bisectionInfo
        False -> return ()
    putStrLn "-------------------------------------------------"
    return (endValues, bisectionInfo)
    where
    (endValues, bisectionInfo) =
        solveIVPWithUncertainTime
            sizeLimits t0SizeLimits effCf substSplitSizeLimit
                delta m minStepSize minT0StepSize splitImprovementThreshold
                    "t0"
                        ivp
    -- parameters:
    delta = 1
    maxdeg = maxdegParam
    t0maxdeg = t0MaxDegParam
    maxsize = 500
    m = 20
    substSplitSizeLimit = maxSplitSizeParam -- 2^t0maxdeg
--    minStepSizeExp = -4 :: Int
    minStepSizeExp = - depthParam
    minStepSize = 2^^minStepSizeExp
    minT0StepSizeExp = - t0depthParam
    minT0StepSize = 2^^minT0StepSizeExp
    splitImprovementThreshold = 2^^(-50 :: Int)
    
    -- auxiliary:
    description = odeivp_description ivp
    tEnd = odeivp_tEnd ivp
    maybeExactResult = odeivp_maybeExactValuesAtTEnd ivp
    componentNames = odeivp_componentNames ivp

    sampleCf = 0 :: CF
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
    sizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms maxdeg maxsize [] []
    t0SizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms t0maxdeg maxsize [] []
            
    getErrorVec exactVec approxVec
        | not (refinesVec exactVec approxVec) = -- && refinesVec vecIn vecExact) ->
            error $ 
                "enclosure error:"
                ++ "\n approxVec = " ++ show approxVec
                ++ "\n exactVec = " ++ show exactVec
        | otherwise =
            map getError $ zip exactVec approxVec
        where
        getError (valueIn, valueOut) =
            err
            where
            err = snd $ RefOrd.getEndpointsOutWithDefaultEffort $ wOut CF.<-> wIn
            wOut = CF.width valueOut     
            wIn = CF.width valueIn     
    showSegInfo1 indent (t, maybeValues) =
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
    showSegInfo2 indent bisectionInfo2 =
        showBisectionInfo showSegInfo1 showSplitReason2 indent bisectionInfo2
    showSegInfo3 indent (segInfoT, (segInfoT0,  Just segInfo2Out)) =
        indent ++ "at t0End:\n" ++ showSegInfo1 (indent ++ "  ") segInfoT0
        ++ indent ++ "at tEnd:\n" ++ showSegInfo1 (indent ++ "  ") segInfoT
        ++ showSegInfo2 (indent ++ ":   ") segInfo2Out
    showSplitReason2 indent (segInfo, (Just improvement)) =
        showSegInfo1 indent segInfo ++ 
        indent ++ "; but splitting improves by " ++ show improvement ++ ":"
    showSplitReason2 indent (segInfo, Nothing) =
        showSegInfo1 indent segInfo ++ 
        indent ++ " - thus splitting"
    showSplitReason3 indent ((segInfoT, (_segInfoT0, _)), (Just improvement)) =
        showSegInfo1 indent segInfoT ++ 
        indent ++ "; but splitting improves by " ++ show improvement ++ ":"
    showSplitReason3 indent ((segInfoT, (_segInfoT0, _)), Nothing) =
        showSegInfo1 indent segInfoT ++ 
        indent ++ " - thus splitting"

solveIVPWithUncertainTime ::
    (solvingInfo1 ~ (CF, Maybe [CF]),
     solvingInfo2 ~ BisectionInfo solvingInfo1 (solvingInfo1, Maybe CF),
     solvingInfo3 ~ (solvingInfo1, (solvingInfo1, Maybe solvingInfo2))
    )
    =>
    SizeLimits Poly -> 
    SizeLimits Poly -> 
    ArithInOut.RoundedRealEffortIndicator CF ->
    Int -> 
    CF ->
    Int ->
    CF ->
    CF ->
    CF ->
    Var Poly ->
    ODEIVP Poly 
    ->
    (
     Maybe [CF]
    ,
     BisectionInfo solvingInfo3 (solvingInfo3, Maybe CF)
    )
solveIVPWithUncertainTime 
        sizeLimits t0SizeLimits effCf substSplitSizeLimit
            delta m minStepSize minT0StepSize splitImprovementThreshold 
                t0Var
                    odeivp
    =
    result
    where
    result =
        solveODEIVPUncertainValueUncertainTime_UsingPicard_Bisect
            sizeLimits t0SizeLimits 
            effSizeLims effCompose effEval effInteg effDeriv effInclFn 
            effAddFn effMultFn effAbsFn effMinmaxFn 
            effDivFnInt effAddFnDom effMultFnDom effCf
--            effSizeLims effCompose effEval effInteg effInclFn effAddFn effMultFn effAddFnDom effCf
                delta m minStepSize minT0StepSize splitImprovementThreshold
                    t0Var
                        odeivp

    sampleCf = delta 
    
    effSizeLims = effCf
    effCompose = (effCf, Int1To10 substSplitSizeLimit)
    effEval = (effCf, Int1To10 substSplitSizeLimit)
    effInteg = effCf
    effDeriv = effCf
    effAddFn = effCf
    effMultFn = effCf
    effInclFn = ((Int1To1000 0, (effCf, Int1To10 20)), ())
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
     
--putEnclosureEndpoints :: 
--    (Show f, Show (Domain f),
--     RefOrd.IntervalLike (Domain f),
--     CanEvaluate f) 
--    =>
--    Var f -> Domain f -> [[f]] -> IO ()
--putEnclosureEndpoints tVar tEnd fnVectors =
--    mapM_ putEndpt $ zip [1..] fnVectors
--    where
--    putEndpt (n, fns) =
--        do
--        putStrLn $ "---------- enclosure " ++ show (n :: Int) ++ ":"
--        putStrLn $ "at end time: " ++ (show $ evalAtEndTimeVec tVar tEnd fns)
----        putStrLn $ "in full: " ++ (show fns)
--        
--        
--putVals ::
--    (Show (Domain f), 
--     CanEvaluate f, 
--     RefOrd.IntervalLike (Domain f)) 
--    =>
--    Var f -> Domain f -> [f] 
--    -> 
--    IO ()
--putVals tVar tEnd vector =
--    do
--    putStrLn $ "at end time: " ++ (show $ evalAtEndTimeVec tVar tEnd vector)
        
        