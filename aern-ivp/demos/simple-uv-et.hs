{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Splitting
import Numeric.AERN.IVP.Solver.Picard.UncertainValue

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain

import Numeric.AERN.RealArithmetic.Basis.Double ()
import qualified Numeric.AERN.DoubleBasis.Interval as CF
--import Numeric.AERN.RealArithmetic.Basis.MPFR
--import qualified Numeric.AERN.MPFRBasis.Interval as MI

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder as RefOrd

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
    args <- getArgs
    case length args of
        2 -> writeCSV args
        4 -> runOnce args
        _ -> usage
        
usage :: IO ()
usage =
    do
    putStrLn "Usage A: simple-uv-et <ivp name> <output file name>"
    putStrLn "Usage B: simple-uv-et <ivp name> <True|False-print steps?> <maxDeg> <minStepSize>"

ivpByName :: String -> ODEIVP Poly
ivpByName "ivpExpDecay-ev-et" = ivpExpDecay_ev_et     
ivpByName "ivpExpDecay-uv-et" = ivpExpDecay_uv_et     
ivpByName "ivpSpringMass-ev-et" = ivpSpringMass_ev_et
ivpByName "ivpSpringMass-uv-et" = ivpSpringMass_uv_et     
ivpByName "ivpSpringMassAir-ev-et" = ivpSpringMassAir_ev_et     

ivpExpDecay_ev_et :: ODEIVP Poly
ivpExpDecay_ev_et =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = "x' = -x; x(" ++ show tStart ++ ") = " ++ show initialValues,
            odeivp_field = \ [x] -> [neg x],
            odeivp_componentNames = ["x"],
            odeivp_tVar = "t",
            odeivp_tStart = 0,
            odeivp_t0End = 0, 
            odeivp_tEnd = 1,
            odeivp_makeInitialValueFnVec = makeIV,
            odeivp_maybeExactValuesAtTEnd = Just [1 CF.<*>| expMOne]
        }
    initialValues = [1]
    expMOne = exp (-1) :: Double
    makeIV =
        makeFnVecFromInitialValues componentNames initialValues
    componentNames = odeivp_componentNames ivp
    tStart = odeivp_tStart ivp

ivpExpDecay_uv_et :: ODEIVP Poly
ivpExpDecay_uv_et =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = "x' = -x; x(" ++ show tStart ++ ") ∊ " ++ show initialValues,
            odeivp_field = \ [x] -> [neg x],
            odeivp_componentNames = ["x"],
            odeivp_tVar = "t",
            odeivp_tStart = 0,
            odeivp_t0End = 0, 
            odeivp_tEnd = 1,
            odeivp_makeInitialValueFnVec = makeIV,
            odeivp_maybeExactValuesAtTEnd = Just [(0.875 CF.<*>| expMOne) CF.</\> (1.125 CF.<*>| expMOne)]
        }
    initialValues = [(1 CF.<-> 0.125 ) CF.</\> (1 CF.<+> 0.125)]
    expMOne = exp (-1) :: Double
    makeIV =
        makeFnVecFromInitialValues componentNames initialValues
    componentNames = odeivp_componentNames ivp
    tStart = odeivp_tStart ivp


ivpSpringMass_ev_et :: ODEIVP Poly
ivpSpringMass_ev_et =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = "x'' = -x; (x,x')(" ++ show tStart ++ ") = " ++ show initialValues,
            odeivp_field = \ [x,x'] -> [x',neg x],
            odeivp_componentNames = ["x", "x'"],
            odeivp_tVar = "t",
            odeivp_tStart = 0,
            odeivp_t0End = 0,
            odeivp_tEnd = 1,
            odeivp_makeInitialValueFnVec = makeIV,
            odeivp_maybeExactValuesAtTEnd = Just [cosOne, -sinOne]
        }
    initialValues = [1,0]
    cosOne = 1 CF.<*>| (cos 1 :: Double)
    sinOne = 1 CF.<*>| (sin 1 :: Double)
    makeIV =
        makeFnVecFromInitialValues componentNames initialValues
    componentNames = odeivp_componentNames ivp
    tStart = odeivp_tStart ivp

ivpSpringMass_uv_et :: ODEIVP Poly
ivpSpringMass_uv_et =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = "x'' = -x; (x,x')(" ++ show tStart ++ ") ∊ " ++ show initialValues,
            odeivp_field = \ [x,x'] -> [x',neg x],
            odeivp_componentNames = ["x", "x'"],
            odeivp_tVar = "t",
            odeivp_tStart = 0,
            odeivp_t0End = 0,
            odeivp_tEnd = 1,
            odeivp_makeInitialValueFnVec = makeIV,
            odeivp_maybeExactValuesAtTEnd = Just $ 
                [
                    (0.875 * cosOne - 0.125 * sinOne) CF.</\> (1.125 * cosOne + 0.125 * sinOne)  
                , 
                    (-1.125 * sinOne - 0.125 * cosOne) CF.</\> (-0.875 * sinOne + 0.125 * cosOne)
                ]
        }
    initialValues = 
        [
            (1 CF.<-> 0.125 ) CF.</\> (1 CF.<+> 0.125)
        ,
            (0 CF.<-> 0.125 ) CF.</\> (0 CF.<+> 0.125)
        ]
    cosOne = 1 CF.<*>| (cos 1 :: Double)
    sinOne = 1 CF.<*>| (sin 1 :: Double)
    makeIV =
        makeFnVecFromInitialValues componentNames initialValues
    componentNames = odeivp_componentNames ivp
    tStart = odeivp_tStart ivp

ivpSpringMassAir_ev_et :: ODEIVP Poly
ivpSpringMassAir_ev_et =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = "x'' = -x - x'*|x'|; (x,x')(" ++ show tStart ++ ") = " ++ show initialValues,
            odeivp_field = \ [x,x'] -> [x',neg (x <+> (x' <*> (myAbs x')))],
            odeivp_componentNames = ["x", "x'"],
            odeivp_tVar = "t",
            odeivp_tStart = 0,
            odeivp_t0End = 0,
            odeivp_tEnd = 1,
            odeivp_makeInitialValueFnVec = makeIV,
            odeivp_maybeExactValuesAtTEnd = Nothing -- Just [cosOne, -sinOne]
        }
    initialValues = [1,0]
    myAbs fn =
        ArithInOut.absOutEff effAbsFn fn
        where
        effAbsFn = ArithInOut.absDefaultEffort fn
        
    makeIV =
        makeFnVecFromInitialValues componentNames initialValues
    componentNames = odeivp_componentNames ivp
    tStart = odeivp_tStart ivp



runOnce :: [String] -> IO ()
runOnce [ivpName, shouldShowStepsS, maxDegS, depthS] =
    do
    let shouldShowSteps = read shouldShowStepsS :: Bool
    let maxDeg = read maxDegS :: Int
    let depth = read depthS :: Int
    putStrLn "--------------------------------------------------"
    putStrLn "demo of solve-Vt from (Konecny, Taha, Duracz 2012)"
    putStrLn "--------------------------------------------------"
    _ <- solveVtPrintSteps shouldShowSteps ivp (maxDeg, depth)
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
            maxDegree <- [0..15], depth <- [0..10]]
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
            solverResult <- solveVtPrintSteps False ivp (maxDegree, depth)
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
        (_enclosureErrorBoundS, enclosureErrorS) =
            case maybeVec of
                Nothing -> (show "no solution", show "no solution")
                Just (vecOut, vecIn) ->
                    (computeMaxDiff vecOut vecIn, 
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
                            _ -> show "exact solution not known")
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
    (solvingInfo ~ (CF, Maybe ([CF],[CF])))
    => 
    Bool
    ->
    ODEIVP Poly 
    -> 
    (Int, Int) 
    -> 
    IO 
    (
     Maybe ([CF],[CF])
    ,
        (
            SplittingInfo solvingInfo (solvingInfo, Maybe CF)
        ,
            SplittingInfo solvingInfo (solvingInfo, Maybe CF)
        )
    )
solveVtPrintSteps shouldShowSteps ivp (maxdegParam, depthParam) =
    do
    putStrLn $ "solving: " ++ description
    putStrLn "----------  parameters: -------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "m = " ++ show m
    putStrLn $ "substSplitSizeLimit = " ++ show substSplitSizeLimit
    putStrLn $ "minimum step size = 2^{" ++ show minStepSizeExp ++ "}"
    putStrLn $ "split improvement threshold = " ++ show splitImprovementThreshold
    case maybeExactResult of
        Just exactResult ->
            putStr $ showSegInfo "(almost) exact result = " (tEnd, Just (exactResult, exactResult))
        _ -> return ()
    putStrLn "----------  result: -----------------------------"
    putStr $ showSegInfo ">>> " (tEnd, endValues)
    case shouldShowSteps of
        True ->
            do
            putStrLn "----------  outwards-rounded computation steps: -----------------"
            putStrLn $ showSplittingInfo showSegInfo showSplitReason "" splittingInfoOut
            putStrLn "----------  inwards-rounded computation steps: -----------------"
            putStrLn $ showSplittingInfo showSegInfo showSplitReason "" splittingInfoIn
        False -> return ()
    putStrLn "-------------------------------------------------"
    return (endValues, splittingInfo)
    where
    -- solver call:
    (endValues, splittingInfo@(splittingInfoOut, splittingInfoIn)) =
        solveIVPWithUncertainValue
            sizeLimits effCf substSplitSizeLimit delta m 
                minStepSize splitImprovementThreshold
                    ivp
    -- parameters:
    delta = 1
    maxdeg = maxdegParam
    maxsize = 100
    m = 20
    substSplitSizeLimit = 100
--    minStepSizeExp = -4 :: Int
    minStepSizeExp = - depthParam
    minStepSize = 2^^minStepSizeExp
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
            
    showSegInfo indent (t, maybeValues) =
        unlines $ map showComponent $ zip componentNames valueSs
        where
        showComponent (name, valueS) =
            indent ++ name ++ "("  ++ show t ++ ") ∊ " ++ valueS
--        showVec [e] = e
--        showVec list = "(" ++ (intercalate "," list) ++ ")"
        valueSs =
            case maybeValues of
                Just (valuesOut, valuesIn) -> map showValue $ zip valuesOut valuesIn
                _ -> replicate (length componentNames) "<no result computed>"
        showValue (valueOut, _valueIn) =
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
    (solvingInfo ~ (CF, Maybe ([CF],[CF])))
    =>
    SizeLimits Poly -> 
    ArithInOut.RoundedRealEffortIndicator CF -> 
    Int -> 
    CF -> 
    Int -> 
    CF -> 
    CF -> 
    ODEIVP Poly
    -> 
    (
     Maybe ([CF],[CF])
    ,
        (
            SplittingInfo solvingInfo (solvingInfo, Maybe CF)
        ,
            SplittingInfo solvingInfo (solvingInfo, Maybe CF)
        )
    )
solveIVPWithUncertainValue
        sizeLimits effCf substSplitSizeLimit
            delta m minStepSize splitImprovementThreshold
                odeivp
    =
    solveUncertainValueExactTimeSplit
        sizeLimits effCompose effEval effInteg effInclFn effAddFn effAddFnDom effCf
            delta m minStepSize splitImprovementThreshold
                odeivp 
    where

--    substituteInitialValueUncertainty fn =
--        pEvalAtPointOutEff effEval initValDomBox fn
--        where
--        initValDomBox =
--            fromList $ zip componentNames initialValues
--        
    sampleCf = delta
    
    effCompose = (effCf, Int1To10 substSplitSizeLimit)
    effEval = (effCf, Int1To10 substSplitSizeLimit)
    effInteg = effCf
    effAddFn = effCf
    effAddFnDom =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effInclFn = ((Int1To1000 0, (effCf, Int1To10 100)), ())

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
