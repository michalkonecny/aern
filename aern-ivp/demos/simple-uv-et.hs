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
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.ShowInternals

import Data.List (intercalate)

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
main = mainCmdLine ivpExpDecay_ev_et
--main = mainCSV ivpExpDecay_ev_et
--main = mainCSV ivpExpDecay_uv_et
--main = mainCSV ivpSpringMass_ev_et
--main = mainCSV ivpSpringMass_uv_et

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
            odeivp_description = "x'' = -x; (x,x')(" ++ show tStart ++ ") = " ++ show initialValues,
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

mainCmdLine :: ODEIVP Poly -> IO ()
mainCmdLine ivp =
    do
    args <- getArgs
    let [maxDegS, depthS] = args
    let maxDeg = read maxDegS :: Int
    let depth = read depthS :: Int
    putStrLn "--------------------------------------------------"
    putStrLn "demo of solve-Vt from (Konecny, Taha, Duracz 2012)"
    putStrLn "--------------------------------------------------"
    _ <- solveVtPrintSteps True ivp (maxDeg, depth)
    return ()

mainCSV :: ODEIVP Poly -> IO ()
mainCSV ivp =
    do
    args <- getArgs
    let [outputFileName] = args
    isClash <- doesFileExist outputFileName
    case isClash of
        True -> putStrLn $ "file " ++ outputFileName ++ " exists"
        False ->
            do
            results <- mapM runSolverMeasureTimeMS paramCombinations 
            writeFile outputFileName $ unlines $ csvLines results
    where
    paramCombinations = 
        [(maxDegree, depth) | 
            maxDegree <- [0..20], depth <- [0..10]]
--            maxDegree <- [0..10], depth <- [0..5]]
    runSolverMeasureTimeMS (maxDegree, depth) =
        do
        resultsAndTimes <- mapM solveAndMeasure ([1..10] :: [Int])
        let ((result, _) : _)  = resultsAndTimes
        let averageTime = average $ map snd resultsAndTimes
        return (result, averageTime)
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
        
    csvLines results =
        [
            "ivp: " ++ description
        ,
            "polynomial degree, min step size (2^(-n)), time (microseconds), error upper bound at t=1, error at t = 1"
        ]
        ++
        (map makeCSVLine $ zip results paramCombinations) 
    description = odeivp_description ivp
    maybeVecExact = odeivp_maybeExactValuesAtTEnd ivp
    makeCSVLine (((maybeVec, _), execTimeMS), (maxDegree, depth)) =
        show maxDegree ++ "," 
        ++ show depth ++ ","
        ++ show execTimeMS ++ ","
        ++ enclosureErrorBoundS ++ ","
        ++ enclosureErrorS
        where
        (enclosureErrorBoundS, enclosureErrorS) =
            case maybeVec of
                Nothing -> (show "no solution", show "no solution")
                Just (vecOut, vecIn) ->
                    (computeMaxDiff vecOut vecIn, 
                        case maybeVecExact of
                            Just vecExact -> computeMaxDiff vecOut vecExact
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

   
solveVtPrintSteps ::
    (solvingInfo ~ (CF, Maybe ([CF],[CF])))
    => 
    Bool
    ->
    ODEIVP Poly 
    -> 
    (Int, Int) 
    -> 
    IO (Maybe ([CF],[CF]), SplittingInfo solvingInfo (solvingInfo, Maybe CF))
solveVtPrintSteps shouldShowSteps ivp (maxdegParam, depthParam) =
    do
    putStrLn $ "solving: " ++ description
    putStrLn "----------  parameters: -------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "m = " ++ show m
    putStrLn $ "minimum step size = 2^{" ++ show minStepSizeExp ++ "}"
    putStrLn $ "split improvement threshold = " ++ show splitImprovementThreshold
    putStrLn "----------  result: -----------------------------"
    putStrLn $ showSegInfo (tEnd, endValues)
    case shouldShowSteps of
        True ->
            do
            putStrLn "----------  steps: ------------------------------"
            putStrLn $ showSplittingInfo showSegInfo showSplitReason splittingInfo
        False -> return ()
    putStrLn "-------------------------------------------------"
    return (endValues, splittingInfo)
    where
    -- solver call:
    (endValues, splittingInfo) =
        solveIVPWithUncertainValue
            sizeLimits effCf delta m 
                minStepSize splitImprovementThreshold
                    ivp
    -- parameters:
    delta = 1
    maxdeg = maxdegParam
    maxsize = 100
    m = 20
--    minStepSizeExp = -4 :: Int
    minStepSizeExp = - depthParam
    minStepSize = 2^^minStepSizeExp
    splitImprovementThreshold = 2^^(-50 :: Int)
    
    -- auxiliary:
    description = odeivp_description ivp
    tEnd = odeivp_tEnd ivp
    componentNames = odeivp_componentNames ivp
    
    sampleCf = 0 :: CF
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
    sizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms maxdeg maxsize [] []
            
    showSegInfo (t, maybeValues) =
        showVec componentNames ++ "(" ++ show t ++ ") ∊ " ++ valuesS 
        where
        showVec [e] = e
        showVec list = "(" ++ (intercalate "," list) ++ ")"
        valuesS =
            case maybeValues of
                Just (valuesOut, valuesIn) -> showVec $ map showValue $ zip valuesOut valuesIn
                _ -> "<no result computed>"
        showValue (valueOut, valueIn) =
            show valueOut ++ "(err<=" ++ show err ++ ")"
            where
            err = snd $ RefOrd.getEndpointsOutWithDefaultEffort $ wOut CF.<-> wIn
            wOut = CF.width valueOut     
            wIn = CF.width valueIn     
    showSplitReason (segInfo, (Just improvement)) =
        showSegInfo segInfo ++ 
        "; but splitting improves by " ++ show improvement ++ ":"
    showSplitReason (segInfo, Nothing) =
        showSegInfo segInfo ++ 
        " - thus splitting"


solveIVPWithUncertainValue ::
    (solvingInfo ~ (CF, Maybe ([CF],[CF])))
    =>
    SizeLimits Poly 
    -> ArithInOut.RoundedRealEffortIndicator CF
    -> CF
    -> Int
    -> CF
    -> CF
    -> ODEIVP Poly
    -> 
    (
     Maybe ([CF],[CF])
    ,
     SplittingInfo solvingInfo (solvingInfo, Maybe CF)
    )
solveIVPWithUncertainValue
        sizeLimits effCf 
            delta m minStepSize splitImprovementThreshold
                odeivp
    =
    solveUncertainValueExactTimeSplit
        sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effCf
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
    
    effCompose = effCf
    effInteg = effCf
    effAddFn = effCf
    effAddFnDom =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effInclFn = ((Int1To1000 0, effCf), ())

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
