{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Splitting
import Numeric.AERN.IVP.Solver.Picard.UncertainValue

import Numeric.AERN.IVP.Solver.ShrinkWrap -- only for testing

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

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
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case length args of
        0 -> testShrinkWrap
        3 -> writeCSV args
        5 -> runOnce args
        _ -> usage
        
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
            effComp effEval effDeriv effAddFn effAbsFn effMinmaxUpDnFn 
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
    sampleCf = 0 :: CF
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
    effEval = (effCf, Int1To10 10)
    effComp = (effCf, Int1To10 10)
    effDeriv = effCf
    effAddFn = effCf
    effAbsFn = (effMinmaxUpDnFn, effAbsCf)
    effAbsCf = ArithInOut.rrEffortAbs sampleCf effCf
    effMinmaxUpDnFn = minmaxUpDnDefaultEffortIntPolyWithBezierDegree 4 s
    effAddPolyCf = effAddCf
    effMultPolyCf = (((), ()), (), ((), (), ()))
    effDivFnInt =
        ArithInOut.mxfldEffortDiv sampleCf (0::Int) $ ArithInOut.rrEffortIntMixedField sampleCf effCf
    effAddCf =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
        
usage :: IO ()
usage =
    do
    putStrLn "Usage A: simple-uv-et <ivp name> <output file name> <True|False-should wrap?>"
    putStrLn "Usage B: simple-uv-et <ivp name> <True|False-print steps?> <True|False-should wrap?> <maxDeg> <minStepSize>"

ivpByName :: String -> ODEIVP Poly
ivpByName "ivpExpDecay-ev-et" = ivpExpDecay_ev_et     
ivpByName "ivpExpDecay-uv-et" = ivpExpDecay_uv_et     
ivpByName "ivpSpringMass-ev-et" = ivpSpringMass_ev_et
ivpByName "ivpSpringMass-uv-et" = ivpSpringMass_uv_et     
ivpByName "ivpSpringMassAir-ev-et" = ivpSpringMassAir_ev_et
ivpByName "ivpFallAir-ishii" = ivpFallAir_ishii
ivpByName "ivpLorenz-ishii" = ivpLorenz_ishii
ivpByName "ivpRoessler" = ivpRoessler

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

ivpFallAir_ishii :: ODEIVP Poly
ivpFallAir_ishii =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = "x'' = -9.8+(x'*x')/1000; (x,x')(" ++ show tStart ++ ") ∊ " ++ show initialValues,
            odeivp_field = \ [_x,x'] -> [x',(-9.8 :: Double) |<+>  x' <*> x' </>| (1000 :: Int)],
            odeivp_componentNames = ["x", "x'"],
            odeivp_tVar = "t",
            odeivp_tStart = 0,
            odeivp_t0End = 0,
            odeivp_tEnd = 10000,
            odeivp_makeInitialValueFnVec = makeIV,
            odeivp_maybeExactValuesAtTEnd = Nothing
        }
    initialValues = 
        [
            (1) CF.</\> (1.1)
        ,
            (0 CF.<-> 4.1)
        ]
    makeIV =
        makeFnVecFromInitialValues componentNames initialValues
    componentNames = odeivp_componentNames ivp
    tStart = odeivp_tStart ivp

ivpLorenz_ishii :: ODEIVP Poly
ivpLorenz_ishii =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = "x' = 10(y-x), y' = x(28-z)-y, z' = xy - 8z/3; (x,y,z)(" ++ show tStart ++ ") ∊ " ++ show initialValues,
            odeivp_field = \ [x,y,z] -> 
                [(10 :: Int) |<*> (y <-> x),
                  (x <*> ((28 :: Int) |<+> (neg z))) <-> y,
                  (x <*> y) <-> (((8 :: Int) |<*> z) </>| (3 :: Int))
                ],
            odeivp_componentNames = ["x", "y", "z"],
            odeivp_tVar = "t",
            odeivp_tStart = 0,
            odeivp_t0End = 0,
            odeivp_tEnd = 24,
            odeivp_makeInitialValueFnVec = makeIV,
            odeivp_maybeExactValuesAtTEnd = Nothing
        }
    initialValues = 
        [
            (0 CF.<+> 15)
        ,
            (0 CF.<+> 15)
        ,
            (0 CF.<+> 36)
        ]
    makeIV =
        makeFnVecFromInitialValues componentNames initialValues
    componentNames = odeivp_componentNames ivp
    tStart = odeivp_tStart ivp

ivpRoessler :: ODEIVP Poly
ivpRoessler =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = 
                "Rössler attractor: x' = -y - z; y' = x + 0.2y; z' = 0.2 + z(x-5.7); "
                ++ "x(0) = 0, y(0) ∊ -8.38095+-0.01, z(0) ∊ 0.0295902+-0.01",
            odeivp_field = \ [x,y,z] -> 
                [neg $ y <+> z, 
                 x <+> ((0.2 :: Double) |<*> y),
                 (0.2 :: Double) |<+> (z <*> ((-5.7 :: Double) |<+> x))
                ],
            odeivp_componentNames = ["x", "y", "z"],
            odeivp_tVar = "t",
            odeivp_tStart = 0,
            odeivp_t0End = 0,
            odeivp_tEnd = 48,
            odeivp_makeInitialValueFnVec = makeIV,
            odeivp_maybeExactValuesAtTEnd = Nothing 
        }
    initialValues = 
        [
            0
        ,
            (-8.38095 :: Double) |<+> pmDelta
        ,
            (0.0295902 :: Double) |<+> pmDelta
        ]
        where
        pmDelta = delta CF.</\> (neg delta)
        delta = (0.01 :: Double) |<+> 0
    makeIV =
        makeFnVecFromInitialValues componentNames initialValues
    componentNames = odeivp_componentNames ivp

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
runOnce [ivpName, shouldShowStepsS, shouldWrapS, maxDegS, depthS] =
    do
    let shouldShowSteps = read shouldShowStepsS :: Bool
    let maxDeg = read maxDegS :: Int
    let depth = read depthS :: Int
    putStrLn "--------------------------------------------------"
    putStrLn "demo of solve-Vt from (Konecny, Taha, Duracz 2012)"
    putStrLn "--------------------------------------------------"
    _ <- solveVtPrintSteps shouldWrap shouldShowSteps ivp (maxDeg, depth)
    return ()
    where
    ivp = ivpByName ivpName
    shouldWrap = read shouldWrapS

writeCSV :: [String] -> IO ()
writeCSV [ivpName, outputFileName, shouldWrapS] =
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
    shouldWrap = read shouldWrapS
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
            solverResult <- solveVtPrintSteps shouldWrap False ivp (maxDegree, depth)
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
    (solvingInfo ~ (CF, Maybe [CF]))
    => 
    Bool
    ->
    Bool
    ->
    ODEIVP Poly 
    -> 
    (Int, Int) 
    -> 
    IO 
    (
        Maybe [CF]
    ,
        (
            SplittingInfo solvingInfo (solvingInfo, Maybe CF)
        )
    )
solveVtPrintSteps shouldWrap shouldShowSteps ivp (maxdegParam, depthParam) =
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
            putStr $ showSegInfo "(almost) exact result = " (tEnd, Just exactResult)
        _ -> return ()
    putStrLn "----------  steps: ---------------------------"
    printStepsInfo (1:: Int) splittingInfoOut
    putStrLn "----------  step summary: -----------------------"
    putStrLn $ "number of atomic segments = " ++ (show $ splittingInfoCountLeafs splittingInfoOut)
    putStrLn $ "smallest segment size: " ++ (show smallestSegSize)  
    putStrLn "----------  result: -----------------------------"
    putStr $ showSegInfo ">>> " (tEnd, endValues)
    case shouldShowSteps of
        True ->
            do
            putStrLn "----------  splitting info: -----------------"
            putStrLn $ showSplittingInfo showSegInfo showSplitReason "" splittingInfoOut
        False -> return ()
    putStrLn "-------------------------------------------------"
    return (endValues, splittingInfoOut)
    where
    -- solver call:
    (endValues, splittingInfoOut) =
        solveIVPWithUncertainValue shouldWrap
            sizeLimits effCf substSplitSizeLimit delta m 
                minStepSize splitImprovementThreshold
                    ivp
    -- parameters:
    delta = 1
    maxdeg = maxdegParam
    maxsize = 1000
    m = 40
    substSplitSizeLimit = 100
--    minStepSizeExp = -4 :: Int
    minStepSizeExp = - depthParam
    minStepSize = 2^^minStepSizeExp
    splitImprovementThreshold = 2^^(-50 :: Int)
    
    -- auxiliary:
    description = odeivp_description ivp
    tStart = odeivp_tStart ivp
    tEnd = odeivp_tEnd ivp
    maybeExactResult = odeivp_maybeExactValuesAtTEnd ivp
    componentNames = odeivp_componentNames ivp
    
    sampleCf = 0 :: CF
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
    sizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms maxdeg maxsize [] []
            
    (smallestSegSize, _) =
        aux tStart (tEnd CF.<-> tStart) splittingInfoOut
        where
        aux tPrev tSmallestSoFar (SegNoSplit (tNow,_)) =
            (CF.minOut tSmallestSoFar (tNow CF.<-> tPrev), tNow)
        aux tPrev tSmallestSoFar (SegSplit _ left Nothing) =
            aux tPrev tSmallestSoFar left
        aux tPrev tSmallestSoFar (SegSplit _ left (Just right)) =
            aux tPrevL tSmallestSoFarL right
            where
            (tSmallestSoFarL, tPrevL) =
                aux tPrev tSmallestSoFar left
    
    showStepInfo (n, segInfo@(t, _)) =
        "step " ++ show n ++ ": t = " ++ show t ++ "\n" ++ (showSegInfo "    " segInfo)
    printStepsInfo n (SegNoSplit segInfo) =
        do
        putStrLn $ showStepInfo (n, segInfo)
        return $ n + 1
    printStepsInfo n (SegSplit _ left maybeRight) =
        do
        n2 <- printStepsInfo n left
        case maybeRight of
            Just right -> printStepsInfo n2 right
            Nothing -> return $ n2 + 1
    
    showSegInfo indent (t, maybeValues) =
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
    (solvingInfo ~ (CF, Maybe [CF]))
    =>
    Bool ->
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
     Maybe ([CF])
    ,
        (
            SplittingInfo solvingInfo (solvingInfo, Maybe CF)
        )
    )
solveIVPWithUncertainValue
        shouldWrap 
            sizeLimits effCf substSplitSizeLimit
                delta m minStepSize splitImprovementThreshold
                    odeivp
    =
    solveUncertainValueExactTimeSplit2
        delta m minStepSize splitImprovementThreshold
            odeivp
    where
    solveUncertainValueExactTimeSplit2 =
        solveUncertainValueExactTimeSplit shouldWrap True
            sizeLimits effSizeLims effCompose effEval effInteg effDeriv effInclFn 
            effAddFn effMultFn effAbsFn effMinmaxFn 
            effDivFnInt effAddFnDom effMultFnDom effCf

--    substituteInitialValueUncertainty fn =
--        pEvalAtPointOutEff effEval initValDomBox fn
--        where
--        initValDomBox =
--            fromList $ zip componentNames initialValues
--        
    sampleCf = delta
    
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
