{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.IVP.Solver.Picard.UncertainValue
import Numeric.AERN.IVP.Solver.Picard.UncertainTime

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

import Numeric.AERN.Misc.Debug

import System.IO

--import qualified Data.Map as Map
--import qualified Data.List as List

type CF = CF.DI
type Poly = IntPoly String CF
type PolyVec = [Poly]

shouldPrintPrecision :: Bool
shouldPrintPrecision = False
--shouldPrintPrecision = True



main :: IO ()
main =
    do
    putStrLn "-------------------------------------------------"
    putStrLn "demo of aern-picard using simple examples"
    putStrLn "-------------------------------------------------"
    solveExpDecayVt
--    solveExpDecayVT

solveExpDecayVt :: IO ()
solveExpDecayVt =
    do
    putStrLn $ "solving: x' = -x; x(" ++ show timeStart ++ ") \\in " ++ show initialValues
    putStrLn "----------  parameters: -------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "m = " ++ show m
    putStrLn $ "minimum step size = " ++ show minStepSize
    putStrLn $ "split improvement threshold = " ++ show splitImprovementThreshold
    putStrLn "----------  result: -----------------------------"
    putStrLn $ "x(" ++ show timeEnd ++ ") = " ++ show endValues
    putStrLn "----------  steps: ------------------------------"
    putStr $ unlines $ map showStep stepValues
    putStrLn "-------------------------------------------------"
    where
    (endValues, stepValues) =
        enclosuresOfIVPWithUncertainValue
            effCf maxdeg maxsize delta m minStepSize splitImprovementThreshold
                tVar timeStart timeEnd componentNames field initialValues
    timeStart = 0
    initialValues = [(-1) CF.</\> 1]
    delta = 1
    maxdeg = 12
    maxsize = 100
    m = 20
    minStepSize = 2^^(-6 :: Int)
    splitImprovementThreshold = 2^^(-50 :: Int)
    timeEnd = 1
    -- TODO: reintroduce epsilon - threshold for improvement by further splitting
    
    field [x] = [neg x]
    componentNames = ["x"]
    tVar = "t"
    effCf = ArithInOut.roundedRealDefaultEffort (0:: CF)
    showStep (t, values) =
        "x(" ++ show t ++ ") = " ++ show values
--    effCf = (100, (100,())) -- MPFR

solveExpDecayVT :: IO ()
solveExpDecayVT =
    do
    putStrLn $ "solving: x' = -x; " 
                ++ show timeStart ++ " < t_0 < " ++ show t0End 
                ++ "; x(t_0) \\in " ++ show initialValueFns
    putStrLn "-------------------------------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "m = " ++ show m
    putStrLn $ "minimum stepSize = " ++ show stepSize
    putStrLn $ "x(" ++ show timeEnd ++ ") = ?"
    putStrLn "-------------------------------------------------"
    putEnclosureEndpoints "t" timeEnd $
        take 20 $
            enclosuresOfIVPWithUncertainTime
                effCf delta m stepSize splitImprovementThreshold
                tVar timeStart timeEnd t0Var t0End 
                field initialValueFns
    where
    timeStart = -0.125
    t0End = 0.125
    initialValueFns = 
        [ (1 :: Int) |<+> t0VarFn]
--        [t0VarFn <+> (xUnitFn </>| (8::Int))]
    delta = 1
    maxdeg = 20
    maxsize = 400
    m = 20
    stepSize = 1
    splitImprovementThreshold :: CF
    splitImprovementThreshold = 2^^(-20)
    timeEnd = 1
    
    field [x] = [neg x]
--    timeDomain = timeStart CF.</\> timeEnd
    t0Domain = timeStart CF.</\> t0End
    componentNames = ["x"]
    tVar = "t"
    t0Var = "t0"

    t0VarFn = newProjectionFromSample sampleFnWithoutT t0Var
    sampleFnWithoutT =
        makeSampleWithVarsDoms maxdeg maxsize (t0Var : componentNames) (t0Domain : componentUncertaintyDomains)
    componentUncertaintyDomains =
        map snd $ zip componentNames $ repeat unitDom
    unitDom = 0 CF.</\> 1 

    effCf = ArithInOut.roundedRealDefaultEffort (0:: CF)
--    effCf = (100, (100,())) -- MPFR

    
enclosuresOfIVPWithUncertainValue :: 
    ArithInOut.RoundedRealEffortIndicator CF
    -> Int
    -> Int
    -> CF
    -> Int
    -> CF
    -> CF
    -> Var Poly
    -> CF
    -> CF
    -> [Var Poly]
    -> ([Poly] -> [Poly])
    -> [CF]
    -> 
    (
     Maybe [CF]
    ,
     [(CF, [CF])]
    )
enclosuresOfIVPWithUncertainValue 
        effCf maxdeg maxsize delta m stepSize splitImprovementThreshold
            tVar tStart tEnd componentNames field initialValues
    =
--    undefined
    solveUncertainValueExactTimeSplit
        sampleFnWithoutT componentNames
        effInteg effInclFn effAddFn effAddFnDom effCf
        tVar tStart tEnd initialValues field delta
        m stepSize splitImprovementThreshold
    where

--    substituteInitialValueUncertainty fn =
--        pEvalAtPointOutEff effEval initValDomBox fn
--        where
--        initValDomBox =
--            fromList $ zip componentNames initialValues
--        
    sampleFnWithoutT :: Poly
    sampleFnWithoutT = 
        makeSampleWithVarsDoms maxdeg maxsize (componentNames) (initialValues)
    sampleCf = 
        getSampleDomValue sampleFnWithoutT
    
--    effEval = effCf
    effInteg = effCf
    effAddFn = effCf
    effAddFnDom =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effInclFn = ((Int1To1000 0, effCf), ())

enclosuresOfIVPWithUncertainTime :: 
    ArithInOut.RoundedRealEffortIndicator CF -> 
    CF ->
    Int ->
    CF ->
    CF ->
    Var Poly ->
    CF ->
    CF ->
    Var Poly ->
    CF ->
    ([Poly] -> [Poly]) ->
    [Poly] ->
    [[Poly]]
enclosuresOfIVPWithUncertainTime 
        effCf delta m stepSize epsilon
        tVar tStart tEnd t0Var t0End
        field initialValueFns
    =
    result
    where
    (Just result) =
        solveUncertainValueUncertainTimeSplit
            effCompose effInteg effInclFn effAddFn effAddFnDom effCf
            sampleFnWithT tVar tStart tEnd t0Var t0End
            initialValueFns field delta m stepSize epsilon

--    substituteInitialValueUncertainty fn =
--        pEvalAtPointOutEff effEval initValDomBox fn
--        where
--        initValDomBox =
--            fromList $ zip componentNames initialValues
--        
    (sampleFnWithoutT : _) =  initialValueFns -- domain @T x D@
    sampleFnWithT = 
        polyMapVars t02t sampleFnWithoutT
        where
        t02t var 
            | var == t0Var = tVar
            | otherwise = var 
    sampleCf = 
        getSampleDomValue sampleFnWithoutT
    
    effCompose = effCf
    effInteg = effCf
    effAddFn = effCf
    effAddFnDom =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effInclFn = ((Int1To1000 0, effCf), ())
--    effJoinDom =
--        ArithInOut.rrEffortJoinMeet sampleCf effCf


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
     
evalAtEndTimeVec :: 
    (RefOrd.IntervalLike (Domain f), CanEvaluate f) 
    =>
    Var f -> Domain f -> [f] -> [Domain f]
evalAtEndTimeVec tVar tEnd fnVec =
    map (evalAtEndTimeFn tVar tEnd) fnVec

evalAtEndTimeFn ::
    (HasDomainBox f, CanEvaluate f, RefOrd.IntervalLike (Domain f))
    =>
    Var f -> Domain f -> f -> (Domain f)
evalAtEndTimeFn tVar tEnd fn =
    evalAtPointOutEff (evaluationDefaultEffort fn) endTimeArea fn
    where
    endTimeArea = insertVar tVar tEnd $ getDomainBox fn
     
putEnclosureEndpoints :: 
    (Show f, Show (Domain f),
     RefOrd.IntervalLike (Domain f),
     CanEvaluate f) 
    =>
    Var f -> Domain f -> [[f]] -> IO ()
putEnclosureEndpoints tVar tEnd fnVectors =
    mapM_ putEndpt $ zip [1..] fnVectors
    where
    putEndpt (n, fns) =
        do
        putStrLn $ "---------- enclosure " ++ show (n :: Int) ++ ":"
        putStrLn $ "at end time: " ++ (show $ evalAtEndTimeVec tVar tEnd fns)
--        putStrLn $ "in full: " ++ (show fns)
        
        
putVals ::
    (Show (Domain f), 
     CanEvaluate f, 
     RefOrd.IntervalLike (Domain f)) 
    =>
    Var f -> Domain f -> [f] 
    -> 
    IO ()
putVals tVar tEnd vector =
    do
    putStrLn $ "at end time: " ++ (show $ evalAtEndTimeVec tVar tEnd vector)
        