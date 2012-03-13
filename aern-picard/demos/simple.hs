{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.RmToRn.Picard

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.Basis.Double
import qualified Numeric.AERN.DoubleBasis.Interval as DI
--import Numeric.AERN.RealArithmetic.Basis.MPFR
--import qualified Numeric.AERN.MPFRBasis.Interval as MI

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.ShowInternals

import Numeric.AERN.Misc.Debug

import System.IO

--import qualified Data.Map as Map
--import qualified Data.List as List

type CF = DI.DI
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
    putStrLn $ "minimum stepSize = " ++ show stepSize
    putStrLn "----------  result: -----------------------------"
    putStrLn $ "x(" ++ show timeEnd ++ ") = " ++ show endValues
    putStrLn "----------  steps: ------------------------------"
    putStr $ unlines $ map showStep stepValues
    putStrLn "-------------------------------------------------"
    where
    (endValues, stepValues) =
        enclosuresOfIVPWithUncertainValue
            effCf maxdeg maxsize delta m stepSize
                tVar timeStart timeEnd componentNames field initialValues
    timeStart = 0
    initialValues = [(-1) DI.</\> 1]
    delta = 1
    maxdeg = 10
    maxsize = 100
    m = 20
    stepSize = 1
    timeEnd = 1
    -- TODO: reintroduce epsilon - threshold for improvement by further splitting
    
    field [x] = [neg x]
    timeDomain = timeStart DI.</\> timeEnd
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
    putStrLn $ "x(" ++ show timeEnd ++ ") = ?"
    putStrLn "-------------------------------------------------"
    putEnclosureEndpoints "t" timeEnd $
        take 20 $
            enclosuresOfIVPWithUncertainTime
                effCf delta 
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
    timeEnd = 1
    
    field [x] = [neg x]
--    timeDomain = timeStart DI.</\> timeEnd
    t0Domain = timeStart DI.</\> t0End
    componentNames = ["x"]
    tVar = "t"
    t0Var = "t0"

    t0VarFn = newProjectionFromSample sampleFnWithoutT t0Var
    [xUnitFn] = map (newProjectionFromSample sampleFnWithoutT) componentNames
    sampleFnWithoutT =
        makeSampleWithVarsDoms maxdeg maxsize (t0Var : componentNames) (t0Domain : componentUncertaintyDomains)
    componentUncertaintyDomains =
        map snd $ zip componentNames $ repeat unitDom
    unitDom = 0 DI.</\> 1 

    effCf = ArithInOut.roundedRealDefaultEffort (0:: CF)
--    effCf = (100, (100,())) -- MPFR

    
--enclosuresOfIVPWithUncertainValue :: 
--    ArithInOut.RoundedRealEffortIndicator CF -> 
--    Int -> Int -> 
--    CF ->
--    Var Poly ->
--    CF ->
--    CF ->
--    [Var Poly] ->
--    ([Poly] -> [Poly]) ->
--    [CF] ->
--    [[Poly]]
enclosuresOfIVPWithUncertainValue 
        effCf maxdeg maxsize delta m stepSize
            tVar tStart tEnd componentNames field initialValues
    =
--    undefined
    solveUncertainValueExactTimeSplit
        sampleFnWithoutT componentNames
        effInteg effInclFn effAddFn effAddFnDom effCf
        tVar tStart tEnd initialValues field delta
        m stepSize
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
    effJoinDom =
        ArithInOut.rrEffortJoinMeet sampleCf effCf

enclosuresOfIVPWithUncertainTime :: 
    ArithInOut.RoundedRealEffortIndicator CF -> 
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
        effCf delta 
        tVar tStart tEnd t0Var t0End 
        field initialValueFns
    =
    result
    where
    (Just result) =
        solveUncertainValueUncertainTime
            effCompose effInteg effInclFn effMinmax effAddFn effAddFnDom effCf
            sampleFnWithT tVar tStart tEnd t0Var t0End 
            initialValueFns field delta

--    substituteInitialValueUncertainty fn =
--        pEvalAtPointOutEff effEval initValDomBox fn
--        where
--        initValDomBox =
--            fromList $ zip componentNames initialValues
--        
    (sampleFnWithoutT : _) =  initialValueFns -- domain @T0 x D@
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
    effMinmax = minmaxInOutDefaultEffortIntPolyWithBezierDegree 10 sampleFnWithoutT
    effAddFn = effCf
    effAddFnDom =
        ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    effInclFn = ((Int1To1000 0, effCf), ())
--    effJoinDom =
--        ArithInOut.rrEffortJoinMeet sampleCf effCf


makeSampleWithVarsDoms :: 
     Int -> Int -> [Var Poly] -> [DI.DI] -> Poly
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
            ipolycfg_domsLZ = zipWith (DI.<->) doms domsLE,
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
        
        
putVals tVar tEnd vector =
    do
    putStrLn $ "at end time: " ++ (show $ evalAtEndTimeVec tVar tEnd vector)
        