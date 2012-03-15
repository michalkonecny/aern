{-# LANGUAGE FlexibleContexts #-}
module Main where

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.IVP.Specification.ODE
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
    solveVt ivpExpDecayVt
--    solveVT ivpExpDecayVT

ivpExpDecayVt :: ODEIVP Poly
ivpExpDecayVt =
    ivp
    where
    ivp =
        ODEIVP
        {
            odeivp_description = "x' = -x; x(" ++ show tStart ++ ") \\in " ++ show initialValues,
            odeivp_field = \ [x] -> [neg x],
            odeivp_componentNames = ["x"],
            odeivp_tVar = "t",
            odeivp_tStart = 0,
            odeivp_t0End = 0, 
            odeivp_tEnd = 1,
            odeivp_makeInitialValueFnVec = makeIV 
        }
    initialValues = [(1 CF.<-> 0.125 ) CF.</\> (1 CF.<+> 0.125)]
    makeIV =
        makeFnVecFromInitialValues componentNames initialValues
    componentNames = odeivp_componentNames ivp
    tStart = odeivp_tStart ivp
    
--    sampleFn :: Poly
--    sampleFn = 
--        makeSampleWithVarsDoms maxdeg maxsize 
--            (tVar : componentNames) 
--            (replicate (dimension + 1) sampleCf) -- these values are irrelevant
--    sampleCf = 0 :: CF
--    tVar = odeivp_tVar ivp
--    dimension = length componentNames
    

solveVt :: ODEIVP Poly -> IO ()
solveVt ivp =
    do
    putStrLn $ "solving: " ++ description
    putStrLn "----------  parameters: -------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "m = " ++ show m
    putStrLn $ "minimum step size = " ++ show minStepSize
    putStrLn $ "split improvement threshold = " ++ show splitImprovementThreshold
    putStrLn "----------  result: -----------------------------"
    putStrLn $ "x(" ++ show tEnd ++ ") = " ++ show endValues
    putStrLn "----------  steps: ------------------------------"
    putStr $ unlines $ map showStep stepValues
    putStrLn "-------------------------------------------------"
    where
    -- solver call:
    (endValues, stepValues) =
        enclosuresOfIVPWithUncertainValue
            sizeLimits effCf delta m 
                minStepSize splitImprovementThreshold
                    ivp
    -- parameters:
    delta = 1
    maxdeg = 12
    maxsize = 100
    m = 20
    minStepSize = 2^^(-6 :: Int)
    splitImprovementThreshold = -10 -- 2^^(-50 :: Int)
    
    -- auxiliary:
    description = odeivp_description ivp
    tStart = odeivp_tStart ivp
    tEnd = odeivp_tEnd ivp
    tVar = odeivp_tVar ivp
    dimension = length componentNames
    componentNames = odeivp_componentNames ivp
    
    sampleCf = 0 :: CF
    showStep (t, values) =
        "x(" ++ show t ++ ") = " ++ show values
--    effCf = (100, (100,())) -- MPFR
    effCf = ArithInOut.roundedRealDefaultEffort sampleCf
    sizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms maxdeg maxsize [] []

ivpExpDecayVT =
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
            odeivp_tEnd = 0.125,
            odeivp_makeInitialValueFnVec = makeIV 
        }
    description =
        "x' = -x; " 
        ++ show tStart ++ " < t_0 < " ++ show t0End
        ++ "; x(t_0) \\in " ++ (show $ makeIV dummySizeLimits "t_0" tStart)
    tStart = odeivp_tStart ivp
    t0End = odeivp_t0End ivp
    componentNames = odeivp_componentNames ivp
    dummySizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms 0 0 [] []
    makeIV sizeLimits t0Var t0Dom =
        [ (1 :: Int) |<+> t0VarFn]
--        [t0VarFn <+> (xUnitFn </>| (8::Int))]
        where
        t0VarFn = newProjectionFromSample sampleInitialValueFn t0Var
        sampleInitialValueFn =
            makeSampleWithVarsDoms 
                maxdeg maxsize 
                (t0Var : componentNames) (t0Dom : componentUncertaintyDomains)
            where
            componentUncertaintyDomains =
                map snd $ zip componentNames $ repeat unitDom
            unitDom = 0 CF.</\> 1 
        maxdeg = ipolycfg_maxdeg sizeLimits
        maxsize = ipolycfg_maxsize sizeLimits

solveVT :: ODEIVP Poly -> IO ()
solveVT ivp =
    do
    putStrLn $ "solving: " ++ description 
    putStrLn "-------------------------------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "m = " ++ show m
    putStrLn $ "minimum stepSize = " ++ show stepSize
    putStrLn $ "x(" ++ show tEnd ++ ") = ?"
    putStrLn "-------------------------------------------------"
    putEnclosureEndpoints "t" tEnd $
        take 20 $
    -- solver call:
            enclosuresOfIVPWithUncertainTime
                sizeLimits
                effCf delta m stepSize splitImprovementThreshold
                ivp
                "t0"
    where
    -- parameters:
    delta = 1
    maxdeg = 10
    maxsize = 400
    m = 20
    stepSize = 1
    splitImprovementThreshold = 2^^(-20 :: Int) :: CF
    
    -- auxiliary:
--    timeDomain = timeStart CF.</\> timeEnd

    initialValueFns =
        odeivp_makeInitialValueFnVec ivp sizeLimits "t0" (tStart CF.</\> t0End) 
    tStart = odeivp_tStart ivp
    tEnd = odeivp_tEnd ivp
    t0End = odeivp_t0End ivp
    description = odeivp_description ivp
--    tVar = odeivp_tVar ivp
--    dimension = length componentNames
    componentNames = odeivp_componentNames ivp


    effCf = ArithInOut.roundedRealDefaultEffort (0:: CF)
--    effCf = (100, (100,())) -- MPFR
    sizeLimits =
        getSizeLimits $
            makeSampleWithVarsDoms maxdeg maxsize [] []

    
enclosuresOfIVPWithUncertainValue ::
    SizeLimits Poly 
    -> ArithInOut.RoundedRealEffortIndicator CF
    -> CF
    -> Int
    -> CF
    -> CF
    -> ODEIVP Poly
    -> 
    (
     Maybe [CF]
    ,
     [(CF, [CF])]
    )
enclosuresOfIVPWithUncertainValue
        sizeLimits 
            effCf delta m stepSize splitImprovementThreshold
                odeivp
    =
--    undefined
    solveUncertainValueExactTimeSplit
        sizeLimits
        effCompose effInteg effInclFn effAddFn effAddFnDom effCf
        odeivp 
        delta m stepSize splitImprovementThreshold
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

enclosuresOfIVPWithUncertainTime ::
    SizeLimits Poly -> 
    ArithInOut.RoundedRealEffortIndicator CF -> 
    CF ->
    Int ->
    CF ->
    CF ->
    ODEIVP Poly ->
    Var Poly ->
    [[Poly]]
enclosuresOfIVPWithUncertainTime 
        sizeLimits effCf delta m stepSize epsilon 
        odeivp
        t0Var
    =
    result
    where
    (Just result) =
        solveUncertainValueUncertainTimeSplit
            sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effCf
            odeivp
            t0Var
            delta m stepSize epsilon

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
        