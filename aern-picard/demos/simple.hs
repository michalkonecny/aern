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
    solveExpDecay

solveExpDecay :: IO ()
solveExpDecay =
    do
    putStrLn $ "solving: x' = -x; x(" ++ show timeStart ++ ") = " ++ show initialValues
    putStrLn "-------------------------------------------------"
    putStrLn $ "maxdeg = " ++ show maxdeg
    putStrLn $ "maxsize = " ++ show maxsize
    putStrLn $ "delta = " ++ show delta
    putStrLn $ "x(" ++ show timeEnd ++ ") = ?"
    putStrLn "-------------------------------------------------"
    putEnclosureEndpoints "t" $
        take 20 $
            enclosuresOfIVPWithUncertainValue 
                effCf maxdeg maxsize delta 
                    tVar timeDomain componentNames field initialValues
    where
    timeStart = 0
    initialValues = [(-1) DI.</\> 1]
    delta = 1
    maxdeg = 10
    maxsize = 100
    timeEnd = 1
    
    field [x] = [neg x]
    timeDomain = timeStart DI.</\> timeEnd
    componentNames = ["x"]
    tVar = "t"
    effCf = ArithInOut.roundedRealDefaultEffort (0:: CF)
--    effCf = (100, (100,())) -- MPFR

    
enclosuresOfIVPWithUncertainValue :: 
    PartialEvaluationEffortIndicator Poly -> 
    Int -> Int -> 
    CF ->
    Var Poly ->
    CF ->
    [Var Poly] ->
    ([Poly] -> [Poly]) ->
    [CF] ->
    [[Poly]]
enclosuresOfIVPWithUncertainValue 
        effCf maxdeg maxsize delta 
            tVar timeDomain componentNames field initialValues
    =
--    map (map substituteInitialValueUncertainty) $
        solveUncertainValueExactTime
            effInteg effInclFn effAddFn effAddFnDom effJoinDom
            tVar timeDomain initialValuesFns field delta
    where

    initialValuesFns =
        map initialValueFn componentNames
    initialValueFn var =
        newProjectionFromSample sampleFnWithoutT var 

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
    Var f -> [f] -> [Domain f]
evalAtEndTimeVec tVar fnVec =
    map (evalAtEndTimeFn tVar) fnVec

evalAtEndTimeFn ::
    (HasDomainBox f, CanEvaluate f, RefOrd.IntervalLike (Domain f))
    =>
    Var f -> f -> (Domain f)
evalAtEndTimeFn tVar fn =
    evalAtPointOutEff (evaluationDefaultEffort fn) endTimeArea fn
    where
    endTimeArea = insertVar tVar timeDomainR $ getDomainBox fn
    (_, timeDomainR) = RefOrd.getEndpointsOutWithDefaultEffort timeDomain
    Just timeDomain = lookupVar (getDomainBox fn) tVar 
     
putEnclosureEndpoints :: 
    (Show f, Show (Domain f),
     RefOrd.IntervalLike (Domain f),
     CanEvaluate f) 
    =>
    Var f -> [[f]] -> IO ()
putEnclosureEndpoints tVar fnVectors =
    mapM_ putEndpt $ zip [1..] fnVectors
    where
    putEndpt (n, fns) =
        do
        putStrLn $ "---------- enclosure " ++ show (n :: Int) ++ ":"
        putStrLn $ "at end time: " ++ (show $ evalAtEndTimeVec tVar fns)
--        putStrLn $ "in full: " ++ (show fns)
        