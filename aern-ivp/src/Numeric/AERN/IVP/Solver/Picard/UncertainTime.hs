{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Solver.Picard.UncertainTime
    Description :  uncertain initial value ODE solvers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Uncertain initial value ODE solvers using interval Piracd methods.
-}

module Numeric.AERN.IVP.Solver.Picard.UncertainTime
(
    solveUncertainValueUncertainTime,
    solveUncertainValueUncertainTimeSplit
)
where

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Picard.UncertainValue

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Misc.Debug
        
solveUncertainValueUncertainTimeSplit
        sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
        odeivpG 
        t0Var
        delta 
        m stepSize epsilon
    =
    solveUncertainValueUncertainTime
        sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
        odeivpG 
        t0Var
        delta
    -- TODO
    -- split T0 and for each segment T0i, solve 2 problems:
    --  one with uncertain initial time T = T0i
    --  one with exact initial time from T0i until the end of T
    -- take the union of the solutions for all segments
        
solveUncertainValueUncertainTime ::
    (CanAddVariables f,
     CanEvaluate f,
     CanCompose f,
     HasProjections f,
     HasConstFns f,
     RefOrd.PartialComparison f,
     RefOrd.IntervalLike f,
     NumOrd.RefinementRoundedLattice f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f), 
     Show f, Show (Domain f)
     )
    =>
    SizeLimits f ->
    CompositionEffortIndicator f ->
    IntegrationEffortIndicator f ->
    RefOrd.PartialCompareEffortIndicator f ->
    ArithInOut.AddEffortIndicator f ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    ODEIVP f ->
    Var f {-^ @t0@ - the initial time variable -} ->
    Domain f {-^ initial widening @delta@ -}  ->
    Maybe [[f]] {-^ sequence of enclosures with domain @T x D@ produced by the Picard operator -}
solveUncertainValueUncertainTime
        sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
        odeivp
        t0Var
        delta
    | (t0End <? tEnd) == Just True =
        error "aern-ivp: solveUncertainValueUncertainTime called with t0End < tEnd"
    | otherwise =
        case solveWithExactTime of
            Just _ ->
                unsafePrint
                (
                    "solveUncertainValueUncertainTime: "
                    ++ "\n tShifted = " ++ show tShifted
                    ++ "\n t0DomainFnBelowT = " ++ show t0DomainFnBelowT
                    ++ "\n tEnd = " ++ show tEnd
                    ++ "\n enclosuresWithTT0[0][tEnd] = " ++ (show $ head $ map (evalAtEndTimeVec tVar tEnd) enclosuresWithTT0) 
                    ++ "\n enclosuresWithTT0[0] = " ++ (show $ head $ enclosuresWithTT0) 
                    ++ "\n enclosuresShifted[0][tEnd] = " ++ (show $ head $ map (evalAtEndTimeVec tVar tEnd) enclosuresShifted) 
                    ++ "\n enclosuresShifted[0] = " ++ (show $ head $ enclosuresShifted) 
                    ++ "\n enclosuresWithoutT0[0][tEnd] = " ++ (show $ head $ map (evalAtEndTimeVec tVar tEnd) enclosuresWithoutT0) 
                    ++ "\n enclosuresWithoutT0[0] = " ++ (show $ head $ enclosuresWithoutT0) 
                ) $
                Just enclosuresWithoutT0
            Nothing -> Nothing
    where
    tVar = odeivp_tVar odeivp
    tStart = odeivp_tStart odeivp
    tEnd = odeivp_tEnd odeivp
    timeDomain =
        RefOrd.fromEndpointsOutWithDefaultEffort (tStart, tEnd)
    t0End = odeivp_t0End odeivp

    -- perform eliminating substitution: t0 |-> min (T0 , t):
    enclosuresWithoutT0 =
        map (map (composeVarOutEff effCompose t0Var t0DomainFnBelowT)) $
        enclosuresShifted
    -- perform non-eliminating substitution: t |-> t - t0 + initialTime:
    enclosuresShifted =
        map (map (composeVarOutEff effCompose tVar tShifted)) enclosuresWithTT0
    (Just enclosuresWithTT0) = solveWithExactTime
    -- compute the enclosures parameterised by t and t0:
    solveWithExactTime =
        solveUncertainValueExactTime
            sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
            delta
            odeivpExactTime
        where
        odeivpExactTime =
            odeivp
            {
                odeivp_makeInitialValueFnVec = makeInitialValuesFnVecExactTime,
                odeivp_t0End = tStart,
                odeivp_tEnd = tEndAdj 
            }
        makeInitialValuesFnVecExactTime sizeLimits t0Var2 t0Domain2 =
            map (addVariablesFront [(t0Var2, t0Domain2)]) $
                odeivp_makeInitialValueFnVec odeivp sizeLimits t0Var timeDomain
    tEndAdj =
        let ?addInOutEffort = effAddDom in 
        tEnd <+> tEnd <-> tStart
    t0DomainFnBelowT =
        RefOrd.fromEndpointsOutWithDefaultEffort (tStartFn, tFn) 
        where
        tStartFn =
            newConstFnFromSample sampleFnWithoutT0 tStart
        tFn = 
            newProjectionFromSample sampleFnWithoutT0 tVar
    (sampleFnWithoutT0 : _) =
        odeivp_makeInitialValueFnVec odeivp sizeLimits tVar timeDomain
    tShifted =
        let ?addInOutEffort = effAddFn in
        (tFn <-> t0Fn) <+> initialTimeFn
        where
        tFn = 
            newProjectionFromSample sampleFnWithTT0 tVar
        t0Fn =
            newProjectionFromSample sampleFnWithTT0 t0Var
        initialTimeFn =
            newConstFnFromSample sampleFnWithTT0 tStart
    (sampleFnWithTT0 : _) = head enclosuresWithTT0
            
    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
    sampleDom = tStart
        