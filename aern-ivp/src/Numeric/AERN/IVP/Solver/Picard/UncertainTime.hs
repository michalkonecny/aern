{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    solveODEIVPUncertainValueUncertainTime_PicardIterations,
    solveODEIVPUncertainValueUncertainTime_UsingPicard_Bisect
)
where

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Bisection
import Numeric.AERN.IVP.Solver.Picard.UncertainValue

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration
import Numeric.AERN.RmToRn.Differentiation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
--import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import Numeric.AERN.Misc.Debug
_ = unsafePrint
        
solveODEIVPUncertainValueUncertainTime_UsingPicard_Bisect ::        
    (CanAddVariables f,
     CanChangeSizeLimits f,
     CanEvaluate f,
     CanPartiallyEvaluate f,
     CanCompose f,
     HasProjections f,
     HasConstFns f,
     RefOrd.PartialComparison f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     NumOrd.RefinementRoundedLattice f,
     RoundedIntegration f,
     RoundedFakeDerivative f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMultiply f,
     ArithUpDn.RoundedAbs f,
     NumOrd.RoundedLattice f,
     ArithInOut.RoundedMixedDivide f Int,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedMixedMultiply f (Domain f),
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f), 
     HasAntiConsistency (Domain f), 
     Show f, Show (Domain f), Show (Var f),
     AppendableVariables (Var f),
     dom ~ Domain f,
     dom ~ Imprecision dom, 
     solvingInfo1 ~ (dom, Maybe [dom]),
     solvingInfo2 ~ BisectionInfo solvingInfo1 (solvingInfo1, Maybe dom),
     solvingInfo3 ~ (solvingInfo1, (solvingInfo1, Maybe solvingInfo2))
    )
    =>
    SizeLimits f -> 
    SizeLimits f ->
    SizeLimitsChangeEffort f -> 
    CompositionEffortIndicator f ->
    EvaluationEffortIndicator f ->
    IntegrationEffortIndicator f ->
    FakeDerivativeEffortIndicator f ->
    RefOrd.PartialCompareEffortIndicator f ->
    ArithInOut.AddEffortIndicator f ->
    ArithInOut.MultEffortIndicator f ->
    ArithUpDn.AbsEffortIndicator f ->
    NumOrd.MinmaxEffortIndicator f ->
    ArithInOut.MixedDivEffortIndicator f Int ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) ->
    ArithInOut.MixedMultEffortIndicator f (Domain f) ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) 
    ->
    dom ->
    Int ->
    dom ->
    dom ->
    dom ->
    Var f ->
    ODEIVP f 
    ->
    (
     Maybe [dom]
    ,
     BisectionInfo solvingInfo3 (solvingInfo3, Maybe dom)
    )
solveODEIVPUncertainValueUncertainTime_UsingPicard_Bisect
        sizeLimits t0SizeLimits 
        effSizeLims effCompose effEval effInteg effDeriv effInclFn 
        effAddFn effMultFn effAbsFn effMinmaxFn 
        effDivFnInt effAddFnDom effMultFnDom effDom
--        sizeLimits t0SizeLimits effSizeLims effCompose effEval effInteg .... effInclFn 
--        effAddFn effMultFn .... 
--        .... effAddFnDom .... effDom
            delta m minStepSize minT0StepSize splitImprovementThreshold
                t0Var
                    odeivpG 
    =
    solverSplittingT0 odeivpG
    where
    componentNames = odeivp_componentNames odeivpG
--    tEndG = odeivp_tEnd odeivpG
    tVar = odeivp_tVar odeivpG
    
    solverSplittingT0 odeivp =
        solveODEIVPByBisectingT0
            solverSplittingAtT0End
                effDom splitImprovementThreshold minT0StepSize
                    odeivp

    solverSplittingAtT0End odeivp =
        solveODEIVPByBisectingAtT0End
            solverVT (makeFnVecFromParamInitialValuesOut effAddFn effMultFn effSizeLims componentNamesWithT0) 
                solverVt 
                    odeivp
        where
        componentNamesWithT0 =
            map (flip appendVar t0Var) componentNames
    
    solverVT odeivp =
        case maybeIterations of
            Just iterations -> 
                let chosenIteration = iterations !! m in
                let valuesAtEnd = evalAtEndTimeOutInVec effEval tVar t0End chosenIteration in
                let paramValuesAtEnd = partiallyEvalAtEndTimeOutInVec tVar t0End $ unzip chosenIteration in
                (Just (fst paramValuesAtEnd), (t0End, Just (fst valuesAtEnd)))
            _ -> (Nothing, (t0End, Nothing))
        where
        t0End = odeivp_t0End odeivp
        maybeIterations =
            solveODEIVPUncertainValueUncertainTime_PicardIterations
                t0SizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
                    delta
                        t0Var
                            odeivp 

    solverVt odeivp =
        solveODEIVPUncertainValueExactTime_UsingPicard_Bisect False True
            sizeLimits effSizeLims effCompose effEval effInteg effDeriv effInclFn 
            effAddFn effMultFn effAbsFn effMinmaxFn 
            effDivFnInt effAddFnDom effMultFnDom effDom
                delta m minStepSize splitImprovementThreshold
                    odeivp
        
solveODEIVPUncertainValueUncertainTime_PicardIterations ::
    (CanAddVariables f,
     CanEvaluate f,
     CanCompose f,
     HasProjections f,
     HasConstFns f,
     RefOrd.PartialComparison f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     NumOrd.RefinementRoundedLattice f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f), 
     HasAntiConsistency (Domain f), 
     Show f, Show (Domain f)
     )
    =>
    SizeLimits f 
    ->
    CompositionEffortIndicator f 
    ->
    IntegrationEffortIndicator f 
    ->
    RefOrd.PartialCompareEffortIndicator f 
    ->
    ArithInOut.AddEffortIndicator f 
    ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) 
    ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) 
    ->
    Domain f {-^ initial widening @delta@ -}  
    ->
    Var f {-^ @t0@ - the initial time variable -} 
    ->
    ODEIVP f 
    ->
    Maybe [[(f,f)]] {-^ sequence of outer and inner enclosures with domain @T x D@ produced by the Picard operator -}
solveODEIVPUncertainValueUncertainTime_PicardIterations
        sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
            delta
                t0Var
                    odeivp
    | (t0End <? tEnd) == Just True =
        error "aern-ivp: solveUncertainValueUncertainTime called with t0End < tEnd"
    | otherwise =
        case solveWithExactTime of
            Just _ ->
--                unsafePrint
--                (
--                    "solveUncertainValueUncertainTime: "
--                    ++ "\n tShifted = " ++ show tShifted
--                    ++ "\n t0DomainFnBelowT = " ++ show t0DomainFnBelowT
--                    ++ "\n tEnd = " ++ show tEnd
--                    ++ "\n enclosuresWithTT0[0][tEnd] = " ++ (show $ head $ map (evalAtEndTimeVec tVar tEnd) enclosuresWithTT0) 
--                    ++ "\n enclosuresWithTT0[0] = " ++ (show $ head $ enclosuresWithTT0) 
--                    ++ "\n enclosuresShifted[0][tEnd] = " ++ (show $ head $ map (evalAtEndTimeVec tVar tEnd) enclosuresShifted) 
--                    ++ "\n enclosuresShifted[0] = " ++ (show $ head $ enclosuresShifted) 
--                    ++ "\n enclosuresWithoutT0[0][tEnd] = " ++ (show $ head $ map (evalAtEndTimeVec tVar tEnd) enclosuresWithoutT0) 
--                    ++ "\n enclosuresWithoutT0[0] = " ++ (show $ head $ enclosuresWithoutT0) 
--                ) $
--                Just enclosuresWithoutT0
                Just enclosuresShifted
            Nothing -> Nothing
    where
    tVar = odeivp_tVar odeivp
    tStart = odeivp_tStart odeivp
    tEnd = odeivp_tEnd odeivp
    timeDomain =
        RefOrd.fromEndpointsOutWithDefaultEffort (tStart, tEnd)
    t0End = odeivp_t0End odeivp

    -- perform eliminating substitution: t0 |-> min (T0 , t):
    _enclosuresWithoutT0 =
        map (map (removeT0)) enclosuresShifted
        where
        removeT0 (enclOut, enclIn) =
            (composeVarOutEff effCompose t0Var t0DomainFnBelowT enclOut,
             composeVarInEff effCompose t0Var t0DomainFnBelowT enclIn)
    -- perform non-eliminating substitution: t |-> t - t0 + initialTime:
    enclosuresShifted =
        map (map shiftTByT0) enclosuresWithTT0
        where
        shiftTByT0 enclosure = 
            (composeVarOutEff effCompose tVar tShifted enclosure,
             composeVarInEff effCompose tVar tShifted $ flipConsistency enclosure) 
    (Just enclosuresWithTT0) = solveWithExactTime
    -- compute the enclosures parameterised by t and t0:
    solveWithExactTime =
        solveODEIVPUncertainValueExactTime_PicardIterations
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
        makeInitialValuesFnVecExactTime sizeLimits2 t0Var2 t0Domain2 =
            map (addVariablesFront [(t0Var2, t0Domain2)]) $
                odeivp_makeInitialValueFnVec odeivp sizeLimits2 t0Var timeDomain
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
        