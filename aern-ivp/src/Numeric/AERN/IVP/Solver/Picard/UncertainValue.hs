{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Solver.Picard.UncertainValue
    Description :  uncertain initial value ODE solvers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Uncertain initial value ODE solvers using interval Piracd methods.
-}

module Numeric.AERN.IVP.Solver.Picard.UncertainValue
(
    solveUncertainValueExactTime,
    solveUncertainValueExactTimeSplitWrap,
    solveUncertainValueExactTimeSplitPEval
)
where

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Splitting
import Numeric.AERN.IVP.Solver.ShrinkWrap

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration
import Numeric.AERN.RmToRn.Differentiation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Exception

import Numeric.AERN.Misc.Debug
_ = unsafePrint
        
solveUncertainValueExactTimeSplitWrap ::
    (CanAddVariables f,
     CanEvaluate f,
     CanPartiallyEvaluate f,
     CanCompose f,
     CanChangeSizeLimits f,
     HasProjections f,
     HasConstFns f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     RefOrd.PartialComparison f,
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
     Domain f ~ Imprecision (Domain f),
     solvingInfo ~ (Domain f, Maybe [Domain f]), 
     Show f, Show (Domain f), Show (Var f))
    =>
    SizeLimits f {-^ size limits for all function -} ->
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
    Domain f {-^ initial widening @delta@ -}  ->
    Int {-^ @m@ -} -> 
    Domain f {-^ step size @s@ -} -> 
    Imprecision (Domain f) {-^ split improvement threshold @eps@ -} ->
    ODEIVP f
    ->
    (
        Maybe [Domain f]
    ,
        (
            SplittingInfo solvingInfo (solvingInfo, Maybe (Imprecision (Domain f)))
        )
    )
solveUncertainValueExactTimeSplitWrap
        sizeLimits _effSizeLims effCompose effEval effInteg _effDeriv effInclFn 
        effAddFn _effMultFn _effAbsFn _effMinmaxFn 
        _effDivFnInt effAddFnDom _effMultFnDom effDom
                delta m minStepSize splitImprovementThreshold
                    (odeivpG :: ODEIVP f)
    | (odeivp_tStart odeivpG <? odeivp_t0End odeivpG) == Just True =
        error "aern-ivp: solveUncertainValueExactTime called with an uncertain time IVP"
    | otherwise =
        solve odeivpG
    where
    tVar = odeivp_tVar odeivpG
    componentNames = odeivp_componentNames odeivpG

    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
    effImpr = ArithInOut.rrEffortImprecision sampleDom effDom
    sampleDom = odeivp_tStart odeivpG 
    
    solve odeivp =
        solveODEIVPBySplittingT
            directSolver measureResultImprecision (makeFnVecFromInitialValues componentNames)
                effDom splitImprovementThreshold minStepSize
                    odeivp

    measureResultImprecision vec =
        let ?addInOutEffort = effAddDom in
        foldl1 (<+>) $ map perComp vec
        where
        perComp comp =        
            imprecisionOfEff effImpr comp
    directSolver odeivp =
        case maybeIterations of
            Just iterations ->
                let valuesAtEnd = evalAtEndTimeVec effEval tVar tEnd $ iterations !! m in
                (Just (fst valuesAtEnd), (tEnd, Just (fst valuesAtEnd)))
            Nothing -> (Nothing, (tEnd, Nothing))
        where
        maybeIterations =
            solveUncertainValueExactTime
                sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
                    delta
                        odeivp
        tEnd = odeivp_tEnd odeivp

solveUncertainValueExactTimeSplitPEval ::
    (CanAddVariables f,
     CanEvaluate f,
     CanPartiallyEvaluate f,
     CanCompose f,
     CanChangeSizeLimits f,
     HasProjections f,
     HasConstFns f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     RefOrd.PartialComparison f,
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
     Domain f ~ Imprecision (Domain f),
     solvingInfo ~ (Domain f, Maybe [Domain f]), 
     Show f, Show (Domain f), Show (Var f))
    =>
    Bool {-^ should try to shrink wrap parameterised intermediate values ? -}
    ->
    SizeLimits f {-^ size limits for all function -} ->
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
    Domain f {-^ initial widening @delta@ -}  ->
    Int {-^ @m@ -} -> 
    Domain f {-^ step size @s@ -} -> 
    Imprecision (Domain f) {-^ split improvement threshold @eps@ -} ->
    ODEIVP f
    ->
    (
        Maybe [Domain f]
    ,
        (
            SplittingInfo solvingInfo (solvingInfo, Maybe (Imprecision (Domain f)))
        )
    )
solveUncertainValueExactTimeSplitPEval
        doShringWrap
            sizeLimits effSizeLims effCompose effEval effInteg effDeriv effInclFn 
            effAddFn effMultFn effAbsFn effMinmaxFn 
            effDivFnInt effAddFnDom effMultFnDom effDom
                delta m minStepSize splitImprovementThreshold
                    (odeivpG :: ODEIVP f)
    | (tStart <? t0End) == Just True =
        error "aern-ivp: solveUncertainValueExactTime called with an uncertain time IVP"
    | otherwise =
        case solve odeivpG of
            (maybeResFnVec, splittingInfo) ->
                (fmap (map getRange) maybeResFnVec, splittingInfo)
    where
    tVar = odeivp_tVar odeivpG
    tStart, t0End :: Domain f
    tStart = odeivp_tStart odeivpG
    t0End = odeivp_t0End odeivpG
    componentNames = odeivp_componentNames odeivpG

    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
--    effImpr = ArithInOut.rrEffortImprecision sampleDom effDom
    sampleDom :: Domain f
    sampleDom = tStart

    
    getRange :: f -> Domain f 
    getRange fn =
        evalAtPointOutEff effEval (getDomainBox fn) fn

    solve odeivp =
        solveODEIVPBySplittingT
            directSolver measureResultImprecision 
                (makeFnVecFromParamInitialValuesOut effAddFn effMultFn effSizeLims componentNames)
                effDom splitImprovementThreshold minStepSize
                    odeivp

    measureResultImprecision :: [f] -> Domain f
    measureResultImprecision fnVec =
        let ?addInOutEffort = effAddDom in
        foldl1 (<+>) $ map perComp fnVec
        where
        perComp fn =
            let ?addInOutEffort = effAddFn in
            getRange $ fnR <-> fnL
            where
            (fnL, fnR) = RefOrd.getEndpointsOutWithDefaultEffort fn
            
    directSolver odeivp =
        case maybeIterations of
            Just iterations ->
                let paramValuesAtEnd = partiallySubst $ iterations !! m in
                let valuesAtEnd = fst $ evalAtEndTimeVec effEval tVar tEnd $ iterations !! m in
                (Just paramValuesAtEnd, (tEnd, Just valuesAtEnd))
            Nothing -> (Nothing, (tEnd, Nothing))
        where
        partiallySubst fns 
            | doShringWrap =
                shrinkWrap 
                    effCompose effEval effDeriv effAddFn effAbsFn effMinmaxFn 
                        effDivFnInt effAddFnDom effMultFnDom effDom $
                            parameterisedMidValue
            | otherwise = parameterisedMidValue
            where
            parameterisedMidValue =
                fst $ partiallyEvalAtEndTimeOutInVec tVar tEnd (fns, fns)
        maybeIterations :: Maybe [[f]]
        maybeIterations =
            solveUncertainValueExactTime
                sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
                    delta
                        odeivp
        tEnd = odeivp_tEnd odeivp
                
solveUncertainValueExactTime ::
    (CanAddVariables f,
     CanEvaluate f,
     CanCompose f,
     HasConstFns f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike(Domain f), 
     Show f, Show (Domain f))
    =>
    SizeLimits f ->
    CompositionEffortIndicator f ->
    IntegrationEffortIndicator f ->
    RefOrd.PartialCompareEffortIndicator f ->
    ArithInOut.AddEffortIndicator f ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    Domain f {-^ initial widening @delta@ -}  ->
    ODEIVP f ->
    Maybe [[f]] {-^ sequence of enclosures with domain @T x D@ produced by the Picard operator -}
solveUncertainValueExactTime
        sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
        delta
        odeivp
    | (tStart <? t0End) == Just True = 
        error "aern-ivp: solveUncertainValueExactTime called with an uncertain time IVP"
    | otherwise =
        case evalCatchAERNExceptions "solveUncertainValueExactTime" result of
            Right res -> res
            _ -> Nothing
    where
    result =
        case findEnclosure (40 :: Int) $ iterate picard initialAttemptFns of
            Just firstEnclosure ->
                Just $ iterate picard firstEnclosure
            Nothing -> Nothing
    field = odeivp_field odeivp
    tVar = odeivp_tVar odeivp
    tStart = odeivp_tStart odeivp
    tEnd = odeivp_tEnd odeivp
    t0End = odeivp_t0End odeivp
    initialValuesFnVec =
        map (composeVarOutEff effCompose tVar tStartFn) initialValuesFnVecWithT
        where
        tStartFn = newConstFnFromSample sampleFn tStart
        (sampleFn : _) = initialValuesFnVecWithT
    initialValuesFnVecWithT =
        odeivp_makeInitialValueFnVec odeivp sizeLimits tVar timeDomain
    
    timeDomain =
        RefOrd.fromEndpointsOutWithDefaultEffort (tStart, tEnd)

    effJoinDom = ArithInOut.rrEffortJoinMeet sampleDom effDom
    sampleDom = tStart
    
        
    initialAttemptFns =
        map widenFn initialValuesFnVec
        where
        widenFn fn =
            let ?mixedAddInOutEffort = effAddFnDom in
            fn <+>| wideningInterval 
        wideningInterval =
            let ?joinmeetEffort = effJoinDom in
            (neg delta) </\> delta
    findEnclosure maxIter (fn1Vec : fn2Vec : rest)
        | maxIter > 0 =
            case fn2RefinesFn1 of
                True -> Just fn2Vec
                _ ->
--                    unsafePrint
--                    (
--                        "solveUncertainValueExactTime: findEnclosure: not yet enclosing:"
--                        ++ "\n fn1Vec = " ++ (show $ evalAtEndTimeVec fn1Vec)
--                        ++ "\n fn2Vec = " ++ (show $ evalAtEndTimeVec fn2Vec)
--                        ++ "\n fn1Vec at end time = " ++ (show $ evalAtEndTimeVec fn1Vec)
--                        ++ "\n fn2Vec at end time = " ++ (show $ evalAtEndTimeVec fn2Vec)
--                    ) $
                    findEnclosure (maxIter - 1) (fn2Vec : rest)
        where
        fn2RefinesFn1 =
            let ?pCompareEffort = effInclFn in
            and $ map (== (Just True)) $ zipWith (|<=?) fn1Vec fn2Vec
    findEnclosure _ _  =
        Nothing
--        error "aern-picard: solveUncertainValueExactTime failed to find enclosure"
    picard xvec = 
--        unsafePrint
--        (
--            "solveUncertainValueExactTime: picard:"
--            ++ "\n tEnd = " ++ (show tEnd)
--            ++ "\n initialValuesFnVec = " ++ (show initialValuesFnVec)
--            ++ "\n xvec = " ++ (show xvec)
--            ++ "\n xdvec = " ++ (show xdvec)
--            ++ "\n result = " ++ (show result)
----            ++ "\n xvec at end time = " ++ (show $ evalAtEndTimeVec tVar tEnd xvec)
----            ++ "\n xdvec at end time = " ++ (show $ evalAtEndTimeVec tVar tEnd xdvec)
----            ++ "\n result at end time = " ++ (show $ evalAtEndTimeVec tVar tEnd result)
--        ) $
        result
        where
        result =
            let ?addInOutEffort = effAddFn in 
            zipWith (<+>) primitFn initialValuesFnVec
        primitFn = map picardFn xdvec
        xdvec = field xvec
        picardFn xdi =
            primitiveFunctionOutEff effInteg xdi tVar

