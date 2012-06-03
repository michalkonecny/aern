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
    solveUncertainValueExactTimeSplit
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
        

solveUncertainValueExactTimeSplit ::
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
    Bool {-^ should wrap intermediate values ? -}
    -> 
    Bool {-^ if not wrapping, should try to shrink wrap parameterised intermediate values ? -}
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
solveUncertainValueExactTimeSplit
        shouldWrap shouldShrinkWrap
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
        snd $ RefOrd.getEndpointsOutWithDefaultEffort $
        let ?addInOutEffort = effAddDom in
        foldl1 (<+>) $ map perComp fnVec
        where
        perComp fn =
            let ?addInOutEffort = effAddFn in
            getRange $ fnR <-> fnL
            where
            (fnL, fnR) = RefOrd.getEndpointsOutWithDefaultEffort fn
            
    directSolver odeivp =
        case maybeResult of
            Just (result, resultAtTEnd) ->
                let valuesAtEnd = resultAtTEnd in
                let paramValuesAtTEnd = evalAtTEnd valuesAtEnd $ result in
                (Just paramValuesAtTEnd, (tEnd, Just valuesAtEnd))
            Nothing -> (Nothing, (tEnd, Nothing))
        where
        evalAtTEnd valuesAtEnd fns
            | shouldWrap = wrappedParamValuesAtTEnd
            | shouldShrinkWrap = 
                case shrinkWrappedParamValuesAtTEnd of
                    Just fns2 ->
                        unsafePrint ("solveUncertainValueExactTimeSplit: shrink wrapping succeeded") $ 
                        fns2
                    _ | haveAnExactDomain -> 
                        unsafePrint ("solveUncertainValueExactTimeSplit: shrink wrapping failed, wrapping") $ 
                        wrappedParamValuesAtTEnd
                    _ -> 
                        unsafePrint ("solveUncertainValueExactTimeSplit: shrink wrapping failed, leaving it thick") $ 
                        thickParamValuesAtTEnd 
            | otherwise = thickParamValuesAtTEnd
            where
            wrappedParamValuesAtTEnd =
                parameteriseInitialValues sizeLimits componentNames valuesAtEnd
            haveAnExactDomain =
                or $ map isExact $ domains
                where
                isExact dom =
                    (domL ==? domR) == Just True
                    where
                    (domL, domR) = RefOrd.getEndpointsOutWithDefaultEffort dom
                domains = map snd $ toAscList $ getDomainBox fn
                (fn : _) = fns 
            shrinkWrappedParamValuesAtTEnd =
                shrinkWrap 
                    effCompose effEval effDeriv effAddFn effAbsFn effMinmaxFn 
                        effDivFnInt effAddFnDom effMultFnDom effDom $
                            thickParamValuesAtTEnd
            thickParamValuesAtTEnd =
                fst $ partiallyEvalAtEndTimeOutInVec tVar tEnd (fns, fns)
        
        maybeResult =
            fmap (untilStableButNotMoreThan m) maybeIterations
        untilStableButNotMoreThan maxIters (hInit : tInit) =
            aux 1 hInitImprecision tInit
            where
            hInitImprecision = measureResultImprecision hInit
            aux iterNo prevImprecision (h : t) 
                | iterNo >= maxIters = (h, hAtTEnd)
                | notMuchImprovement =
                     unsafePrint 
                        ("solveUncertainValueExactTimeSplit:"
                            ++ " finished after " ++ show iterNo 
                            ++ " iterations (max = " ++ show maxIters ++ ")") $
                    (h, hAtTEnd)
                | otherwise =
                    unsafePrint 
                        ("solveUncertainValueExactTimeSplit: "
                            ++ "hImprecision = " ++ show hImprecision
                        ) $
                    aux (iterNo + 1) hImprecision t
                where
                notMuchImprovement = 
                    let ?addInOutEffort = effAddDom in
                    (prevImprecision <-> hImprecision <=? splitImprovementThreshold) == Just True  
                hImprecision = measureResultImprecision h
                hAtTEnd = fst $ evalAtEndTimeVec effEval tVar tEnd h
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
        case findEnclosure (50 :: Int) $ iterate picard initialAttemptFns of
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
    findEnclosure maxIter = aux 0 
        where
        aux iterNo (fn1Vec : fn2Vec : rest)
            | iterNo <= maxIter =
                case fn2RefinesFn1 of
                    True ->
                        unsafePrint 
                            ("solveUncertainValueExactTime:" 
                                ++ " enclosure found after " ++ show iterNo ++ " iterations"
                                ++ " (max = " ++ show maxIter ++ ")") $ 
                        Just fn2Vec
                    _ ->
--                        unsafePrint
--                        (
--                            "solveUncertainValueExactTime: findEnclosure: not yet enclosing:"
--                            ++ "\n fn1Vec = " ++ (show $ evalAtEndTimeVec fn1Vec)
--                            ++ "\n fn2Vec = " ++ (show $ evalAtEndTimeVec fn2Vec)
--                            ++ "\n fn1Vec at end time = " ++ (show $ evalAtEndTimeVec fn1Vec)
--                            ++ "\n fn2Vec at end time = " ++ (show $ evalAtEndTimeVec fn2Vec)
--                        ) $
                        aux (iterNo + 1) (fn2Vec : rest)
            where
            fn2RefinesFn1 =
                let ?pCompareEffort = effInclFn in
                and $ map (== (Just True)) $ zipWith (|<=?) fn1Vec fn2Vec
        aux _ _  =
            Nothing
--            error "aern-picard: solveUncertainValueExactTime failed to find enclosure"
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

