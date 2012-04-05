{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ScopedTypeVariables #-}
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

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

--import qualified Numeric.AERN.NumericOrder as NumOrd
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
     CanCompose f,
     HasProjections f,
     HasConstFns f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f),
     Domain f ~ Imprecision (Domain f),
     solvingInfo ~ (Domain f, Maybe ([Domain f], [Domain f])), 
     Show f, Show (Domain f), Show (Var f))
    =>
    SizeLimits f {-^ size limits for all function -} ->
    CompositionEffortIndicator f ->
    EvaluationEffortIndicator f ->
    IntegrationEffortIndicator f ->
    RefOrd.PartialCompareEffortIndicator f ->
    ArithInOut.AddEffortIndicator f ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    Domain f {-^ initial widening @delta@ -}  ->
    Int {-^ @m@ -} -> 
    Domain f {-^ step size @s@ -} -> 
    Imprecision (Domain f) {-^ split improvement threshold @eps@ -} ->
    ODEIVP f
    ->
    (
        Maybe ([Domain f], [Domain f])
    ,
        (
            SplittingInfo solvingInfo (solvingInfo, Maybe (Imprecision (Domain f)))
        ,
            SplittingInfo solvingInfo (solvingInfo, Maybe (Imprecision (Domain f)))
        )
    )
solveUncertainValueExactTimeSplit
        sizeLimits effCompose effEval effInteg effInclFn effAddFn effAddFnDom effDom
            delta m minStepSize splitImprovementThreshold
                odeivpG
    | (odeivp_tStart odeivpG <? odeivp_t0End odeivpG) == Just True =
        error "aern-ivp: solveUncertainValueExactTime called with an uncertain time IVP"
    | otherwise =
        solve odeivpG
    where
    tVar = odeivp_tVar odeivpG
    componentNames = odeivp_componentNames odeivpG

    solve odeivp =
        solveODEIVPBySplittingT
            directSolver (makeFnVecFromInitialValues componentNames)
                effDom splitImprovementThreshold minStepSize
                    odeivp

    directSolver odeivp =
        case maybeIterations of
            Just iterations ->
                let valuesAtEnd = evalAtEndTimeVec effEval tVar tEnd $ iterations !! m in
                (Just valuesAtEnd, (tEnd, Just valuesAtEnd))
            Nothing -> (Nothing, (tEnd, Nothing))
        where
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

