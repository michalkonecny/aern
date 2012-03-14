{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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

--import Numeric.AERN.Misc.Debug
        
solveUncertainValueExactTimeSplit ::
    (CanAddVariables f,
     CanEvaluate f,
     CanCompose f,
     HasProjections f,
     HasConstFns f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike(Domain f), 
     Show f, Show (Domain f))
    =>
    f {-^ sample function used only to carry size limits, its domain is irrelevant -} ->
    CompositionEffortIndicator f ->
    IntegrationEffortIndicator f ->
    RefOrd.PartialCompareEffortIndicator f ->
    ArithInOut.AddEffortIndicator f ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    ODEIVP f ->
    Domain f {-^ initial widening @delta@ -}  ->
    Int {-^ @m@ -} -> 
    Domain f {-^ step size @s@ -} -> 
    Imprecision (Domain f) {-^ split improvement threshold @eps@ -}
    ->
    (
        Maybe [Domain f] 
    ,
        [(Domain f, [Domain f])]
    ) {-^ value approximations at time tEnd and intermediate values at various time points -}
solveUncertainValueExactTimeSplit
        (sampleF :: f)
        effCompose effInteg effInclFn effAddFn effAddFnDom effDom
        odeivpG 
        delta m stepSize splitImprovementThreshold
    | (odeivp_tStart odeivpG <? odeivp_t0End odeivpG) == Just True =
        error "aern-ivp: solveUncertainValueExactTime called with an uncertain time IVP"
    | otherwise =
        solve odeivpG
    where
    effImpr = ArithInOut.rrEffortImprecision sampleDom effDom
    effAddImpr = ArithInOut.fldEffortAdd sampleImpr $ ArithInOut.rrEffortImprecisionField sampleDom effDom
    sampleImpr = imprecisionOfEff effImpr sampleDom
    effRefComp = ArithInOut.rrEffortRefComp sampleDom effDom
    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
    effDivDomInt = 
        ArithInOut.mxfldEffortDiv sampleDom (1 :: Int) $ 
            ArithInOut.rrEffortIntMixedField sampleDom effDom
    sampleDom = odeivp_tStart odeivpG
    
    tVar = odeivp_tVar odeivpG
    componentNames = odeivp_componentNames odeivpG

    solve odeivp
        | belowStepSize = directComputation
        | not splitImproves = directComputation
        | otherwise = splitComputation
        where
        tStart = odeivp_tStart odeivp
        tEnd = odeivp_tEnd odeivp
        
        belowStepSize =
--            unsafePrintReturn ("belowStepSize = ") $
            let ?addInOutEffort = effAddDom in
            ((tEnd <-> tStart) >? stepSize) /= Just True
            
        directComputation =
            case maybeIterations of
                Just iterations ->
                    let valuesAtEnd = evalAtEndTimeVec tVar tEnd $ iterations !! m in
                    (Just valuesAtEnd, [(tEnd, valuesAtEnd)])
                Nothing -> (Nothing, [])
            where
            maybeIterations =
                solveUncertainValueExactTime
                        effCompose effInteg effInclFn effAddFn effAddFnDom effDom
                        delta
                        odeivp

        splitComputation =
            case solve odeivpL of
                (Just midValues, intermediateValuesL) -> 
                    case solve odeivpR of
                        (Just endValues, intermediateValuesR) ->
                            (Just endValues, intermediateValuesL ++ intermediateValuesR)
                        (Nothing, intermediateValuesR) ->
                            (Nothing, intermediateValuesL ++ intermediateValuesR)
                    where
                    odeivpR =
                        odeivp
                        {
                            odeivp_tStart = tMid,
                            odeivp_t0End = tMid, -- exact initial time
                            odeivp_makeInitialValueFnVec = makeMidValuesFnVec
                        }
                    makeMidValuesFnVec =
                        makeFnVecFromInitialValues sampleF componentNames midValues
                failedLeftComputation -> failedLeftComputation
            where
            odeivpL =
                odeivp
                {
                    odeivp_tEnd = tMid
                }
            tMid = 
                let ?addInOutEffort = effAddDom in
                let ?mixedDivInOutEffort = effDivDomInt in
                (tStart <+> tEnd) </>| (2 :: Int)
        
        splitImproves =
            case directComputation  of
                (Just directResult, _) ->
                    case splitComputation of
                        (Just splitResult, _) ->
                            directResult `allWorseThan` splitResult
                        _ -> False
                _ -> False
            where
            allWorseThan vec1 vec2 =
                and $ zipWith worseThan vec1 vec2
            worseThan encl1 encl2 =
--                unsafePrintReturn
--                (
--                    "worseThan: "
--                    ++ "\n encl1 = " ++ show encl1
--                    ++ "\n encl2 = " ++ show encl2
--                    ++ "\n encl1 |<? encl2 = "
--                ) $
                let ?pCompareEffort = effRefComp in
                ((encl1 |<? encl2) == Just True)
                &&
                (improvementAboveThreshold encl1 encl2)
            improvementAboveThreshold encl1 encl2 =
                let ?addInOutEffort = effAddImpr in
                ((imprecisionOfEff effImpr encl1) <-> (imprecisionOfEff effImpr encl2)
                >?
                splitImprovementThreshold)
                == Just True
                
    
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
        effCompose effInteg effInclFn effAddFn effAddFnDom effDom
        delta
        odeivp
    | (tStart <? t0End) == Just True =
        error "aern-ivp: solveUncertainValueExactTime called with an uncertain time IVP"
    | otherwise =
        case findEnclosure (40 :: Int) $ iterate picard initialAttemptFns of
            Just firstEnclosure ->
                Just $ iterate picard firstEnclosure
            Nothing -> Nothing
    where
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
        odeivp_makeInitialValueFnVec odeivp tVar timeDomain
    
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
            null $ filter (/= (Just True)) $ zipWith (|<=?) fn1Vec fn2Vec
    findEnclosure _ _  =
        Nothing
--        error "aern-picard: solveUncertainValueExactTime failed to find enclosure"
    picard xvec = 
--        unsafePrint
--        (
--            "solveUncertainValueExactTime: picard:"
--            ++ "\n tEnd = " ++ (show timeDomainR)
--            ++ "\n initialValuesFnsWithT = " ++ (show initialValuesFnsWithT)
----            ++ "\n xvec = " ++ (show xvec)
----            ++ "\n xdvec = " ++ (show xdvec)
----            ++ "\n result = " ++ (show result)
--            ++ "\n xvec at end time = " ++ (show $ evalAtEndTimeVec xvec)
--            ++ "\n xdvec at end time = " ++ (show $ evalAtEndTimeVec xdvec)
--            ++ "\n result at end time = " ++ (show $ evalAtEndTimeVec result)
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

evalAtEndTimeVec ::
    (CanEvaluate f)
    =>
    (Var f) -> 
    (Domain f) -> 
    [f] 
    -> 
    [Domain f]
evalAtEndTimeVec tVar tEnd fnVec =
    map (evalAtEndTimeFn tVar tEnd) fnVec
    
evalAtEndTimeFn ::
    (CanEvaluate f)
    =>
    (Var f) -> 
    (Domain f) -> 
    f 
    -> 
    Domain f
evalAtEndTimeFn tVar tEnd fn =
    evalAtPointOutEff (evaluationDefaultEffort fn) endTimeArea fn
    where
--    endTimeArea :: DomainBox f
    endTimeArea = insertVar tVar tEnd $ getDomainBox fn
