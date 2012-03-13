{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Picard
    Description :  IVP solvers using Picard operators
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Integrators of function enclosures.
-}

module Numeric.AERN.RmToRn.Picard 
(
    solveUncertainValueExactTime,
    solveUncertainValueExactTimeSplit,
    solveUncertainValueUncertainTime
)
where

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

solveUncertainValueExactTime ::
    (CanAddVariables f,
     CanEvaluate f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike(Domain f), 
     fvec ~ [f],
     Show f, Show (Domain f))
    =>
    IntegrationEffortIndicator f ->
    RefOrd.PartialCompareEffortIndicator f ->
    ArithInOut.AddEffortIndicator f ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    Var f {-^ @t@ - the time variable -} ->
    Domain f {-^ @TL@ - initial time -} ->
    Domain f {-^ @TR@ - end of the time interval of interest -} ->
    fvec {-^ @a@ - functions giving the initial value parametrised by domain @D@ -}  ->
    (fvec -> fvec) {-^ the approximate vector field, transforming vectors of functions, all functions have the same domain -} ->
    Domain f {-^ initial widening @delta@ -}  ->
    Maybe [fvec] {-^ sequence of enclosures with domain @T x D@ produced by the Picard operator -}
solveUncertainValueExactTime
        effInteg effInclFn effAddFn effAddFnDom effDom
        tVar tStart tEnd (initialValuesFns :: [f]) field delta
    =
    case findEnclosure (40 :: Int) $ iterate picard initialAttemptFns of
        Just firstEnclosure ->
            Just $ iterate picard firstEnclosure
        Nothing -> Nothing
    where
    timeDomain =
        RefOrd.fromEndpointsOutWithDefaultEffort (tStart, tEnd)

    effJoinDom = ArithInOut.rrEffortJoinMeet sampleDom effDom
    sampleDom = tStart
    
    firstEnclosure =
        findEnclosure (40 :: Int) $ iterate picard initialAttemptFns
    initialValuesFnsWithT =
        map (addVariablesFront [(tVar, timeDomain)]) initialValuesFns
    initialAttemptFns =
        map widenFn initialValuesFnsWithT
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
            zipWith (<+>) primitFn initialValuesFnsWithT
        primitFn = map picardFn xdvec
        xdvec = field xvec
        picardFn xdi =
            primitiveFunctionOutEff effInteg xdi tVar
    evalAtEndTimeVec fnVec =
        map evalAtEndTimeFn fnVec
    evalAtEndTimeFn :: f -> Domain f
    evalAtEndTimeFn fn =
        evalAtPointOutEff (evaluationDefaultEffort fn) endTimeArea fn
        where
        endTimeArea :: DomainBox f
        endTimeArea = insertVar tVar timeDomainR $ getDomainBox fn
    (_, timeDomainR) = RefOrd.getEndpointsOutWithDefaultEffort timeDomain 
        
        
solveUncertainValueExactTimeSplit ::
    (CanAddVariables f,
     CanEvaluate f,
     HasProjections f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike(Domain f), 
     fvec ~ [f],
     Show f, Show (Domain f))
    =>
    f {-^ sample function with domain @D@ for parametrising the inital values -} ->
    [Var f] ->
    IntegrationEffortIndicator f ->
    RefOrd.PartialCompareEffortIndicator f ->
    ArithInOut.AddEffortIndicator f ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    Var f {-^ @t@ - the time variable -} ->
    Domain f {-^ @TL@ - initial time -} ->
    Domain f {-^ @TR@ - end of the time interval of interest -} ->
    [Domain f] {-^ @A@ - uncertain values at time tStart -}  ->
    (fvec -> fvec) {-^ the approximate vector field, transforming vectors of functions, all functions have the same domain -} ->
    Domain f {-^ initial widening @delta@ -}  ->
    Int -> 
    Domain f {-^ step size @s@ -}  
    ->
    (
        Maybe [Domain f] 
    ,
        [(Domain f, [Domain f])]
    ) {-^ value approximations at time tEnd and intermediate values at various time points -}
solveUncertainValueExactTimeSplit
        (sampleF :: f) componentNames
        effInteg effInclFn effAddFn effAddFnDom effDom
        tVar tStartG tEndG initialValuesG field delta
        m stepSize
    =
    solve tStartG tEndG initialValuesG
    where
    effRefComp = ArithInOut.rrEffortRefComp sampleDom effDom
    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
    effDivDomInt = 
        ArithInOut.mxfldEffortDiv sampleDom (1 :: Int) $ 
            ArithInOut.rrEffortIntMixedField sampleDom effDom
    sampleDom = tStartG

    solve tStart tEnd initialValues
        | belowStepSize = directComputation
        | not splitImproves = directComputation
        | otherwise = splitComputation
        where
        belowStepSize =
--            unsafePrintReturn ("belowStepSize = ") $
            let ?addInOutEffort = effAddDom in
            ((tEnd <-> tStart) >? stepSize) /= Just True
            
        directComputation =
            case maybeIterations of
                Just iterations ->
                    let valuesAtEnd = evalAtEndTimeVec $ iterations !! m in
                    (Just valuesAtEnd, [(tEnd, valuesAtEnd)])
                Nothing -> (Nothing, [])
            where
            maybeIterations =
                solveUncertainValueExactTime
                        effInteg effInclFn effAddFn effAddFnDom effDom
                        tVar tStart tEnd initialValuesFns field delta
        initialValuesFns =
            map initialValueFn componentNames
        initialValueFn var =
            newProjection sizeLimits dombox var
        dombox =
            fromList $ zip vars initialValues
        sizeLimits = getSizeLimits sampleF
        vars = getVars $ getDomainBox sampleF
        
        splitComputation =
            case solve tStart tMid initialValues of
                (Just midValues, intermediateValuesL) -> 
                    case solve tMid tEnd midValues of
                        (Just endValues, intermediateValuesR) ->
                            (Just endValues, intermediateValuesL ++ intermediateValuesR)
                        (Nothing, intermediateValuesR) ->
                            (Nothing, intermediateValuesL ++ intermediateValuesR)
                failedLeftComputation -> failedLeftComputation
            where
            tMid = 
                let ?addInOutEffort = effAddDom in
                let ?mixedDivInOutEffort = effDivDomInt in
                (tStart <+> tEnd) </>| (2 :: Int)
        
        splitImproves =
            case directComputation  of
                (Just directResult, _) ->
                    case splitComputation of
                        (Just splitResult, _) ->
                            directResult `oneWorseThan` splitResult
                        _ -> False
                _ -> False
            where
            oneWorseThan vec1 vec2 =
                or $ zipWith worseThan vec1 vec2
            worseThan encl1 encl2 =
--                unsafePrintReturn
--                (
--                    "worseThan: "
--                    ++ "\n encl1 = " ++ show encl1
--                    ++ "\n encl2 = " ++ show encl2
--                    ++ "\n encl1 |<? encl2 = "
--                ) $
                let ?pCompareEffort = effRefComp in
                (encl1 |<? encl2) == Just True 
    
        evalAtEndTimeVec fnVec =
            map evalAtEndTimeFn fnVec
        evalAtEndTimeFn :: f -> Domain f
        evalAtEndTimeFn fn =
            evalAtPointOutEff (evaluationDefaultEffort fn) endTimeArea fn
            where
            endTimeArea :: DomainBox f
            endTimeArea = insertVar tVar tEnd $ getDomainBox fn
    
    
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
     RefOrd.IntervalLike(Domain f), 
     fvec ~ [f],
     Show f, Show (Domain f))
    =>
    CompositionEffortIndicator f ->
    IntegrationEffortIndicator f ->
    RefOrd.PartialCompareEffortIndicator f ->
    NumOrd.MinmaxInOutEffortIndicator f ->
    ArithInOut.AddEffortIndicator f ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    f {-^ sample function without variable @t0@ -} ->
    Var f {-^ @t@ - the time variable -} ->
    Domain f {-^ @TL@ - initial time -} ->
    Domain f {-^ @TR@ - end of the time interval of interest -} ->
    Var f {-^ @t0@ - the initial time variable -} ->
    Domain f {-^ @T0R@ - end of the domain of the initial time variable -} ->
    fvec {-^ @g@ - functions giving the initial value parametrised by domain @T0 x D@ -}  ->
    (fvec -> fvec) {-^ the approximate vector field, transforming vectors of functions, all functions have the same domain -} ->
    Domain f {-^ initial widening @delta@ -}  ->
    Maybe [fvec] {-^ sequence of enclosures with domain @T x D@ produced by the Picard operator -}
solveUncertainValueUncertainTime
        effCompose effInteg effInclFn effMinmax effAddFn effAddFnDom effDom
        sampleFnWithoutT0
        tVar tStart tEnd t0Var t0End 
        (initialValuesFns :: [f]) field delta
    =
    case solveWithExactTime of
        Just _ ->
--            unsafePrint
--            (
--                "solveUncertainValueUncertainTime: "
--                ++ "\n t0DomainFnBelowT = " ++ show t0DomainFnBelowT
--                ++ "\n at time " ++ show tEnd ++ ":"
--                ++ "\n enclosuresWithTT0 = " ++ (show $ take 3 $ map evalAtEndTimeVec enclosuresWithTT0) 
--                ++ "\n enclosuresShifted = " ++ (show $ take 3 $ map evalAtEndTimeVec enclosuresShifted) 
--                ++ "\n enclosuresWithoutT0 = " ++ (show $ take 3 $ map evalAtEndTimeVec enclosuresWithoutT0) 
--            )
            Just enclosuresWithoutT0
        Nothing -> Nothing
    where
    -- perform eliminating substitution: t0 |-> min (T0 , t):
    enclosuresWithoutT0 =
        map (map (composeVarOutEff effCompose t0Var t0DomainFnBelowT)) $
        enclosuresShifted
    -- perform non-eliminating substitution: t |-> t - t0 + initialTime:
    enclosuresShifted =
        map (map (composeVarOutEff effCompose tVar tShifted)) enclosuresWithTT0
    (Just enclosuresWithTT0) = solveWithExactTime
    -- compute the enclosure parameterised by t and t0:
    solveWithExactTime =
        solveUncertainValueExactTime
            effInteg effInclFn effAddFn effAddFnDom effDom
            tVar tStart tEndAdj initialValuesFns
            field delta
    timeDomain =
        RefOrd.fromEndpointsOutWithDefaultEffort (tStart, tEndAdj)
    tEndAdj =
        let ?addInOutEffort = effAddDom in 
        tEnd <+> t0End <-> tStart
    t0Domain = RefOrd.fromEndpointsOutWithDefaultEffort (tStart, t0End)
    t0DomainFnBelowT =
--        NumOrd.minOutEff effMinmax t0DomainFn tFn
        RefOrd.fromEndpointsOutWithDefaultEffort (tStartFn, tFn) 
            -- WARNING: assuming T = T_0!
            -- HACK to make event detection work while min is broken; 
        where
        tStartFn =
            newConstFnFromSample sampleFnWithoutT0 tStart
        t0DomainFn =
            newConstFnFromSample sampleFnWithoutT0 t0Domain
        tFn = 
            newProjectionFromSample sampleFnWithoutT0 tVar

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

    evalAtEndTimeVec fnVec =
        map evalAtEndTimeFn fnVec
        
    evalAtEndTimeFn :: f -> Domain f
    evalAtEndTimeFn fn =
        evalAtPointOutEff (evaluationDefaultEffort fn) endTimeArea fn
        where
        endTimeArea :: DomainBox f
        endTimeArea = insertVar tVar tEnd $ getDomainBox fn
    
solveUncertainValueUncertainTimeSplit 
        effCompose effInteg effInclFn effMinmax effAddFn effAddFnDom effDom
        sampleFnWithoutT0
        tVar tStart tEnd t0Var t0End 
        (initialValuesFns :: [f]) field delta
    =
    undefined
