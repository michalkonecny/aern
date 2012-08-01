{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Solver.Events.SplitNearEvents
    Description :  hybrid system simulation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Hybrid system simulation with splitting based on event localisation.
-}

module Numeric.AERN.IVP.Solver.Events.SplitNearEvents
(
    solveHybridIVP_UsingPicardAndEventTree_SplitNearEvents
)
where

import Numeric.AERN.IVP.Solver.Events.Aggregate
import Numeric.AERN.IVP.Solver.Picard.UncertainValue

import Numeric.AERN.IVP.Specification.Hybrid
import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Solver.Bisection

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration
import Numeric.AERN.RmToRn.Differentiation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.Measures
--import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import qualified Data.Map as Map
--import qualified Data.Set as Set
--import qualified Data.List as List

import Numeric.AERN.Misc.Debug
_ = unsafePrint

solveHybridIVP_UsingPicardAndEventTree_SplitNearEvents ::
    (CanAddVariables f,
     CanRenameVariables f,
     CanEvaluate f,
     CanCompose f,
     CanChangeSizeLimits f,
     CanPartiallyEvaluate f,
     HasProjections f,
     HasConstFns f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     NumOrd.RefinementRoundedLattice f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     RoundedFakeDerivative f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMultiply f,
     ArithInOut.RoundedMixedDivide f Int,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedMixedMultiply f (Domain f),
     ArithUpDn.RoundedAbs f,
     NumOrd.RoundedLattice f,
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f),
     Domain f ~ Imprecision (Domain f),
     solvingInfo ~ (Domain f, Maybe (HybridSystemUncertainState (Domain f)), [(HybSysMode, EventInfo f)]),
     Show f, Show (Domain f), Show (Var f), Eq (Var f))
    =>
    SizeLimits f {-^ size limits for all function -} ->
    SizeLimitsChangeEffort f ->
    PartialEvaluationEffortIndicator f ->
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
    ArithInOut.RoundedRealEffortIndicator (Domain f) ->
    Domain f {-^ initial widening @delta@ -}  ->
    Int {-^ @m@ -} -> 
    Var f {-^ @t0@ - the initial time variable -} ->
    Domain f {-^ min step size @s@ -} -> 
    Domain f {-^ max step size @s@ -} -> 
    Imprecision (Domain f) {-^ split improvement threshold @eps@ -} ->
    HybridIVP f
    ->
    (
        Maybe (HybridSystemUncertainState (Domain f))
    ,
        [solvingInfo]
    )
solveHybridIVP_UsingPicardAndEventTree_SplitNearEvents
        sizeLimits effSizeLims effPEval effCompose effEval effInteg effDeriv effInclFn 
            effAddFn effMultFn effAbsFn effMinmaxFn 
            effDivFnInt effAddFnDom effMultFnDom effDom
            delta m t0Var minStepSize maxStepSize splitImprovementThreshold
                hybivpG
    = 
    solve hybivpG
    where
    solve hybivp =
        solveHybridIVP_SplitNearEvents
            solveHybridNoSplitting
            solveODEWithSplitting
                effDom splitImprovementThreshold minStepSize maxStepSize
                    hybivp

    solveODEWithSplitting =
        solveODEIVPUncertainValueExactTime_UsingPicard_Bisect
            shouldWrap shouldShrinkWrap
                sizeLimits effSizeLims effCompose effEval effInteg effDeriv effInclFn 
                effAddFn effMultFn effAbsFn effMinmaxFn 
                effDivFnInt effAddFnDom effMultFnDom effDom
                    delta m minStepSize splitImprovementThreshold
        where
        shouldWrap = True
        shouldShrinkWrap = False

    solveHybridNoSplitting hybivp =
        (maybeFinalStateWithInvariants, (tEnd, maybeFinalStateWithInvariants, modeEventInfoList))
        where
        tEnd = hybivp_tEnd hybivp
        maybeFinalStateWithInvariants
            = fmap filterInvariants maybeFinalState
            where
            filterInvariants st =
                Map.mapWithKey filterInvariantsVec st
                where
                filterInvariantsVec mode vec =
                    invariant vec
                    where
                    Just invariant =
                        Map.lookup mode modeInvariants
        modeInvariants = hybsys_modeInvariants $ hybivp_system hybivp
        (maybeFinalState, modeEventInfoList) = 
            solveHybridIVP_UsingPicardAndEventTree
                sizeLimits effPEval effCompose effEval effInteg effInclFn effAddFn effMultFn effAddFnDom effDom
                     20
                        delta m
                            t0Var
                                hybivp

solveHybridIVP_SplitNearEvents ::
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
     HasAntiConsistency (Domain f),
     Domain f ~ Imprecision (Domain f),
     Show f, Show (Domain f),
     solvingInfoODE ~ (Maybe [f], (Domain f, Maybe [Domain f])))
    =>
    (HybridIVP f -> (Maybe (HybridSystemUncertainState (Domain f)), solvingInfo))
        -- ^ solver to use on small segments that may contain events  
    ->
    (ODEIVP f -> (Maybe [Domain f], BisectionInfo solvingInfoODE (solvingInfoODE, prec)))
        -- ^ solver to use on large segments before event localisation  
    ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) 
    ->
    Imprecision (Domain f) -- ^ splitting improvement threshold
    ->
    Domain f -- ^ minimum segment length  
    ->
    Domain f -- ^ maximum segment length  
    ->
    (HybridIVP f)  -- ^ problem to solve
    ->
    (
        Maybe (HybridSystemUncertainState (Domain f))
    ,
        [solvingInfo]
    )
solveHybridIVP_SplitNearEvents
        solveHybridNoSplitting
        solveODEWithSplitting
            effDom splitImprovementThreshold minStepSize maxStepSize 
                hybivpG 
    =
    splitSolve hybivpG
    {-
        overview:
        
        (1) apply solveODENoSplitting over T_1 = T, T/2, T/4, ... (here assuming \leftendpoint{T} = 0)
            until it succeeds or the minimum step size is reached
        (2) locate the first event on T_1 within T_ev \subseteq T_1, set t_R = \rightendpoint{T_ev}
            or if no event, set t_R = \rightendpoint{T_1} and compute the value A_R at t_R
        (3) investigate whether bisecting T_1 leads to a substantial improvement in T_e/A_R
            and if so, halve the size of T_1 and go to (2)
        (4) if the occurrence of an event is uncertain and 
            t_R = \rightendpoint{T_1} and 2*\length{T_e} < \length{T_1}, 
            halve T_1 and compute t_R and A_R for the new T_1
        (5) if we have T_e, apply solveHybridNoSplitting on T_e to compute value A_R at t_R
        (6) if t_R < \rightendpoint{T}, 
            recursively apply this computation on the interval [t_R, \rightendpoint{T}]
    -}
    where
    splitSolve hybivp =
        undefined
        -- TODO: complete solveHybridIVPBySplittingTNearEvents

----        unsafePrint
----        (
----            "solveHybridIVPByBisectingT: splitSolve: "
----            ++ "tStart = " ++ show tStart
----            ++ "tEnd = " ++ show tEnd
----        ) $
--        result2
--        where
--        result2
--            | belowStepSize = directComputation
--            | aboveMaxStepSize = splitComputation
--            | directComputationFailed = splitComputation
--            | otherwise = 
--                case maybeSplitImprovement of
--                    Just improvementBy 
--                        | (improvementBy >? splitImprovementThreshold) /= Just True -> 
--                            directComputation -- split once computations succeeded but brought no noticeable improvement
--                    _
--                        | splitComputationFailed -> directComputation
--                        | otherwise -> splitComputation -- splitting either brought noticeable improvement or some computation failed 
--        tStart = hybivp_tStart hybivp
--        tEnd = hybivp_tEnd hybivp
--        
--        belowStepSize =
--            let ?addInOutEffort = effAddDom in
--            ((tEnd <-> tStart) >? minStepSize) /= Just True
--        aboveMaxStepSize =
--            let ?addInOutEffort = effAddDom in
--            ((tEnd <-> tStart) <? maxStepSize) /= Just True
--
--        directComputation =
----            unsafePrint
----            (
----                "solveHybridIVPByBisectingT: completed time " ++ show tEnd
----            ) $
--            case maybeDirectResult of
--                Just resultOut 
--                    | otherwise -> (Just resultOut, BisectionNoSplit directInfo)
--                _ -> (Nothing, BisectionNoSplit directInfo) 
--        (maybeDirectResult, directInfo) = solver depth hybivp
--        directComputationFailed =
--            case maybeDirectResult of Just _ -> False; _ -> True
--        
--        splitOnceComputation = -- needed only to decide whether splitting is benefitial, the result is then discarded
--            case solver (depth + 1) hybivpL of
--                (Just midState, _) ->
--                    case solver (depth + 1) hybivpR of
--                        (Just endStateOut, _) -> Just endStateOut 
--                        _ -> Nothing
--                    where
--                    hybivpR =
--                        hybivp
--                        {
--                            hybivp_tStart = tMid,
--                            hybivp_initialStateEnclosure = midState
--                        }
--                _ -> Nothing
--                
--        (splitComputation, splitComputationFailed) =
--            (
--                (maybeState, BisectionSplit (directInfo, maybeSplitImprovement) infoL maybeInfoR)
--            , 
--                case maybeState of Just _ -> False; _ -> True
--            )
--            where
--            (maybeMidState, infoL) =
--                splitSolve (depth + 1) hybivpL
--            (maybeState, maybeInfoR) =
--                case maybeMidState of
--                    Just midState ->
--                        case splitSolve (depth + 1) hybivpR of
--                            (maybeState2, infoR) -> (maybeState2, Just infoR)
--                        where
--                        hybivpR =
--                            hybivp
--                            {
--                                hybivp_tStart = tMid,
--                                hybivp_initialStateEnclosure = midState
--                            }
--                    Nothing -> (Nothing, Nothing)
--        hybivpL =
--            hybivp
--            {
--                hybivp_tEnd = tMid
--            }
--        tMid = 
--            let ?addInOutEffort = effAddDom in
--            let ?mixedDivInOutEffort = effDivDomInt in
--            (tStart <+> tEnd) </>| (2 :: Int)
--        
--        maybeSplitImprovement =
--            case (maybeDirectResult, splitOnceComputation) of
--                (Just directResult, Just splitOnceResult) -> 
--                    Just $ measureImprovementState sampleDom effDom directResult splitOnceResult
--                _ -> Nothing
--
--    effAddDom = ArithInOut.fldEffortAdd sampleDom $ ArithInOut.rrEffortField sampleDom effDom
--    effDivDomInt = 
--        ArithInOut.mxfldEffortDiv sampleDom (1 :: Int) $ 
--            ArithInOut.rrEffortIntMixedField sampleDom effDom
----    effRefComp = ArithInOut.rrEffortRefComp sampleDom effDom
--    sampleDom = hybivp_tStart hybivpG
----    effMinmax = ArithInOut.rrEffortMinmaxInOut sampleDom effDom
--    
----    effImpr = ArithInOut.rrEffortImprecision sampleDom effDom
----    sampleImpr = imprecisionOfEff effImpr sampleDom
----    effAddImpr = ArithInOut.fldEffortAdd sampleImpr $ ArithInOut.rrEffortImprecisionField sampleDom effDom
                                