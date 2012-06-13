{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Solver.Events
    Description :  hybrid system simulation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Hybrid system simulation.
-}

module Numeric.AERN.IVP.Solver.Events
--(
--)
where

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Specification.Hybrid
import Numeric.AERN.IVP.Solver.Picard.UncertainValue
import Numeric.AERN.IVP.Solver.Picard.UncertainTime
import Numeric.AERN.IVP.Solver.Splitting

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import qualified Data.Map as Map
import qualified Data.Set as Set
--import qualified Data.List as List

import Numeric.AERN.Misc.Debug
_ = unsafePrint

solveEventsTimeSplit ::
    (CanAddVariables f,
     CanRenameVariables f,
     CanEvaluate f,
     CanCompose f,
     CanPartiallyEvaluate f,
     HasProjections f,
     HasConstFns f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     NumOrd.RefinementRoundedLattice f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMultiply f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f),
     Domain f ~ Imprecision (Domain f),
     solvingInfo ~ (Domain f, Maybe (HybridSystemUncertainState f), [(HybSysMode, EventInfo f)]),
     Show f, Show (Domain f), Show (Var f), Eq (Var f))
    =>
    SizeLimits f {-^ size limits for all function -} ->
    PartialEvaluationEffortIndicator f ->
    CompositionEffortIndicator f ->
    EvaluationEffortIndicator f ->
    IntegrationEffortIndicator f ->
    RefOrd.PartialCompareEffortIndicator f ->
    ArithInOut.AddEffortIndicator f ->
    ArithInOut.MultEffortIndicator f ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) ->
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
        Maybe (HybridSystemUncertainState f)
    ,
        (
            SplittingInfo solvingInfo (solvingInfo, Maybe (Imprecision (Domain f)))
        )
    )
solveEventsTimeSplit
        sizeLimits effPEval effCompose effEval effInteg effInclFn effAddFn effMultFn effAddFnDom effDom
            delta m t0Var minStepSize maxStepSize splitImprovementThreshold
                hybivpG
    = solve hybivpG
    where
    solve hybivp =
        solveHybridIVPBySplittingT
            directSolver
                effDom splitImprovementThreshold minStepSize maxStepSize
                    hybivp

    directSolver depth hybivp =
        (maybeFinalStateWithInvariants, (tEnd, maybeFinalStateWithInvariants, modeEventInfoList))
        where
        tEnd = hybivp_tEnd hybivp
        maybeFinalStateWithInvariants
            = fmap filterInvariants maybeFinalState
            where
            filterInvariants st@(HybridSystemUncertainState modes vec) =
                result
                where
                _ = [st, result]
                result =
                    HybridSystemUncertainState modes $ filterInvariantsVec vec
                filterInvariantsVec vec =
                    -- for each mode, lookup the invariant and apply it, 
                    --  then take the union of all the results
                    takeUnion $ map applyModeInvariant $ Set.toList modes
                    where
                    applyModeInvariant mode =
                        invariant vec
                        where
                        Just invariant =
                            Map.lookup mode modeInvariants
                    takeUnion [] = vec
                    takeUnion list =
                        let ?joinmeetEffort = effJoin in 
                        foldl1 (zipWith (</\>)) list
                    effJoin = ArithInOut.rrEffortJoinMeet sampleDom effDom
                    (sampleDom : _) = vec
        modeInvariants = hybsys_modeInvariants $ hybivp_system hybivp
        (maybeFinalState, modeEventInfoList) = 
            solveEvents
                sizeLimits effPEval effCompose effEval effInteg effInclFn effAddFn effMultFn effAddFnDom effDom
                    (10 + 2 * depth)
                        delta m
                            t0Var
                                hybivp

solveEvents ::
    (CanAddVariables f,
     CanRenameVariables f,
     CanEvaluate f,
     CanCompose f,
     CanPartiallyEvaluate f,
     HasProjections f,
     HasConstFns f,
     RefOrd.PartialComparison f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     NumOrd.RefinementRoundedLattice f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMultiply f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f),
     RefOrd.IntervalLike (Domain f), 
     HasAntiConsistency (Domain f), 
     Show f, Show (Domain f), Show (Var f), Eq (Var f)
     )
    =>
    SizeLimits f 
    ->
    PartialEvaluationEffortIndicator f 
    ->
    CompositionEffortIndicator f 
    ->
    EvaluationEffortIndicator f 
    ->
    IntegrationEffortIndicator f 
    ->
    RefOrd.PartialCompareEffortIndicator f 
    ->
    ArithInOut.AddEffortIndicator f 
    ->
    ArithInOut.MultEffortIndicator f 
    ->
    ArithInOut.MixedAddEffortIndicator f (Domain f) 
    ->
    ArithInOut.RoundedRealEffortIndicator (Domain f)
    ->
    Int {-^ maximum number of potential events to consider -}
    ->
    Domain f {-^ initial widening @delta@ -}
    ->
    Int  
    ->
    Var f {-^ @t0@ - the initial time variable -} 
    ->
    HybridIVP f 
    ->
    (Maybe (HybridSystemUncertainState f), [(HybSysMode, (EventInfo f))])
solveEvents
        sizeLimits effPEval effCompose effEval effInteg effInclFn effAddFn effMultFn effAddFnDom effDom
            maxNodes
                delta m
                    t0Var
                        hybivp
    | givenUp =        
        (Nothing, modeEventInfoList)
    | otherwise =
        (Just finalState, modeEventInfoList)
    where
    effJoinDom = ArithInOut.rrEffortJoinMeet sampleDom effDom
    sampleDom = tStart
    tStart = hybivp_tStart hybivp
    tEnd = hybivp_tEnd hybivp
    tVar = hybivp_tVar hybivp
    hybsys = hybivp_system hybivp
    componentNames = hybsys_componentNames hybsys
    
    (givenUp, finalState) = 
        case sequence perModeMaybeFinalState of
            Just perModeFinalState ->
                (False, foldl1 (mergeHybridStates effJoinDom) perModeFinalState) 
            _ -> (True, error "internal error in solveEvents: finalState undefined")
    modeEventInfoList = 
        zip modeList perModeEventInfo 
    (perModeMaybeFinalState, perModeEventInfo) = 
        unzip $ map solveEventsOneMode modeList 
    modeList = 
        Set.toList initialModeSet
    HybridSystemUncertainState initialModeSet initialValues 
        = hybivp_initialStateEnclosure hybivp
    
    solveEventsOneMode initialMode =
        (maybeFinalState, eventInfo)
        where
--        maxNodes = 100
        
        maybeFinalState =
            case eventInfoCollectFinalStates effEval tVar tEnd eventInfo of
                Just states -> Just $ foldl1 (mergeHybridStates effJoinDom) states
                _ -> Nothing
            
        eventInfo =
            case maybeFnVecNoEvent of
                Nothing -> EventGivenUp
                Just fnVecNoEvent ->
                    esolve (EventTODO (initialMode, fnVecNoEvent))
        maybeFnVecNoEvent =
            fmap (map removeAllVarsButT) $ -- parameters have to be removed so that we can test inclusion
            fmap (!! m) $
            solveUncertainValueExactTime
                sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
                delta
                (odeivp tStart initialMode $ 
                    makeFnVecFromInitialValues componentNames initialValues)
        removeAllVarsButT fn =
            -- substitute domains for all variables except t:
            pEvalAtPointOutEff effPEval domboxNoT fn
            where
            domboxNoT =
                fromAscList $ filter ((/= tVar) . fst) varDoms
            varDoms = 
                toAscList $ getDomainBox fn

        esolve prevEventInfo =
            case addOneLayer prevEventInfo of
                (newEventInfo, _, False) -> newEventInfo -- nothing left to do
                (newEventInfo, Nothing, _) -> newEventInfo -- ie given up or reached limit
                (newEventInfo, _, _) -> esolve newEventInfo
        addOneLayer prevEventInfo =
--            unsafePrint
--            (
--                "solveEvents: solveEventsOneMode: "
--                ++ "\n tStart = " ++ show tStart  
--                ++ "\n tEnd = " ++ show tEnd  
--                ++ "\n prevEventInfo = \n" ++ showEventInfo "   " (show . id) prevEventInfo
--            ) $
            processNode 0 [] prevEventInfo
            where
            processNode nodeCountSoFar previousStates eventInfo2 = 
                case eventInfo2 of
                    EventGivenUp -> (eventInfo2, Nothing, False)
                    EventFixedPoint _ -> (eventInfo2, Just nodeCountSoFar, False)
                    EventNextSure state children ->
                        (EventNextSure state newChildren, maybeNodeCount, someChildHasChanged)
                        where
                        (newChildren, maybeNodeCount, someChildHasChanged) = 
                            processChildren (nodeCountSoFar + 1) (state : previousStates) $ 
                                Map.toAscList children
                    EventNextMaybe state children -> 
                        (EventNextMaybe state newChildren, maybeNodeCount, someChildHasChanged)
                        where
                        (newChildren, maybeNodeCount, someChildHasChanged) = 
                            processChildren (nodeCountSoFar + 1) (state : previousStates) $ 
                                Map.toAscList children
                    EventTODO state -> processState (nodeCountSoFar + 1) previousStates state
            processChildren nodeCountSoFar _previousStates [] = (Map.empty, Just nodeCountSoFar, False)
            processChildren nodeCountSoFar previousStates ((key, child) : rest) =
                 case processChildren nodeCountSoFar previousStates rest of
                    (newRest, Nothing, someChildHasChanged) -> 
                        (Map.insert key child newRest, Nothing, someChildHasChanged)
                    (newRest, Just nodeCountSoFarWithRest, someChildHasChanged) ->
                        (Map.insert key newChild newRest, newNodeCountSoFar, someChildHasChanged || hasChanged)
                        where
                        (newChild, newNodeCountSoFar, hasChanged) = 
                            processNode nodeCountSoFarWithRest previousStates child
            processState nodeCountSoFar previousStates state@(mode, fnVecBeforeEvent) 
                -- first check whether this state is included in a previous state:
                | stateShrinksPreviousOne =
                    (EventFixedPoint state, Just (nodeCountSoFar + 1), True)
                | stateExpandsPreviousOne =
                    (EventGivenUp, Nothing, True)
                | otherwise =
                -- find which events are not ruled out by fnVec and try to determine whether an event is certain
                -- for each potential event, compute an enclosure for the state at the event
                --    then enclosure for the state after that event
                -- build the event info
                -- at the same time sum up the overall events 
                --  (how? need breadth-first with size cut off - need to have partial event info to hold intermediate results;
                --   could use a zipper...)
                (constructorForNextEvents state eventTasksMap, maybeNodeCountNew, True)
                where
                stateShrinksPreviousOne =
                    or $ map (stateIncludedIn state) previousStates
                stateExpandsPreviousOne =
                    or $ map (flip stateIncludedIn state) previousStates
                stateIncludedIn (mode1, fnVec1) (mode2, fnVec2) 
                    | mode1 /= mode2 = False
                    | otherwise =
                        let ?pCompareEffort = effInclFn in
                        and $ map (== Just True) $ zipWith (|<=?) fnVec2 fnVec1 
                maybeNodeCountNew
                    | givenUp2 = Nothing
                    | nodeCountSoFar + eventCount > maxNodes = Nothing
                    | otherwise = Just $ nodeCountSoFar + eventCount
                eventCount = 
                    Map.size possibleOrCertainFirstEventsMap
                possibleOrCertainFirstEventsMap = 
                    hybsys_eventDetector hybsys mode fnVecBeforeEvent

                constructorForNextEvents 
                    | someEventCertain = EventNextSure
                    | otherwise = EventNextMaybe
                someEventCertain =
                    or $ map (\(eventCertain, _,_) -> eventCertain) $ Map.elems possibleOrCertainFirstEventsMap
                
                eventTasksMap =
                    Map.fromAscList eventTasks
                givenUp2 = 
                    or $ map eventInfoIsGivenUp (map snd eventTasks)
                eventTasks =
                    map simulateEvent $ eventKindAffectCompsAndPruneList
                simulateEvent (eventKind, affectedComponents, pruneUsingTheGuard) =
--                    case maybeFnVecAfterEventUseVT of
                    case maybeFnVecAfterEventUseBox of
                        Just fnVecAfterEvent ->
                            (eventKind, EventTODO (modeAfterEvent, fnVecAfterEvent))
                        Nothing ->
                            (eventKind, EventGivenUp)
                    where
                    (modeAfterEvent, eventSwitchingFn) =
                        case Map.lookup eventKind eventModeSwitchesAndResetFunctions of
                            Just res -> res
                            Nothing -> error $ "aern-ivp: hybrid system has no information about event kind " ++ show eventKind
                    fnVecAtEvent =
                        wrapFnVecAsBox effEval pruneUsingTheGuard $
                        eventSwitchingFn fnVecBeforeEvent
--                    maybeFnVecAfterEventUseVT = 
--                        fmap (map $ removeAllVarsButT . fst) $
--                        fmap (!! m) $
--                        solveUncertainValueUncertainTime
--                                sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
--                                    delta
--                                        t0Var $
--                                            odeivp tEnd modeAfterEvent $
--                                                makeInitialValuesFromFnVecAtEvent
--                        where
--                        makeInitialValuesFromFnVecAtEvent _sizeLimits t0Var2 _t0Domain =
--                            parametriseThickFunctions effAddFn effMultFn componentNames $
--                                map (renameVar tVar t0Var2) fnVecAtEvent
                    maybeFnVecAfterEventUseBox =
                        fmap (restoreUnaffectedComponents) $ -- to remove boxes for unaffected components
                        fmap (wrapFnVecAsBox effEval invariant) $
                        fmap (restoreUnaffectedComponents) $ -- to improve the effect of the invariant if unaffected components are used
                        fmap (map removeAllVarsButT) $
                        fmap (!! m) $
                        solveUncertainValueExactTime
                            sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
                                delta
                                    (odeivp tStart modeAfterEvent $ 
                                        makeFnVecFromInitialValues componentNames 
                                            $ getRangeVec fnVecAtEvent)
                        where
                        Just invariant = Map.lookup modeAfterEvent modeInvariants  
                        restoreUnaffectedComponents fnVecAfterEvent =
                            zipWith pick affectedComponents $ zip fnVecAfterEvent fnVecBeforeEvent
                            where
                            pick True (fnAfterEvent, _fnBeforeEvent) = fnAfterEvent
                            pick False (_fnAfterEvent, fnBeforeEvent) = fnBeforeEvent  
                    getRangeVec fnVec = 
                        map getRange fnVec
                    getRange fn =
                        evalAtPointOutEff effEval (getDomainBox fn) fn
    
                eventModeSwitchesAndResetFunctions = hybsys_eventModeSwitchesAndResetFunctions hybsys
                modeInvariants = hybsys_modeInvariants hybsys
                eventKindAffectCompsAndPruneList =
                    map (\(eventKind,(_,affectedComps,pruneFn)) -> (eventKind,affectedComps,pruneFn)) $ 
                        Map.toList $ possibleOrCertainFirstEventsMap

    odeivp t0End mode makeInitValueFnVec =
        ODEIVP
        {
            odeivp_description = "ODE for " ++ show mode,
            odeivp_field = field,
            odeivp_componentNames = componentNames,
            odeivp_tVar = tVar,
            odeivp_tStart = tStart,
            odeivp_tEnd = tEnd,
            odeivp_makeInitialValueFnVec = makeInitValueFnVec,
            odeivp_t0End = t0End,
            odeivp_maybeExactValuesAtTEnd = Nothing
        }
        where
        Just field = Map.lookup mode modeFields
        modeFields = hybsys_modeFields hybsys
    
wrapFnVecAsBox effEval transformVec fnVec =
    result
    where
    result =
        map (newConstFnFromSample sampleFn) $ transformVec rangeVec

    rangeVec = map (evalAtPointOutEff effEval dombox) fnVec         
    dombox = getDomainBox sampleFn
    (sampleFn : _) = fnVec

makeNonneg ::
    (HasZero d, NumOrd.PartialComparison d, RefOrd.IntervalLike d) 
    => 
    d -> d
makeNonneg r
    | rangeContainsZero =
        RefOrd.fromEndpointsOutWithDefaultEffort (z, rR)
    | otherwise = r 
    where
    rangeContainsZero =
        ((rL <=? z) == Just True)
        &&
        ((z <=? rR) == Just True)
    z = zero r
    (rL, rR) = RefOrd.getEndpointsOutWithDefaultEffort r
    
data EventInfo f
    = EventNextSure (HybSysMode, [f]) (Map.Map HybSysEventKind (EventInfo f)) -- at least one
    | EventNextMaybe (HybSysMode, [f]) (Map.Map HybSysEventKind (EventInfo f)) -- possibly none
    | EventFixedPoint (HybSysMode, [f]) -- been there before
    | EventGivenUp -- eg when tree has become too large or the ODE solver gave up
    | EventTODO (HybSysMode, [f]) -- to capture intermediate states during breadth-first search 
    
instance
    (Show f)
    => 
    Show (EventInfo f)
    where
    show = showEventInfo "" show
    
showEventInfo :: 
    String -> ((HybSysMode, [f]) -> String) -> EventInfo f -> String
showEventInfo prefix showState (EventTODO state) =
    prefix ++ "EventTODO " ++ showState state
showEventInfo prefix _showState EventGivenUp =
    prefix ++ "EventGivenUp"
showEventInfo prefix showState (EventFixedPoint state) =
    prefix ++ "EventFixedPoint " ++ showState state
showEventInfo prefix showState eventInfo =
    prefix ++ eventInfoConstrS ++ " " ++ showState state 
    ++ (showEvents $ Map.toAscList eventsMap)
    where
    (eventInfoConstrS, state, eventsMap) =
        case eventInfo of
            (EventNextMaybe state2 eventsMap2) -> ("EventNextMaybe", state2, eventsMap2)
            (EventNextSure state2 eventsMap2) -> ("EventNextSure", state2, eventsMap2)
            _ -> error ""
    showEvents [] = " []"
    showEvents events = 
        "\n" ++ (unlines $ map showEvent events)
    showEvent (eventKind, eventInfo2) =
        prefix ++ show eventKind ++ " ->\n" 
        ++ showEventInfo (prefix ++ "| ") showState eventInfo2
            
eventInfoIsGivenUp :: EventInfo t -> Bool
eventInfoIsGivenUp EventGivenUp = True
eventInfoIsGivenUp _ = False
    
eventInfoCollectFinalStates ::
    (Show (Domain f), Show f,
     CanEvaluate f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f))
    =>
    EvaluationEffortIndicator f
    ->
    Var f -> Domain f -> EventInfo f 
    -> 
    Maybe [HybridSystemUncertainState f]
eventInfoCollectFinalStates effEval tVar tEnd eventInfo =
    aux eventInfo
    where
    aux (EventFixedPoint state) = Just [stateFromModeFnVec state] 
    aux (EventNextSure _ furtherInfo) =
        fmap concat $ sequence $ map aux $ Map.elems furtherInfo
    aux (EventNextMaybe state furtherInfo) =
        fmap concat $ sequence $
            (Just [stateFromModeFnVec state] :
             (map aux $ Map.elems furtherInfo))
    aux _ = Nothing
    stateFromModeFnVec (mode, fnVec) =
        HybridSystemUncertainState
        {
            hybstate_modes = Set.singleton mode
        ,
            hybstate_values = valueVec
        }
        where
        valueVec =
            fst $ evalAtEndTimeVec effEval tVar tEnd fnVec
        
eventInfoCountEvents ::
    (ArithInOut.RoundedReal d)
    =>
    d ->
    ArithInOut.RoundedRealEffortIndicator d ->
    EventInfo f ->
    d
eventInfoCountEvents sampleD effD eventInfo =
    aux eventInfo
    where
    aux (EventFixedPoint _) = nonneg 
    aux (EventNextSure _ furtherInfo) =
        foldl1 union $ map incr $ map aux $ Map.elems furtherInfo
    aux (EventNextMaybe _ furtherInfo) =
        foldl union c0 $ map incr $ map aux $ Map.elems furtherInfo
    aux _ = nonneg
    
    incr = ArithInOut.addOutEff effAdd c1
    union = RefOrd.meetOutEff effJoinMeet
    c0 = zero sampleD
    c1 = one sampleD
    cInfty = plusInfinity sampleD
    nonneg = c0 `union` cInfty
    
    effJoinMeet =
        ArithInOut.rrEffortJoinMeet sampleD effD
    effAdd = 
        ArithInOut.fldEffortAdd sampleD $ ArithInOut.rrEffortField sampleD effD
        
        
leqOverSomeT ::
    (Show (Domain f),
    Show f,
    RefOrd.IntervalLike f,
    RefOrd.IntervalLike (Domain f),
    NumOrd.PartialComparison (Domain f),
    HasAntiConsistency f,
    HasAntiConsistency (Domain f),
    CanEvaluate f) 
    =>
    EvaluationEffortIndicator f -> 
    Int -> 
    Var f 
    -> 
    f -> f -> Bool
leqOverSomeT effEval n tVar f1 f2 =
    predOverSomeT (\[e1,e2] -> (e1 <=? e2) == Just True) effEval n tVar [f1, f2]
        
predOverSomeT :: 
    (Show (Domain f),
    Show f,
    RefOrd.IntervalLike f,
    RefOrd.IntervalLike (Domain f),
    NumOrd.PartialComparison (Domain f),
    HasAntiConsistency f,
    HasAntiConsistency (Domain f),
    CanEvaluate f) 
    =>
    ([Domain f] -> Bool)
    ->
    EvaluationEffortIndicator f -> 
    Int -> 
    Var f 
    -> 
    [f] -> Bool
predOverSomeT predicate effEval n tVar fs =
    or predResults
    where
    predResults = map predOnSample tSamples
        where
        predOnSample tSample = predicate fsOnT
            where
            fsOnT = map onT fs
            onT f = res
                where
                [res] = fst $ evalAtEndTimeVec effEval tVar tSample [f]
    tSamples = map getTDom tSampleBoxes
    tSampleBoxes = getNSamplesFromDomainBox sampleF domboxTOnly n
    domboxTOnly = fromList [(tVar, tDom)]
    tDom = getTDom dombox
    getTDom box = 
        case lookupVar box tVar of
            Just tDom2 -> tDom2
            _ -> error "aern-ivp: internal error in predOverSomeT"
    dombox = getDomainBox sampleF
    (sampleF : _) = fs
