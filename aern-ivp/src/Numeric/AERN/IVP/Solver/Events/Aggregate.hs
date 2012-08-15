{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Solver.Events.Aggregate
    Description :  hybrid system simulation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Hybrid system simulation.
-}

module Numeric.AERN.IVP.Solver.Events.Aggregate
--(
--)
where

import Numeric.AERN.IVP.Solver.Events.Locate

import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Specification.Hybrid
import Numeric.AERN.IVP.Solver.Picard.UncertainValue
--import Numeric.AERN.IVP.Solver.Picard.UncertainTime

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import Data.Maybe (catMaybes)
import qualified Data.Map as Map
--import qualified Data.Set as Set
--import qualified Data.List as List

import Numeric.AERN.Misc.Debug
_ = unsafePrint

solveHybridIVP_UsingPicardAndEventTree ::
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
    Int  {-^ @m@ - limit on the number of Picard iterations after stabilisation -}
    ->
    Var f {-^ @t0@ - the initial time variable -} 
    ->
    HybridIVP f 
    ->
    (Maybe (HybridSystemUncertainState (Domain f)), [(HybSysMode, (EventInfo f))])
solveHybridIVP_UsingPicardAndEventTree
        sizeLimits effPEval effCompose effEval effInteg effInclFn effAddFn _effMultFn effAddFnDom effDom
            maxNodes
                delta m
                    _t0Var
                        (hybivp :: HybridIVP f)
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
    
--    finalState :: HybridSystemUncertainState (Domain f)
    (givenUp, finalState) = 
        case sequence perModeMaybeFinalState of
            Just perModeFinalState | not (null perModeFinalState) ->
                (False, foldl1 (mergeHybridStates effJoinDom) perModeFinalState) 
            _ -> (True, error "internal error in solveEvents: finalState undefined")
    modeEventInfoList = 
        zip modeList perModeEventInfo 
    (perModeMaybeFinalState, perModeEventInfo) = 
        unzip $ map solveEventsOneMode modeValueList 
    modeList = map fst modeValueList
    modeValueList = Map.toList initState 
    initState = hybivp_initialStateEnclosure hybivp
    
    solveEventsOneMode (initialMode, initialValues) =
        (maybeFinalState, eventInfo)
        where
        maybeFinalState =
            case eventInfoCollectFinalStates effEval tVar tEnd eventInfo of
                Just states | not (null states) -> Just $ foldl1 (mergeHybridStates effJoinDom) states
                _ -> Nothing
            
        eventInfo =
            case maybeFnVecNoEvent of
                Nothing -> EventGivenUp
                Just fnVecNoEvent ->
                    esolve (EventTODO (initialMode, fnVecNoEvent))
        maybeFnVecNoEvent =
            fmap (map removeAllVarsButT) $ -- parameters have to be removed so that we can test inclusion
            fmap (!! m) $
            solveODEIVPUncertainValueExactTime_PicardIterations
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
--                -- the following case unnecessarily gives up in some cases:
--                | stateExpandsPreviousOne =
--                    unsafePrint
--                    (
--                        "solveHybridIVP_UsingPicardAndEventTree: processState: stateExpandsPreviousOne"
--                        ++ "\n  state = " ++ show state
--                        ++ "\n  previousStates =\n" ++ unlines (map show previousStates)
--                    ) $
--                    (EventGivenUp, Nothing, True)
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
                    detectEventsWithoutLocalisation effEval eventSpecMap (tStart, tEnd) fnVecBeforeEvent
                eventSpecMap = 
                    hybsys_eventSpecification hybsys mode

                constructorForNextEvents 
                    | someEventCertain = EventNextSure
                    | otherwise = EventNextMaybe
                someEventCertain =
                    or $ map (\(eventCertain, _, _) -> eventCertain) $ Map.elems possibleOrCertainFirstEventsMap
                
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
                        eventSwitchingFn $ 
                        wrapFnVecAsBox effEval pruneUsingTheGuard $
                        fnVecBeforeEvent
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
                        solveODEIVPUncertainValueExactTime_PicardIterations
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
    
detectEventsWithoutLocalisation :: 
    (RefOrd.IntervalLike (Domain f), CanEvaluate f,
     NumOrd.PartialComparison (Domain f),
     HasZero (Domain f)) 
    =>
    EvaluationEffortIndicator f 
    ->
    Map.Map HybSysEventKind ([Bool], [f] -> f, [Domain f] -> Maybe Bool, resetMap) 
    -> 
    (Domain f, Domain f) 
    -> 
    [f] 
    -> 
    Map.Map HybSysEventKind (Bool, [Bool], resetMap)
detectEventsWithoutLocalisation effEval eventSpecMap (tStart,tEnd) fnVecBeforeEvent =
    Map.fromAscList $
        catMaybes $ 
            map detectEvent $
                Map.toAscList eventSpecMap
    where
    detectEvent (eventType, (affectedComps, makeZeroCrossingFn, otherCond, pruneFn)) =
        case examineDipOnDom 
                otherConditionOnDom 
                dipFnPositiveOnDom
                dipFnNegativeOnDom
                dipFnEnclosesZeroOnDom
                (tStart,tEnd) of
            LDResNone -> Nothing
            LDResSome LDResDipCertain _ _ -> Just (eventType, (True, affectedComps, pruneFn))
            _ -> Just (eventType, (False, affectedComps, pruneFn))
        where
        otherConditionOnDom d =
            otherCond fnVecBeforeEventOnD
            where
            fnVecBeforeEventOnD = map (evalAtPointOutEff effEval boxD) fnVecBeforeEvent
            boxD = fromList [(tVar, d)]
        dipFnPositiveOnDom d =
            dipFnOnD >? (zero d)
            where
            dipFn = makeZeroCrossingFn fnVecBeforeEvent
            dipFnOnD = evalAtPointOutEff effEval boxD dipFn
            boxD = fromList [(tVar, d)]
        dipFnNegativeOnDom d =
            dipFnOnD <? (zero d)
            where
            dipFn = makeZeroCrossingFn fnVecBeforeEvent
            dipFnOnD = evalAtPointOutEff effEval boxD dipFn
            boxD = fromList [(tVar, d)]
        dipFnEnclosesZeroOnDom _ = Nothing
        [(tVar,_)] = toAscList $ getDomainBox sampleFn
        (sampleFn : _) = fnVecBeforeEvent
            
        
wrapFnVecAsBox ::
    (CanEvaluate f, HasConstFns f) 
    =>
    EvaluationEffortIndicator f -> 
    ([Domain f] -> [Domain f]) -> 
    [f] -> 
    [f]
wrapFnVecAsBox effEval transformVec fnVec =
    result
    where
    result =
        map (newConstFnFromSample sampleFn) $ transformVec rangeVec

    rangeVec = map (evalAtPointOutEff effEval dombox) fnVec         
    dombox = getDomainBox sampleFn
    (sampleFn : _) = fnVec

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
    Maybe [HybridSystemUncertainState (Domain f)]
eventInfoCollectFinalStates effEval tVar tEnd eventInfo =
    aux eventInfo
    where
    aux (EventFixedPoint _state) = Just [] -- the enclosure can be discounted as it is included in an earlier one 
    aux (EventNextSure _state furtherInfo) =
        fmap concat $ sequence $ map aux $ Map.elems furtherInfo
    aux (EventNextMaybe state furtherInfo) =
        fmap concat $ sequence $
            (Just [stateFromModeFnVec state] :
             (map aux $ Map.elems furtherInfo))
    aux _ = Nothing
    stateFromModeFnVec (mode, fnVec) =
        Map.singleton mode valueVec
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
    aux (EventNextSure _ furtherInfo) 
        | Map.null furtherInfo = error "eventInfoCountEvents: EventNextSure furtherInfo is empty"
        | otherwise = foldl1 union $ map incr $ map aux $ Map.elems furtherInfo
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
        
--leqOverSomeT ::
--    (Show (Domain f),
--    Show f,
--    RefOrd.IntervalLike f,
--    RefOrd.IntervalLike (Domain f),
--    NumOrd.PartialComparison (Domain f),
--    HasAntiConsistency f,
--    HasAntiConsistency (Domain f),
--    CanEvaluate f) 
--    =>
--    EvaluationEffortIndicator f -> 
--    Int -> 
--    Var f 
--    -> 
--    f -> f -> Bool
--leqOverSomeT effEval n tVar f1 f2 =
--    predOverSomeT (\[e1,e2] -> (e1 <=? e2) == Just True) effEval n tVar [f1, f2]
--        
--predOverSomeT :: 
--    (Show (Domain f),
--    Show f,
--    RefOrd.IntervalLike f,
--    RefOrd.IntervalLike (Domain f),
--    NumOrd.PartialComparison (Domain f),
--    HasAntiConsistency f,
--    HasAntiConsistency (Domain f),
--    CanEvaluate f) 
--    =>
--    ([Domain f] -> Bool)
--    ->
--    EvaluationEffortIndicator f -> 
--    Int -> 
--    Var f 
--    -> 
--    [f] -> Bool
--predOverSomeT predicate effEval n tVar fs =
--    or predResults
--    where
--    predResults = map predOnSample tSamples
--        where
--        predOnSample tSample = predicate fsOnT
--            where
--            fsOnT = map onT fs
--            onT f = res
--                where
--                [res] = fst $ evalAtEndTimeVec effEval tVar tSample [f]
--    tSamples = map getTDom tSampleBoxes
--    tSampleBoxes = getNSamplesFromDomainBox sampleF domboxTOnly n
--    domboxTOnly = fromList [(tVar, tDom)]
--    tDom = getTDom dombox
--    getTDom box = 
--        case lookupVar box tVar of
--            Just tDom2 -> tDom2
--            _ -> error "aern-ivp: internal error in predOverSomeT"
--    dombox = getDomainBox sampleF
--    (sampleF : _) = fs
