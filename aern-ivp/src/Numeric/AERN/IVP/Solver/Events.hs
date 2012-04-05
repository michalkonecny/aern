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

import Numeric.AERN.IVP.Solver.Picard.UncertainValue
import Numeric.AERN.IVP.Solver.Picard.UncertainTime
import Numeric.AERN.IVP.Specification.ODE
import Numeric.AERN.IVP.Specification.Hybrid

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
--import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import qualified Data.Map as Map
import qualified Data.Set as Set

import Numeric.AERN.Misc.Debug
_ = unsafePrint
        

solveEvents ::
    (CanAddVariables f,
     CanRenameVariables f,
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
     Show f, Show (Domain f), Show (Var f) 
     )
    =>
    SizeLimits f 
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
    ArithInOut.MixedAddEffortIndicator f (Domain f) 
    ->
    ArithInOut.RoundedRealEffortIndicator (Domain f) 
    ->
    Domain f {-^ initial widening @delta@ -}
    ->
    Int  
    ->
    Var f {-^ @t0@ - the initial time variable -} 
    ->
    HybridIVP f 
    ->
    (Maybe (HybridSystemUncertainState f), Map.Map HybSysMode (EventInfo f))
solveEvents
    sizeLimits effCompose effEval effInteg effInclFn effAddFn effAddFnDom effDom
        delta m
            t0Var
                hybivp
    | givenUp =        
        (Nothing, modeToEventInfo)
    | otherwise =
        (Just finalState, modeToEventInfo)
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
    modeToEventInfo = 
        Map.fromList $ zip modeList perModeEventInfo 
    (perModeMaybeFinalState, perModeEventInfo) = 
        unzip $ map solveEventsOneMode modeList 
    modeList = 
        Set.toList initialModeSet
    HybridSystemUncertainState initialModeSet initialValues 
        = hybivp_initialStateEnclosure hybivp
    
    solveEventsOneMode initialMode =
        (maybeFinalState, eventInfo)
        where
        maxNodes = 100
        
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
            fmap (!! m) $
            solveUncertainValueExactTime
                sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
                delta
                (odeivp initialMode $ makeFnVecFromInitialValues componentNames initialValues)
        esolve prevEventInfo =
            case addOneLayer prevEventInfo of
                (newEventInfo, Nothing) -> newEventInfo -- ie given up or reached limit
                (newEventInfo, _) -> esolve newEventInfo
        addOneLayer prevEventInfo =
            processNode 0 [] prevEventInfo
            where
            processNode nodeCountSoFar previousStates eventInfo2 = 
                case eventInfo2 of
                    EventGivenUp -> (eventInfo2, Nothing)
                    EventNextSure state children ->
                        (EventNextSure state newChildren, maybeNodeCount)
                        where
                        (newChildren, maybeNodeCount) = 
                            processChildren (nodeCountSoFar + 1) (state : previousStates) $ 
                                Map.toAscList children
                    EventNextMaybe state children -> 
                        (EventNextMaybe state newChildren, maybeNodeCount)
                        where
                        (newChildren, maybeNodeCount) = 
                            processChildren (nodeCountSoFar + 1) (state : previousStates) $ 
                                Map.toAscList children
                    EventTODO state -> processState (nodeCountSoFar + 1) previousStates state
            processChildren nodeCountSoFar _previousStates [] = (Map.empty, Just nodeCountSoFar)
            processChildren nodeCountSoFar previousStates ((key, child) : rest) =
                 case processChildren nodeCountSoFar previousStates rest of
                    (newRest, Nothing) -> 
                        (Map.insert key child newRest, Nothing)
                    (newRest, Just nodeCountSoFarWithRest) ->
                        (Map.insert key newChild newRest, newNodeCountSoFar)
                        where
                        (newChild, newNodeCountSoFar) = 
                            processNode nodeCountSoFarWithRest previousStates child
            processState nodeCountSoFar previousStates state@(mode, fnVec) 
                -- first check whether this state is included in a previous state:
                | stateOccurredEarlier =
                    (EventFixedPoint state, Just (nodeCountSoFar + 1))
                | otherwise =
                -- find which events are not ruled out by fnVec and try to determine whether an event is certain
                -- for each potential event, compute an enclosure for the state at the event
                --    then enclosure for the state after that event
                -- build the event info
                -- at the same time sum up the overall events 
                --  (how? need breadth-first with size cut off - need to have partial event info to hold intermediate results;
                --   could use a zipper...)
                (constructorForNextEvents state eventTasksMap, maybeNodeCountNew)
                where
                stateOccurredEarlier =
                    or $ map (stateIncludedIn state) previousStates
                stateIncludedIn (mode1, fnVec1) (mode2, fnVec2) 
                    | mode1 /= mode2 = False
                    | otherwise =
                        let ?pCompareEffort = effInclFn in
                        and $ map (== Just True) $ zipWith (|<=?) fnVec1 fnVec2 
                maybeNodeCountNew
                    | givenUp2 = Nothing
                    | nodeCountSoFar + eventCount > maxNodes = Nothing
                    | otherwise = Just $ nodeCountSoFar + eventCount
                eventCount = 
                    Set.size possibleOrCertainFirstEventsSet
                possibleOrCertainFirstEventsSet = 
                    hybsys_eventDetector hybsys mode fnVec

                constructorForNextEvents 
                    | someEventCertain = EventNextSure
                    | otherwise = EventNextMaybe
                someEventCertain =
                    or $ map snd $ Set.elems possibleOrCertainFirstEventsSet
                
                eventTasksMap =
                    Map.fromAscList eventTasks
                givenUp2 = 
                    or $ map eventInfoIsGivenUp (map snd eventTasks)
                eventTasks =
                    map simulateEvent $ eventKindList
                simulateEvent eventKind =
                    case maybeFnVecAfterEvent of
                        Just fnVecOutInAfterEvent ->
                            (eventKind, EventTODO (modeAfterEvent, fnVecAfterEvent))
                            where
                            fnVecAfterEvent = map fst $ fnVecOutInAfterEvent !! m
                        Nothing ->
                            (eventKind, EventGivenUp)
                    where
                    (modeAfterEvent, eventSwitchingFn) =
                        case Map.lookup eventKind eventModeSwitchesAndResetFunctions of
                            Just res -> res
                            Nothing -> error $ "aern-ivp: hybrid system has no information about event kind " ++ show eventKind
                    fnVecAtEvent = eventSwitchingFn fnVec
                    maybeFnVecAfterEvent = 
                        solveUncertainValueUncertainTime
                                sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
                                    delta
                                        t0Var $
                                            odeivp modeAfterEvent $
                                                makeInitialValuesFromFnVecAtEvent
                    makeInitialValuesFromFnVecAtEvent _sizeLimits t0Var2 _t0Domain =
                        map (renameVar tVar t0Var2) fnVecAtEvent
                eventModeSwitchesAndResetFunctions = hybsys_eventModeSwitchesAndResetFunctions hybsys
                eventKindList =
                    map fst $ Set.elems $ possibleOrCertainFirstEventsSet
                    
            

    odeivp mode makeInitValueFnVec =
        ODEIVP
        {
            odeivp_description = "ODE for " ++ show mode,
            odeivp_field = field,
            odeivp_componentNames = componentNames,
            odeivp_tVar = tVar,
            odeivp_tStart = tStart,
            odeivp_tEnd = tEnd,
            odeivp_makeInitialValueFnVec = makeInitValueFnVec,
            odeivp_t0End = tStart,
            odeivp_maybeExactValuesAtTEnd = Nothing
        }
        where
        Just field = Map.lookup mode modeFields
        modeFields = hybsys_modeFields hybsys
    
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
showEventInfo prefix showState (EventNextMaybe state eventsMap) =
    prefix ++ "EventNextMaybe " ++ showState state 
    ++ (showEvents $ Map.toAscList eventsMap)
    where
    showEvents [] = " []"
    showEvents events = 
        "\n" ++ (unlines $ map showEvent events)
    showEvent (eventKind, eventInfo) =
        prefix ++ show eventKind ++ " ->\n" 
        ++ showEventInfo (prefix ++ "| ") showState eventInfo
            
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
    aux (EventFixedPoint _) = Just [] -- no need to add the state as it refines one that occurred earlier 
    aux (EventNextSure _ furtherInfo) =
        fmap concat $ sequence $ map aux $ Map.elems furtherInfo
    aux (EventNextMaybe (mode, fnVec) furtherInfo) =
        fmap concat $ sequence $
            (Just [stateWhenThisEventLast] :
             (map aux $ Map.elems furtherInfo))
        where
        stateWhenThisEventLast =
            HybridSystemUncertainState
            {
                hybstate_modes = Set.singleton mode
            ,
                hybstate_values = valueVec
            }
        valueVec =
            fst $ evalAtEndTimeVec effEval tVar tEnd fnVec
    aux _ = Nothing
        
