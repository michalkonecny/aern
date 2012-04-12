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

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Numeric.AERN.Misc.Debug
_ = unsafePrint

solveEventsTimeSplit ::
    (CanAddVariables f,
     CanRenameVariables f,
     CanEvaluate f,
     CanCompose f,
     HasProjections f,
     HasConstFns f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     NumOrd.RefinementRoundedLattice f,
     RefOrd.PartialComparison f,
     RoundedIntegration f,
     ArithInOut.RoundedAdd f,
     ArithInOut.RoundedSubtr f,
     ArithInOut.RoundedMixedAdd f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f),
     Domain f ~ Imprecision (Domain f),
     solvingInfo ~ (Domain f, Maybe (HybridSystemUncertainState f), [(HybSysMode, EventInfo f)]),
     Show f, Show (Domain f), Show (Var f), Eq (Var f))
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
    Var f {-^ @t0@ - the initial time variable -} ->
    Domain f {-^ step size @s@ -} -> 
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
        sizeLimits effCompose effEval effInteg effInclFn effAddFn effAddFnDom effDom
            delta m t0Var minStepSize splitImprovementThreshold
                hybivpG
    = solve hybivpG
    where
    solve hybivp =
        solveHybridIVPBySplittingT
            directSolver
                effDom splitImprovementThreshold minStepSize
                    hybivp

    directSolver hybivp =
        (maybeFinalState, (tEnd, maybeFinalState, modeEventInfoList))
        where
        tEnd = hybivp_tEnd hybivp
        (maybeFinalState, modeEventInfoList) = 
            solveEvents
                sizeLimits effCompose effEval effInteg effInclFn effAddFn effAddFnDom effDom
                    delta m
                        t0Var
                            hybivp

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
     Show f, Show (Domain f), Show (Var f), Eq (Var f)
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
    (Maybe (HybridSystemUncertainState f), [(HybSysMode, (EventInfo f))])
solveEvents
    sizeLimits effCompose effEval effInteg effInclFn effAddFn effAddFnDom effDom
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
                (odeivp tStart initialMode $ makeFnVecFromInitialValues componentNames initialValues)
        esolve prevEventInfo =
            case addOneLayer prevEventInfo of
                (newEventInfo, _, False) -> newEventInfo -- nothing left to do
                (newEventInfo, Nothing, _) -> newEventInfo -- ie given up or reached limit
                (newEventInfo, _, _) -> esolve newEventInfo
        addOneLayer prevEventInfo =
--            unsafePrint
--            (
--                "solveEvents: solveEventsOneMode: " ++
--                "\n prevEventInfo = \n" ++ showEventInfo "   " (show . id) prevEventInfo
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
            processState nodeCountSoFar previousStates state@(mode, fnVec) 
                -- first check whether this state is included in a previous state:
                | stateOccurredEarlier =
                    (EventFixedPoint state, Just (nodeCountSoFar + 1), True)
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
                            fnVecAfterEvent =
                                map removeT0Var $ 
                                    map fst $ fnVecOutInAfterEvent !! m
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
                                            odeivp tEnd modeAfterEvent $
                                                makeInitialValuesFromFnVecAtEvent
                    makeInitialValuesFromFnVecAtEvent _sizeLimits t0Var2 _t0Domain =
                        map (renameVar tVar t0Var2) fnVecAtEvent
                    removeT0Var fn =
                        composeVarOutEff effCompose t0Var t0DomFn fn
                        where
                        t0DomFn =
                            newConstFnFromSample sampleFnWithoutT0 t0Dom
                        sampleFnWithoutT0 : _ = fnVecAtEvent
                        Just (_,t0Dom) =
                            List.find ((== t0Var) . fst) varDoms
                        varDoms = toAscList $ getDomainBox fn
                eventModeSwitchesAndResetFunctions = hybsys_eventModeSwitchesAndResetFunctions hybsys
                eventKindList =
                    map fst $ Set.elems $ possibleOrCertainFirstEventsSet
                    
            

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
        
leqOverSomeT :: 
    (Show (Domain t),
    Show t,
    RefOrd.IntervalLike t,
    RefOrd.IntervalLike (Domain t),
    NumOrd.PartialComparison (Domain t),
    HasAntiConsistency t,
    HasAntiConsistency (Domain t),
    CanEvaluate t) 
    =>
    EvaluationEffortIndicator t -> Int -> Var t -> t -> t -> Bool
leqOverSomeT effEval n tVar f1 f2 =
    or leqResults
    where
    _ = [f1,f2]
    leqResults = map leqOnSample tSamples
        where
        leqOnSample tSample =
            (f1OnT <=? f2OnT) == Just True
            where
            [f1OnT] = fst $ evalAtEndTimeVec effEval tVar tSample [f1]
            [f2OnT] = fst $ evalAtEndTimeVec effEval tVar tSample [f2]
    tSamples = map getTDom tSampleBoxes
    tSampleBoxes = getNSamplesFromDomainBox sampleF domboxTOnly n
    domboxTOnly = fromList [(tVar, tDom)]
    tDom = getTDom dombox
    getTDom box = 
        case lookupVar box tVar of
            Just tDom2 -> tDom2
    dombox = getDomainBox sampleF
    sampleF = f1