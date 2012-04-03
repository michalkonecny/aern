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
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust)

import Numeric.AERN.Misc.Debug
_ = unsafePrint
        

solveEvents ::
    (CanAddVariables f,
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
    sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
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
        maxIndividualEvents = 100
        
        maybeFinalState =
            case eventInfoCollectFinalStates tVar tEnd eventInfo of
                Just states -> Just $ foldl1 (mergeHybridStates effJoinDom) states
                _ -> Nothing
            
        (eventInfo, _nodeCount) =
            esolve initialMode maybeFnVecNoEvent
        maybeFnVecNoEvent =
            fmap (!! m) $
            solveUncertainValueExactTime
                sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
                delta
                (odeivp initialMode $ makeFnVecFromInitialValues componentNames initialValues)
        esolve mode fnVec =
            undefined -- TODO            
            

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
    | EventUnknown -- only used when given up
    
eventInfoCollectFinalStates ::
    (Show (Domain f), Show f,
     CanEvaluate f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f))
    =>
    Var f -> Domain f -> EventInfo f -> Maybe [HybridSystemUncertainState f]
eventInfoCollectFinalStates tVar tEnd eventInfo =
    aux eventInfo
    where
    aux EventUnknown = Nothing
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
            fst $ evalAtEndTimeVec tVar tEnd fnVec
        
