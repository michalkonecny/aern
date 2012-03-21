{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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

import Numeric.AERN.Misc.Debug
_ = unsafePrint
        
--solveEvents
--    sizeLimits effCompose effInteg effInclFn effAddFn effAddFnDom effDom
--        delta m
--            t0Var
--                hybivp
--    | givenUp =        
--        (Nothing, modeToEventInfo)
--    | otherwise =
--        (Just finalState, modeToEventInfo)
--    where
--    maxIndividualEvents = 100
--    
--    HybridSystemUncertainState initialModeSet initialValues 
--        = hybivp_initialStateEnclosure hybivp
--    
--    (modeToEventInfo, givenUp, _nodeCount) =
--        undefined
    
data EventInfo f
    = EventNextSure (HybSysMode, [f]) (Map.Map HybSysEventKind (EventInfo f)) -- at least one
    | EventNextMaybe (HybSysMode, [f]) (Map.Map HybSysEventKind (EventInfo f)) -- possibly none
    | EventFixedPoint (HybSysMode, [f]) -- been there before
    | EventUnknown -- only used when given up
    
    