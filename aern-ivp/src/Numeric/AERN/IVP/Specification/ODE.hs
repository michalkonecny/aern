{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Specification.ODE
    Description :  specification of ODE IVP with uncertainty
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Specification of ODE IVP with uncertainty.
-}

module Numeric.AERN.IVP.Specification.ODE
--(
--)
where

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
--import Numeric.AERN.RmToRn.Integration
--
--import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
--import Numeric.AERN.RealArithmetic.ExactOps
--
--import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort
--
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import Numeric.AERN.Misc.Debug

data ODEIVP f =
    ODEIVP
    {
        odeivp_description :: String
    ,
        odeivp_field :: [f] -> [f]
    ,
        odeivp_componentNames :: [Var f]
    , 
        odeivp_tVar :: Var f -- ^ @tVar@
    ,
        odeivp_tStart :: Domain f -- ^ @tStart@
    ,
        odeivp_tEnd :: Domain f -- ^ @tEnd@
    ,
        odeivp_makeInitialValueFnVec :: ODEInitialValues f 
        {-^ the parameters are:
            * size limits for the function
            * initial time variable @t0@ 
            * time domain @T0@ 
            the result are functions whose first variable is @t0 : T0@ 
            - the initial value may depend on initial time
        -} 
    ,
        odeivp_t0End :: Domain f 
        {-^ 
            @t0End@ -- initial time uncertainty; 
            must satisfy @tStart <= t0End <= tEnd@;
            with exact initial time, we have @t0End = tStart@
        -}
    ,
        odeivp_maybeExactValuesAtTEnd :: Maybe [Domain f]
    }

type ODEInitialValues f =
    SizeLimits f -> Var f -> Domain f -> [f]

makeFnVecFromInitialValues ::
     (Show f, Show (Domain f), Show (Var f),
      HasSizeLimits f,
      HasProjections f) 
      =>
     [Var f] {-^ names for solution vector components -} -> 
     [Domain f] {-^ initial values, one for each component -}
     ->
     SizeLimits f {-^ a sample function, only its size limits matter -} ->
     Var f {-^ @tVar@ - time variable -} -> 
     Domain f {-^ time domain  -} 
     -> 
     [f]
makeFnVecFromInitialValues componentNames initialValues sizeLimits tVar timeDomain =
--    unsafePrint
--    (
--        "makeFnVecFromInitialValues:"
--        ++ "\n componentNames = " ++ show componentNames
--        ++ "\n initialValues = " ++ show initialValues
----        ++ "\n sizeLimits = " ++ show sizeLimits
--        ++ "\n tVar = " ++ show tVar
--        ++ "\n timeDomain = " ++ show timeDomain
--        ++ "\n result = " ++ show initialValuesFnVec
--    ) $
    initialValuesFnVec
    where    
    initialValuesFnVec =
        map initialValueFn componentNames
    initialValueFn var =
        newProjection sizeLimitsAdjusted dombox var
    sizeLimitsAdjusted =
        adjustSizeLimitsToVarsAndDombox sampleF vars dombox sizeLimits
    sampleF = initialValueFn tVar 
    vars = tVar : componentNames
    dombox =
        fromList $ (tVar, timeDomain) : (zip componentNames initialValues)

evalAtEndTimeOutInVec ::
    (Show (Domain f), Show f,
     CanEvaluate f,
     RefOrd.IntervalLike f,
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f))
    =>
    (Var f) -> 
    (Domain f) -> 
    [(f,f)] 
    -> 
    ([Domain f], [Domain f])
evalAtEndTimeOutInVec tVar tEnd fnVec =
    unzip $ map (evalAtEndTimeOutInFn tVar tEnd) fnVec
    
evalAtEndTimeOutInFn ::
    (Show (Domain f), Show f,
     CanEvaluate f,
     RefOrd.IntervalLike f,
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f))
    =>
    (Var f) -> 
    (Domain f) -> 
    (f, f)
    -> 
    (Domain f, Domain f)
evalAtEndTimeOutInFn tVar tEnd (fnOut, fnIn) =
--    unsafePrint
--    (
--        "evalAtEndTimeFn:"
--        ++ "\n fnOut = " ++ show fnOut
--        ++ "\n fnIn = " ++ show fnIn
--        ++ "\n valueOut = " ++ show valueOut
--        ++ "\n valueIn = " ++ show valueIn
--        ++ "\n valueL = " ++ show valueL
--        ++ "\n valueR = " ++ show valueR
--    ) $
    (valueOut, valueIn)
    where
    valueOut =
        evalAtPointOutEff (evaluationDefaultEffort fnOut) endTimeArea fnOut
    valueIn =
        RefOrd.fromEndpointsOutWithDefaultEffort (valueRL, valueLR)
    (_, valueLR) =
        RefOrd.getEndpointsOutWithDefaultEffort valueL
    (valueRL, _) =
        RefOrd.getEndpointsOutWithDefaultEffort valueR
    valueL =
        evalAtPointInEff (evaluationDefaultEffort fnIn) endTimeArea fnL
    valueR =
        evalAtPointInEff (evaluationDefaultEffort fnIn) endTimeArea fnR
    (fnR, fnL) = RefOrd.getEndpointsOutWithDefaultEffort fnIn
--    endTimeArea :: DomainBox f
    endTimeArea = insertVar tVar tEnd $ getDomainBox fnOut
    
        
evalAtEndTimeVec ::
    (Show (Domain f), Show f,
     CanEvaluate f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f))
    =>
    (Var f) -> 
    (Domain f) -> 
    [f] 
    -> 
    ([Domain f], [Domain f])
evalAtEndTimeVec tVar tEnd fnVec =
    unzip $ map (evalAtEndTimeFn tVar tEnd) fnVec
    
evalAtEndTimeFn ::
    (Show (Domain f), Show f,
     CanEvaluate f,
     RefOrd.IntervalLike f,
     HasAntiConsistency f,
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f))
    =>
    (Var f) -> 
    (Domain f) -> 
    f
    -> 
    (Domain f, Domain f)
evalAtEndTimeFn tVar tEnd fn =
    evalAtEndTimeOutInFn tVar tEnd (fn, flipConsistency fn)

