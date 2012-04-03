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
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps
--
--import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort
--
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import Numeric.AERN.Misc.Debug
_ = unsafePrint

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
     SizeLimits f {-^ size limits to use in new function -} ->
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
    initialValueFn = 
        newProjection sizeLimitsAdjusted dombox
    sizeLimitsAdjusted =
        adjustSizeLimitsToVarsAndDombox sampleF vars dombox sizeLimits
    sampleF = initialValueFn tVar 
    vars = tVar : componentNames
    dombox =
        fromList $ (tVar, timeDomain) : zip componentNames initialValues

makeFnVecFromParamInitialValuesOut ::
     (Show f, Show (Domain f), Show (Var f),
      ArithInOut.RoundedReal (Domain f),
      ArithInOut.RoundedAdd f,
      ArithInOut.RoundedSubtr f,
      ArithInOut.RoundedMultiply f,
      RefOrd.IntervalLike f,
      RefOrd.IntervalLike (Domain f),
      HasConstFns f,  HasProjections f,
      CanAddVariables f, CanChangeSizeLimits f) 
      =>
     (ArithInOut.AddEffortIndicator f) ->
     (ArithInOut.MultEffortIndicator f) ->
     (SizeLimitsChangeEffort f)
     ->
     [Var f] {-^ names for solution vector components -} -> 
     [f] {-^ parametrised initial values, one for each component -}
     ->
     SizeLimits f {-^ size limits to use in new function - ignored here -} ->
     Var f {-^ @tVar@ - time variable -} -> 
     Domain f {-^ time domain  -} 
     -> 
     [f]
makeFnVecFromParamInitialValuesOut effAddFn effMultFn effSizeLims componentNames initialValueFnVec sizeLimits tVar timeDomain =
--    unsafePrint
--    (
--        "makeFnVecFromParamInitialValuesOut:"
--        ++ "\n initialValueFnVec = " ++ show initialValueFnVec
--        ++ "\n result = " ++ show result
--    )
    result
    where
    result =
        map updateFn $ zip componentNames initialValueFnVec
    updateFn (compName, initValFn) =
        let ?addInOutEffort = effAddFn in
        let ?multInOutEffort = effMultFn in
--        (initValFnL <*> compVar) <+> (initValFnR <*> (c1 <-> compVar))
        initValFnWithNewVars
        where
--        compVar = makeVar compName
        (initValFnL, initValFnR) = RefOrd.getEndpointsOutWithDefaultEffort initValFnWithNewVars
        initValFnWithNewVars =
            changeSizeLimitsOut effSizeLims sizeLimits $
                addVariablesFront newVarDoms initValFn

--    c1 = 
--        newConstFn sizeLimitsOutgoing domboxOutgoing (one sampleDom) 
--    makeVar =
--        newProjection sizeLimitsOutgoing domboxOutgoing

    sizeLimitsOutgoing =
        getSizeLimits $ changeSizeLimitsOut effSizeLims sizeLimits sampleFnOutgoing

    domboxOutgoing =
        getDomainBox sampleFnOutgoing
    sampleFnOutgoing =
        addVariablesFront newVarDoms sampleFnIncoming 
    newVarDoms =
        (tVar, timeDomain) : (zip componentNames $ repeat unitDom)
        where
        unitDom = RefOrd.fromEndpointsOutWithDefaultEffort (zero sampleDom, one sampleDom)
    
    sampleFnIncoming : _ = initialValueFnVec
    sampleDom = getSampleDomValue sampleFnIncoming


evalAtEndTimeOutInVec ::
    (Show (Domain f), Show f,
     CanEvaluate f,
     RefOrd.IntervalLike f,
     RefOrd.IntervalLike (Domain f),
     HasAntiConsistency (Domain f))
    =>
    Var f -> 
    Domain f -> 
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
    Var f -> 
    Domain f -> 
    (f, f)
    -> 
    (Domain f, Domain f)
evalAtEndTimeOutInFn tVar tEnd (fnOut, fnIn) =
--    unsafePrint
--    (
--        "evalAtEndTimeOutInFn:"
--        ++ "\n fnOut = " ++ show fnOut
--        ++ "\n fnIn = " ++ show fnIn
--        ++ "\n at tEnd = " ++ show tEnd
--        ++ "\n valueOut = " ++ show valueOut
--        ++ "\n valueIn = " ++ show valueIn
--        ++ "\n valueLR = " ++ show valueLR
--        ++ "\n valueRL = " ++ show valueRL
--    ) $
    (valueOut, valueIn)
    where
    valueOut =
        evalAtPointOutEff (evaluationDefaultEffort fnOut) endTimeArea fnOut
    valueIn =
        RefOrd.fromEndpointsOutWithDefaultEffort (valueRL, valueLR)
    (_, valueLR) =
        RefOrd.getEndpointsOutWithDefaultEffort valueBelowR
    (valueRL, _) =
        RefOrd.getEndpointsOutWithDefaultEffort valueAboveL
    valueBelowR =
        evalAtPointInEff (evaluationDefaultEffort fnIn) endTimeArea fnBelowR
    valueAboveL =
        evalAtPointInEff (evaluationDefaultEffort fnIn) endTimeArea fnAboveL
    (fnAboveL, fnBelowR) = RefOrd.getEndpointsOutWithDefaultEffort fnIn
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
    Var f -> 
    Domain f -> 
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
    Var f -> 
    Domain f -> 
    f
    -> 
    (Domain f, Domain f)
evalAtEndTimeFn tVar tEnd fn =
    evalAtEndTimeOutInFn tVar tEnd (fn, flipConsistency fn)

partiallyEvalAtEndTimeOutInVec ::
    (Show (Domain f), Show f,
     CanPartiallyEvaluate f)
    =>
    Var f -> 
    Domain f -> 
    [(f,f)]
    -> 
    ([f],[f])
partiallyEvalAtEndTimeOutInVec tVar tEnd fnVec =
    unzip $ map (partiallyEvalAtEndTimeOutInFn tVar tEnd) fnVec

partiallyEvalAtEndTimeOutInFn ::
    (Show (Domain f), Show f,
     CanPartiallyEvaluate f)
    =>
    Var f -> 
    Domain f -> 
    (f, f)
    -> 
    (f, f)
partiallyEvalAtEndTimeOutInFn tVar tEnd (fnOut, fnIn) =
--    unsafePrint
--    (
--        "partiallyEvalAtEndTimeOutInFn:"
--        ++ "\n fnOut = " ++ show fnOut
--        ++ "\n fnIn = " ++ show fnIn
--        ++ "\n at tEnd = " ++ show tEnd
--        ++ "\n paramValueOut = " ++ show paramValueOut
--        ++ "\n paramValueIn = " ++ show paramValueIn
--    ) $
    (paramValueOut, paramValueIn)
    where
    paramValueOut =
        pEvalAtPointOutEff (partialEvaluationDefaultEffort fnOut) endTimeArea fnOut
    paramValueIn =
        pEvalAtPointInEff (partialEvaluationDefaultEffort fnIn) endTimeArea fnIn
    endTimeArea = fromList [(tVar, tEnd)]
