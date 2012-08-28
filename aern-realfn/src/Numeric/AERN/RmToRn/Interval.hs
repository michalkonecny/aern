{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-  
    Module      :  Numeric.AERN.RmToRn.Interval
    Description :  interval of functions as a function approximation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval of functions as a function approximation.
    Assuming that both endpoint functions have the same domain and size limits.
-}
module Numeric.AERN.RmToRn.Interval where

import Numeric.AERN.Basics.Interval

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation
import Numeric.AERN.RmToRn.Integration

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

instance
    (HasDomainBox f)
    =>
    (HasDomainBox (Interval f))
    where
    type (Var (Interval f)) = Var f
    type (Domain (Interval f)) = Domain f
    type (VarBox (Interval f)) = VarBox f
    getSampleDomValue (Interval l _) = getSampleDomValue l
    defaultDomSplit (Interval l _) = defaultDomSplit l
    getDomainBox (Interval l _) = getDomainBox l
    getNSamplesFromDomainBox (Interval l _) = getNSamplesFromDomainBox l
    getSampleFromInsideDomainBox (Interval l _) = getSampleFromInsideDomainBox l
    
    
instance 
    (HasSizeLimits f)
    =>
    (HasSizeLimits (Interval f))
    where
    type SizeLimits (Interval f) = SizeLimits f 
    getSizeLimits (Interval l _) = getSizeLimits l
    defaultSizeLimits (Interval l _) = defaultSizeLimits l
    adjustSizeLimitsToVarsAndDombox (Interval l _) vars dombox sizeLims =
        adjustSizeLimitsToVarsAndDombox l vars dombox sizeLims

instance
    (CanChangeSizeLimits f,
     RefOrd.IntervalLike f) 
    =>
    (CanChangeSizeLimits (Interval f)) 
    where
    type SizeLimitsChangeEffort (Interval f) = 
        (SizeLimitsChangeEffort f,
         RefOrd.GetEndpointsEffortIndicator f)
    sizeLimitsChangeDefaultEffort (Interval l _) = 
        (sizeLimitsChangeDefaultEffort l,
         RefOrd.getEndpointsDefaultEffort l)
    changeSizeLimitsOut (eff, effGetE) sizeLims (Interval l r) = (Interval lN rN)
        where
        (lN, _) = 
            RefOrd.getEndpointsOutEff effGetE $
                changeSizeLimitsOut eff sizeLims l
        (_, rN) = 
            RefOrd.getEndpointsOutEff effGetE $
                changeSizeLimitsOut eff sizeLims r
    changeSizeLimitsIn (eff, effGetE) sizeLims (Interval l r) = (Interval lN rN)
        where
        (lN, _) = 
            RefOrd.getEndpointsInEff effGetE $
                changeSizeLimitsIn eff sizeLims l
        (_, rN) = 
            RefOrd.getEndpointsInEff effGetE $
                changeSizeLimitsIn eff sizeLims r

instance 
    (HasProjections f,
     RefOrd.IntervalLike f)
    => 
    HasProjections (Interval f)
    where
    newProjection sizeLims dombox vars = Interval l r
        where
        (l,r) = RefOrd.getEndpointsOutWithDefaultEffort p
        p = newProjection sizeLims dombox vars

instance
    (HasConstFns f,
     RefOrd.IntervalLike f) 
    => 
    HasConstFns (Interval f) 
    where
    newConstFn sizeLims dombox value = Interval l r
        where
        (l,r) = RefOrd.getEndpointsOutWithDefaultEffort p
        p = newConstFn sizeLims dombox value

instance 
    CanAddVariables f 
    => 
    CanAddVariables (Interval f) 
    where
    addVariablesFront varDoms (Interval l r) = Interval lN rN
        where
        lN = addVariablesFront varDoms l
        rN = addVariablesFront varDoms r
    addVariablesBack varDoms (Interval l r) = Interval lN rN
        where
        lN = addVariablesBack varDoms l
        rN = addVariablesBack varDoms r

instance 
    CanRenameVariables f
    => 
    CanRenameVariables (Interval f) 
    where
    renameVar old new (Interval l r) = Interval lN rN
        where
        lN = renameVar old new l
        rN = renameVar old new r
    renameVars renVar (Interval l r) = Interval lN rN
        where
        lN = renameVars renVar l
        rN = renameVars renVar r
    

instance 
    (CanEvaluate f, 
     RefOrd.IntervalLike (Domain f)) 
    =>
    (CanEvaluate (Interval f))
    where
    type EvaluationEffortIndicator (Interval f) =
        (EvaluationEffortIndicator f, RefOrd.FromEndpointsEffortIndicator (Domain f))
    evaluationDefaultEffort (Interval l _) = 
        (evaluationDefaultEffort l, RefOrd.fromEndpointsDefaultEffort sampleDom)
        where
        sampleDom = getSampleDomValue l 
    evalAtPointOutEff (effEval, effFromE) dombox (Interval l r) =
        RefOrd.fromEndpointsOutEff effFromE (lVal, rVal)
        where
        lVal = evalAtPointOutEff effEval dombox l
        rVal = evalAtPointOutEff effEval dombox r
    evalAtPointInEff (effEval, effFromE) dombox (Interval l r) =
        RefOrd.fromEndpointsInEff effFromE (lVal, rVal)
        where
        lVal = evalAtPointInEff effEval dombox l
        rVal = evalAtPointInEff effEval dombox r

instance 
    (RoundedIntegration f,
     RefOrd.IntervalLike f) 
    => 
    RoundedIntegration (Interval f) 
    where
    type IntegrationEffortIndicator (Interval f) =
        (IntegrationEffortIndicator f,
         RefOrd.GetEndpointsEffortIndicator f)
    integrationDefaultEffort (Interval l _) =
        (integrationDefaultEffort l,
         RefOrd.getEndpointsDefaultEffort l)
    primitiveFunctionOutEff (effInteg, effGetE) (Interval l r) var = Interval lN rN
        where
        (lN, _) = RefOrd.getEndpointsOutEff effGetE $ primitiveFunctionOutEff effInteg l var 
        (_, rN) = RefOrd.getEndpointsOutEff effGetE $ primitiveFunctionOutEff effInteg r var 
    primitiveFunctionInEff (effInteg, effGetE) (Interval l r) var = Interval lN rN
        where
        (lN, _) = RefOrd.getEndpointsInEff effGetE $ primitiveFunctionInEff effInteg l var 
        (_, rN) = RefOrd.getEndpointsInEff effGetE $ primitiveFunctionInEff effInteg r var 
