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
import Numeric.AERN.RmToRn.Evaluation

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
        