{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Evaluate
    Description :  operations focusing on function evaluation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Operations focusing on function evaluation.
-}

module Numeric.AERN.RmToRn.Evaluation where

import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Arbitrary

import Numeric.AERN.Misc.Debug

class (HasDomainBox f) => CanEvaluateOtherType f
    where
    type EvalOps f :: * -> *
    evalOtherType :: (Show t) => (EvalOps f t) -> (VarBox f t) -> f -> t

class 
    (CanEvaluateOtherType f,
     EffortIndicator (EvalOpsEffortIndicator f t)) 
    => 
    HasEvalOps f t
    where
    type EvalOpsEffortIndicator f t
    evalOpsDefaultEffort :: f -> t -> EvalOpsEffortIndicator f t
    evalOpsOut :: EvalOpsEffortIndicator f t -> f -> t -> EvalOps f t 
    evalOpsIn :: EvalOpsEffortIndicator f t -> f -> t -> EvalOps f t 

{-
    The following are special cases of the above, which
    can sometimes be implemented more efficiently.
-}

class 
    (HasDomainBox f,
     EffortIndicator (EvaluationEffortIndicator f)) 
    => 
    CanEvaluate f
    where
    type EvaluationEffortIndicator f
    evaluationDefaultEffort :: f -> EvaluationEffortIndicator f
    evalAtPointOutEff :: 
        EvaluationEffortIndicator f -> 
        (VarBox f (Domain f)) -> f -> (Domain f)
    evalAtPointInEff ::
        EvaluationEffortIndicator f -> 
        (VarBox f (Domain f)) -> f -> (Domain f)
    
{-
    Properties and tests of CanEvaluate are in the Laws module
    to avoid a circular dependency on that module.
    Laws requires Evaluation and the property requires
    a function defined in Laws.
-}
    
class (HasDomainBox f) => CanSubstitute f
    where
    type SubstitutionEffortIndicator f
    substitutionDefaultEffort :: f -> SubstitutionEffortIndicator f
    substituteOut ::
        SubstitutionEffortIndicator f -> 
        (VarBox f (Domain f)) -> f -> f
    substituteIn ::
        SubstitutionEffortIndicator f -> 
        (VarBox f (Domain f)) -> f -> f

