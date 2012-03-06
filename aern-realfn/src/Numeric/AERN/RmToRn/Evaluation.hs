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
import Numeric.AERN.RmToRn.Laws

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Arbitrary

import Numeric.AERN.Misc.Debug

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

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
    
propEvalRefIsotone ::
    (CanEvaluate f,
     RefOrd.PartialComparison (Domain f),
     RefOrd.ArbitraryOrderedTuple (VarBox f (Domain f)), 
     Area (VarBox f (Domain f)) ~ VarBox f (Domain  f),
     Show f,
     Show (Domain f),
     Show (DomainBox f))
    =>
    f ->
    (SingletonInArea f) -> 
    (Area (DomainBox f),
        (RefOrd.LEPair (DomainBox f)) -> 
        EvaluationEffortIndicator f ->
        (RefOrd.PartialCompareEffortIndicator (Domain f)) ->
        Bool)
propEvalRefIsotone _
        (SingletonInArea fn)
    =
--    unsafePrint
--    (
--       "propEvalRefIsotone: "
--       ++ "\n fn = " ++ show fn
--    ) $
    (sampleDomBox, roundedRefinementIsotoneDom fn "function evaluation" evalIn evalOut) 
    where
    sampleDomBox = getDomainBox fn
    evalIn eff d = evalAtPointInEff eff d fn
    evalOut eff d = evalAtPointOutEff eff d fn

testsEval (name, sample) area =
    testGroup (name ++ " evaluation") $
        [
            testProperty "refinement isotone" (area, propEvalRefIsotone sample)
        ]
    
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

