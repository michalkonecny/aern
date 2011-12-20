{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.FromInOutRingOps.Comparison
    Description :  approximation of min and max using only ring operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Approximation of min and max using only ring operations.
    .
    The motivating use case for this module is where we compute min or max for a 
    /function/ pointwise over its domain.
-}

module Numeric.AERN.NumericOrder.FromInOutRingOps.Comparison where

import Numeric.AERN.RmToRn.Domain
--import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsImplicitEffort

--import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

--import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort
----import Numeric.AERN.RefinementOrder.InPlace.OpsImplicitEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort
--
--import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.Mutable
--import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.Basics.PartialOrdering

type PartialCompareEffortIndicatorFromRingOps f =
    (ArithInOut.AddEffortIndicator f,
     NumOrd.PartialCompareEffortIndicator (Domain f),
     EvalOpsEffortIndicator f (Domain f)) 
    

pCompareEffFromRingOps ::
    (Show (Domain f),
     NumOrd.PartialComparison (Domain f),
     HasZero (Domain f),
     HasEvalOps f (Domain f),
     ArithInOut.RoundedSubtr f) 
    =>
    PartialCompareEffortIndicatorFromRingOps f ->
    f ->
    f ->
    Maybe PartialOrdering
pCompareEffFromRingOps (effAdd, effCompDom, effEval) p1 p2 =
    NumOrd.pCompareEff effCompDom diffRange (zero diffRange)
    where
    diffRange = evalOtherType (evalOpsOut effEval diff sampleDom) dom diff
    diff = ArithInOut.subtrOutEff effAdd p1 p2
    dom = getDomainBox diff
    sampleDom = getSampleDomValue p1
