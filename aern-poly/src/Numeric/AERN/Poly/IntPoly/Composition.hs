{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Composition
    Description :  composition of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Evaluation of interval polynomials in general using its instance of 'CanEvaluateOtherType' 
    and specilised cases for evaluation at a point and on an interval.
-}

module Numeric.AERN.Poly.IntPoly.Composition
where

import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.New ()
import Numeric.AERN.Poly.IntPoly.Differentiation
import Numeric.AERN.Poly.IntPoly.Evaluation
import Numeric.AERN.Poly.IntPoly.NumericOrder
import Numeric.AERN.Poly.IntPoly.RefinementOrder
import Numeric.AERN.Poly.IntPoly.Addition
import Numeric.AERN.Poly.IntPoly.Multiplication

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort

--import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf, 
     Show cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf))
    =>
    HasEvalOps (IntPoly var cf) (IntPoly var cf)
    where
    type EvalOpsEffortIndicator (IntPoly var cf) (IntPoly var cf) = 
        (Int1To1000, ArithInOut.RoundedRealEffortIndicator cf)
    evalOpsDefaultEffort _ sampleP@(IntPoly cfg _) = 
        NumOrd.pCompareDefaultEffort sampleP
    evalOpsOut eff _ sampleP =
        polyPolyEvalOpsOut eff sampleP sampleCf
        where
        sampleCf = getSampleDomValue sampleP
    evalOpsIn =
        error "aern-poly: inner-roudned composition not supported for IntPoly"

polyPolyEvalOpsOut ::
    (Ord var, Show var, Show cf,
     RefOrd.IntervalLike cf, 
     ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf),
     Show (Imprecision cf))
    =>
   (Int1To1000, ArithInOut.RoundedRealEffortIndicator cf) ->
   (IntPoly var cf) ->
   cf ->
   PolyEvalOps var cf (IntPoly var cf)
polyPolyEvalOpsOut effCmp@(_,effCf) sampleP sampleCf =
    let (<+>) = ArithInOut.addOutEff effCf in
    let (<*>) = ArithInOut.multOutEff effCf in
    let (<^>) = ArithInOut.powerToNonnegIntOutEff effCf in
    let (<=?) = NumOrd.pLeqEff effCmp in
    PolyEvalOps (zero sampleP) (<+>) (<*>) (<^>) (newConstFnFromSample sampleP) (const Nothing) (<=?) $
        Just $ PolyEvalMonoOps
            RefOrd.getEndpointsOutWithDefaultEffort
            RefOrd.fromEndpointsOutWithDefaultEffort
            isDefinitelyExact
            effCf
    where
    isDefinitelyExact p = 
        polyIsExactEff effImpr p == Just True
    effImpr = ArithInOut.rrEffortImprecision sampleCf effCf
    effAddCf = ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
        
        