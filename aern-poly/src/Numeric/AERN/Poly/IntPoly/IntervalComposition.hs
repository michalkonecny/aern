{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.IntervalComposition
    Description :  composition of polynomial intervals  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Evaluation of _polynomial intervals_ in general using its instance of 'CanEvaluateOtherType'.
-}

module Numeric.AERN.Poly.IntPoly.IntervalComposition
()
where

--import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.New ()
import Numeric.AERN.Poly.IntPoly.Conversion ()
import Numeric.AERN.Poly.IntPoly.Evaluation
import Numeric.AERN.Poly.IntPoly.Show ()
import Numeric.AERN.Poly.IntPoly.NumericOrder ()
import Numeric.AERN.Poly.IntPoly.RefinementOrder ()
import Numeric.AERN.Poly.IntPoly.Addition ()
import Numeric.AERN.Poly.IntPoly.Multiplication ()
import Numeric.AERN.Poly.IntPoly.Composition ()
import Numeric.AERN.Poly.IntPoly.Minmax ()
import Numeric.AERN.Poly.IntPoly.UpDnField ()

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation


import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort ((<+>|))
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Interval
import Numeric.AERN.RealArithmetic.Interval ()
import Numeric.AERN.RmToRn.Interval ()

import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort

--import qualified Data.IntMap as IntMap
--import qualified Data.Map as Map

import Test.QuickCheck

import Numeric.AERN.Misc.Debug
_ = unsafePrint

instance
    (Ord var, Show var, GeneratableVariables var, 
     ArithInOut.RoundedReal cf, 
     ArithInOut.RoundedMixedField cf cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf, 
     ArithUpDn.Convertible cf cf,
     Show cf, Arbitrary cf, 
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf)
    )
    =>
    HasEvalOps (IntPoly var cf) (Interval (IntPoly var cf))
    where
    type EvalOpsEffortIndicator (IntPoly var cf) (Interval (IntPoly var cf)) = 
        (
            ArithInOut.RingOpsEffortIndicator (Interval (IntPoly var cf)),
            (Int1To1000, (ArithInOut.RoundedRealEffortIndicator cf, Int1To10))
        )
    evalOpsDefaultEffort sampleP samplePI = 
        (
            ArithInOut.ringOpsDefaultEffort samplePI,
            NumOrd.pCompareDefaultEffort sampleP
        )
    evalOpsEff eff sampleP samplePI =
        intpolyPolyEvalOps eff samplePI sampleCf
        where
        sampleCf = getSampleDomValue sampleP
        
        
intpolyPolyEvalOps ::
    (Ord var, Show var, GeneratableVariables var, 
     Show cf, Arbitrary cf, 
     ArithUpDn.Convertible cf cf,
     RefOrd.IntervalLike cf, 
     ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf cf,
     HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf),
     Show (Imprecision cf),
     poly ~ IntPoly var cf)
    =>
   (
    (ArithInOut.RingOpsEffortIndicator (Interval poly)),
    (Int1To1000, (ArithInOut.RoundedRealEffortIndicator cf, Int1To10))
   ) ->
   (Interval poly) ->
   cf ->
   PolyEvalOps var cf (Interval poly)
intpolyPolyEvalOps (effRing, effCmp@(_,(effCf, Int1To10 maxSplitDepth))) samplePI sampleCf =
    result
    where
    result =
        let (<+>) = ArithInOut.addOutEff  (ArithInOut.ringEffortAdd samplePI effRing) in
        let (<*>) = ArithInOut.multOutEff (ArithInOut.ringEffortMult samplePI effRing) in
        let (<^>) = ArithInOut.powerToNonnegIntOutEff (ArithInOut.ringEffortPow samplePI effRing) in
        let (<=?) = NumOrd.pLeqEff effCmp in
        PolyEvalOps (zero samplePI) (<+>) (<*>) (<^>) (newConstFnFromSample samplePI) (const Nothing) maxSplitDepth $
            Just $ PolyEvalMonoOps
                result
                (<=?)
                RefOrd.getEndpointsOutWithDefaultEffort
                RefOrd.fromEndpointsOutWithDefaultEffort
                isDefinitelyExact
                split
                join
                (curry join) -- a very dummy min
                (curry join) -- a very dummy max
                getWidthAsDoubleDummy
                effCf
    split val = (val1, val2)
        where
        val1 = RefOrd.fromEndpointsOutWithDefaultEffort (valL, valM)
        val2 = RefOrd.fromEndpointsOutWithDefaultEffort (valM, valR)
        (valL, valR) = RefOrd.getEndpointsOutWithDefaultEffort val
        valM =
            let (<+>) = ArithInOut.addOutEff (ArithInOut.ringEffortAdd samplePI effRing) in
            let (<*>) = ArithInOut.multOutEff (ArithInOut.ringEffortMult samplePI effRing) in
            (valL <+> valR) <*> (newConstFnFromSample samplePI half)
            where
            half = ArithInOut.mixedAddOutEff effAddCfDbl (zero sampleCf) (0.5 :: Double)
            effAddCfDbl = 
                ArithInOut.mxfldEffortAdd sampleCf (1::Double) $ 
                    ArithInOut.rrEffortDoubleMixedField sampleCf effCf
    getWidthAsDoubleDummy _ = 0 -- no splitting...
--    effAddCf = ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf effCf
    join (Interval l1 _, Interval _ r2) = Interval l1 r2 
    isDefinitelyExact (Interval l r) = 
        (NumOrd.pEqualEff effCmp l r) == Just True
        
