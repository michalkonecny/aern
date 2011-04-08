{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.ElementaryDirect.Sqrt
    Description :  an interval-specific implementation of sqrt
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    An interval-specific implementation of sqrt.
-}

module Numeric.AERN.RealArithmetic.Interval.ElementaryDirect.Sqrt where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Exception

import Control.Exception (throw)

sqrtOutThinArg ::
    (HasZero e, HasOne e, Show e,
     NumOrd.RoundedLattice e,
     NumOrd.PartialComparison e,
     ArithUpDn.Convertible e Double,
     ArithUpDn.RoundedMixedField e Int,
     ArithUpDn.RoundedField e) =>
    ArithUpDn.FieldOpsEffortIndicator e ->
    ArithUpDn.MixedFieldOpsEffortIndicator e Int ->
    NumOrd.MinmaxEffortIndicator e ->
    NumOrd.PartialCompareEffortIndicator e ->
    ArithUpDn.ConvertEffortIndicator e Double ->
    Int {-^ the highest number of iterations of Newton method to make -} ->
    e {-^ @x@ as a singleton interval -} -> 
    (Interval e) {-^ @sqrt(x)@ -}
sqrtOutThinArg
        effortField
        effortMixedField
        effortMinmax
        effortCompare
        effortToDouble
        maxIters
        x
    | sureIsZero x = zero
    | not (sureAbove0 x) = 
        case (sureAbove0 (neg x)) of
            True -> 
                throw $ AERNDomViolationException $ 
                    "sqrtOutThinArg: applied to a negative argument " ++ show x
            _ ->
                throw $ AERNMaybeDomViolationException $ 
                    "sqrtOutThinArg: cannot check that sqrt is applied to a positive argument " ++ show x
    | xRecipSqrtDownInFastRegion =
--            unsafePrint ("AERN: sqrtOutThinArg: lower bound in fast region") $
        Interval 
            (x *. xRecipSqrtDown)
            (x *^ xRecipSqrtUp) -- best upper bound estimate based on an error estimate of the lower bound
    | sureAbove0 xRecipSqrtDown =
--            unsafePrint ("AERN: sqrtOutThinArg: lower bound NOT in fast region, using division") $
        Interval 
            (x *. xRecipSqrtDown)
            (recipUp xRecipSqrtDown) 
         -- an upper bound using division - introduces a fairly large error; used when iteration has not reached the fast region
    | otherwise =
--            unsafePrint ("AERN: sqrtOutThinArg: lower bound too close to zero, using dummy upper bound") $
        Interval
            (x *. xRecipSqrtDown)
            (NumOrd.maxUpEff effortMinmax x one)
         -- a dummy fallback upper bound where lower bound is too close to 0
    where
    (xRecipSqrtDownPrev, xRecipSqrtDown) = recipSqrtDown x
    xRecipSqrtDownInFastRegion =
        case ArithUpDn.convertDnEff effortToDouble t of
            Just lowerBound -> lowerBound > (0.381966012 :: Double) -- (3 - sqrt 5)/2
            Nothing -> False
        where
        t = (xRecipSqrtDownPrev *. xRecipSqrtDownPrev) *. x
    xRecipSqrtUp = 
         -- only valid in "fast" region, ie where the error is smaller than the gap between the
         -- last two iterates
        (xRecipSqrtLastUp +^ xRecipSqrtLastUp) +^ (neg xRecipSqrtDownPrev)
    xRecipSqrtLastUp = 
            (xRecipSqrtDownPrev /^| 2 )
            *^
            (3 |+^ (neg $ x *. (xRecipSqrtDownPrev *. xRecipSqrtDownPrev))) 
            
    sureAbove0 t = 
        case ArithUpDn.convertDnEff effortToDouble t of
            Just lowerBound -> lowerBound > (0 :: Double)
            Nothing -> False
            
    sureIsZero t = 
        case NumOrd.pEqualEff effortCompare t zero of
            Just True -> True
            _ -> False
    
    p1 +^ p2 = ArithUpDn.addUpEff effortAdd p1 p2
    p1 *^ p2 = ArithUpDn.multUpEff effortMult p1 p2
    p1 *. p2 = ArithUpDn.multDnEff effortMult p1 p2
    recipUp p = ArithUpDn.recipUpEff effortDiv p
    recipDn p = ArithUpDn.recipDnEff effortDiv p
    n |+^ p = ArithUpDn.mixedAddUpEff effortAddInt p (n :: Int)
    n |+. p = ArithUpDn.mixedAddDnEff effortAddInt p  (n :: Int)
    p /^| n = ArithUpDn.mixedDivUpEff effortDivInt p  (n :: Int)
    p /.| n = ArithUpDn.mixedDivDnEff effortDivInt p  (n :: Int)
    
    effortAdd = ArithUpDn.fldEffortAdd x effortField
    effortMult = ArithUpDn.fldEffortMult x effortField
    effortDiv = ArithUpDn.fldEffortDiv x effortField
    effortAddInt = ArithUpDn.mxfldEffortAdd x (0::Int) effortMixedField
    effortDivInt = ArithUpDn.mxfldEffortDiv x (0::Int) effortMixedField

    recipSqrtDown p 
        | q0OK = 
            iterRecipSqrt maxIters zero q0
        | otherwise = (zero, zero)
        where
        (q0OK, q0) = 
            (sureAbove0 pp1Up && sureAbove0 babylon2, 
             recipDn babylon2)
            where
            babylon2 = pp5d4Up +^ (neg pp1recipDn)
            pp5d4Up = ((5::Int) |+^ p) /^| (4::Int)
            pp1recipDn = recipDn pp1Up
            pp1Up = (1::Int) |+^ p
        iterRecipSqrt maxIters qNm2 qNm1
            | maxIters > 0 && sureAbove0 qNm1 =
--                    unsafePrint ("AERN: sqrtOutThinArg: recipSqrtDown: iterRecipSqrt: maxIters = " ++ show maxIters) $
                iterRecipSqrt (maxIters - 1) qNm1 qN
            | otherwise = (qNm2, qNm1)
            where
            qN =
                (qNm1 /.| (2::Int))
                *.
                ((3::Int) |+. (neg $ p *^ (qNm1 *^ qNm1))) 
    