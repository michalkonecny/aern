{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.Implementation.Elementary
    Description :  implementation of in/out rounded elementary operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Implementation of in/out rounded elementary operations.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.Implementation.Elementary where

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort
import Numeric.AERN.Basics.RefinementOrder.InPlace.OpsImplicitEffort

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.ExactOps

import Control.Monad.ST (ST, runST)

expOutThinArg ::
    (HasZero t, HasOne t, HasInfinities t,
     RefOrd.PartialComparison t,
     NumOrd.PartialComparison t,
     RefOrd.OuterRoundedLattice t,
     ArithUpDn.Convertible t Int,
     ArithInOut.Convertible Double t,
     ArithInOut.RoundedMixedField t Int,
     ArithInOut.RoundedField t) =>
    ArithInOut.FieldOpsEffortIndicator t ->
    ArithInOut.MixedFieldOpsEffortIndicator t Int ->
    RefOrd.JoinMeetOutEffortIndicator t ->
    RefOrd.PartialCompareEffortIndicator t ->
    NumOrd.PartialCompareEffortIndicator t ->
    (ArithUpDn.ConvertEffortIndicator t Int, 
     ArithInOut.ConvertEffortIndicator Double t) ->
    Int {-^ the highest degree to consider in the Taylor expansion -} ->
    t {-^ @x@ assumed to be a thin approximation -} -> 
    t {-^ @exp(x)@ -}
expOutThinArg
        effortField
        effortMixedField
        effortMeet
        effortRefinement effortCompare
        (effortToInt, effortFromDouble)
        degr x =
    let ?pCompareEffort = effortRefinement in
    let ?joinmeetOutEffort = effortMeet in
    let ?divInOutEffort = ArithInOut.fldEffortDiv x effortField in
    -- infinities not handled well by the Taylor formula,
    -- treat them as special cases, adding also 0 for efficiency:
    case (xTooBig, xTooLow, x |>=? zero) of
        (True, _, _) -> x </\> plusInfinity -- x almost oo
        (_, True, _) -> zero </\> (one </> (neg x)) -- x almost -oo
        (_, _, Just True) -> one -- x = 0
        _ | excludesPlusInfinity x && excludesMinusInfinity x ->
            expOutViaTaylorForXScaledNearZero
        _ -> -- not equal to infinity but not excluding infinity:
            zero </\> plusInfinity
             -- this is always a valid outer approx
    where
    (xUp, xTooBig) =
        case ArithUpDn.convertUpEff effortToInt x of
            Just xUp -> (xUp :: Int, False)
            _ -> (error "internal error in expOutThinArg", True)
    (xDn, xTooLow) =
        case ArithUpDn.convertDnEff effortToInt x of
            Just xDn -> (xDn :: Int, False)
            _ -> (error "internal error in expOutThinArg", True)
    expOutViaTaylorForXScaledNearZero =
        let ?joinmeetOutEffort = effortMeet in
        let ?addInOutEffort = ArithInOut.fldEffortAdd x effortField in
        let ?multInOutEffort = ArithInOut.fldEffortMult x effortField in
        let ?intPowerInOutEffort = ArithInOut.fldEffortPow x effortField in
        let ?divInOutEffort = ArithInOut.fldEffortDiv x effortField in
        let ?mixedAddInOutEffort = ArithInOut.mxfldEffortAdd x xUp effortMixedField in
        let ?mixedMultInOutEffort = ArithInOut.mxfldEffortMult x xUp effortMixedField in
        let ?mixedDivInOutEffort = ArithInOut.mxfldEffortDiv x xUp effortMixedField in
        (expOutViaTaylor degr (x </>| n)) <^> n
        where
        n = -- x / n must fall inside [-1,1] 
            (abs xUp) `max` (abs xDn)
    expOutViaTaylor degr x = -- assuming x inside [-1,1]
        oneI |<+> (te degr oneI)
        where
        oneI :: Int
        oneI = 1
        te steps i
            | steps > 0 =
                (x </>| i) <*> (oneI |<+> (te (steps - 1) (i + 1)))
            | steps == 0 = 
                errorBound
                where
                errorBound = 
                    (x </>| i) <*> ithDerivBound
                ithDerivBound =
                    case (pNonnegNonposEff effortCompare x) of
                        (Just True, _) -> -- x >= 0:
                            one </\> eUp
                        (_, Just True) -> -- x <= 0:
                            recipEDn </\> one
                        _ -> -- near or crossing zero:
                            recipEDn </\> eUp
                eUp =
                    ArithInOut.convertOutEff effortFromDouble (2.718281829 :: Double)
                recipEDn =
                    ArithInOut.convertOutEff effortFromDouble (0.367879440 :: Double)

expOutThinArgInPlace ::
    (CanBeMutable t, 
     HasZero t, HasOne t, HasInfinities t,
     RefOrd.PartialComparison t,
     NumOrd.PartialComparison t,
     RefOrd.OuterRoundedLattice t,
     ArithUpDn.Convertible t Int,
     ArithInOut.Convertible Double t,
     ArithInOut.RoundedField t,
     ArithInOut.RoundedFieldInPlace t,
     ArithInOut.RoundedMixedField t Int,
     ArithInOut.RoundedMixedFieldInPlace t Int, -- this constraint should be redundant..
     ArithInOut.RoundedPowerToNonnegIntInPlace t) => 
    ArithInOut.FieldOpsEffortIndicator t ->
    ArithInOut.MixedFieldOpsEffortIndicator t Int ->
    RefOrd.JoinMeetOutEffortIndicator t ->
    RefOrd.PartialCompareEffortIndicator t ->
    NumOrd.PartialCompareEffortIndicator t ->
    (ArithUpDn.ConvertEffortIndicator t Int, 
     ArithInOut.ConvertEffortIndicator Double t) ->
    Mutable t s -> {-^ out parameter -}
    Int {-^ the highest degree to consider in the Taylor expansion -} ->
    Mutable t s {-^ @xM@ assumed to be a thin approximation -} -> 
    ST s ()
expOutThinArgInPlace
        effortField
        effortMixedField
        effortMeet
        effortRefinement effortCompare
        (effortToInt, effortFromDouble)
        resM degr xM =
    do
    -- clone xM to ensure no aliasing with resM:
    xMNA <- cloneMutable xM
    
    -- we need x - a pure version of xM for branching conditions:
    x <- unsafeReadMutable xMNA
    -- unsafe is OK because we do not write into xMNA while x is in scope

    -- set various effort indicators for the following block using implicit parameters: 
    let ?pCompareEffort = effortRefinement
    let ?joinmeetOutEffort = effortMeet
    let ?divInOutEffort = ArithInOut.fldEffortDiv x effortField
    let ?multInOutEffort = ArithInOut.fldEffortMult x effortField
    let ?intPowerInOutEffort = ArithInOut.fldEffortPow x effortField
    let ?mixedAddInOutEffort = ArithInOut.mxfldEffortAdd x degr effortMixedField
    let ?mixedDivInOutEffort = ArithInOut.mxfldEffortDiv x degr effortMixedField

    -- compute integer bounds on x if possible: 
    let (xUp, xTooBig) =
          case ArithUpDn.convertUpEff effortToInt x of
            Just xUp -> (xUp :: Int, False)
            _ -> (error "internal error in expOutThinArg", True)
    let (xDn, xTooLow) =
          case ArithUpDn.convertDnEff effortToInt x of
            Just xDn -> (xDn :: Int, False)
            _ -> (error "internal error in expOutThinArg", True)

    -- infinities not handled well by the Taylor formula,
    -- treat them as special cases, adding also 0 for efficiency:
    case (xTooBig, xTooLow, x |>=? zero) of
        (True, _, _) -> unsafeWriteMutable resM (x </\> plusInfinity) -- x almost oo
        (_, True, _) -> unsafeWriteMutable resM (zero </\> (one </> (neg x))) -- x almost -oo
        (_, _, Just True) -> unsafeWriteMutable resM one -- x = 0
        _ | excludesPlusInfinity x && excludesMinusInfinity x ->
            -- the main case where Taylor is used:
            expOutViaTaylorForXScaledNearZero resM xUp xDn xMNA
        _ -> -- not equal to infinity but not excluding infinity:
            unsafeWriteMutable resM (zero </\> plusInfinity)
             -- this is always a valid outer approx
    where
    expOutViaTaylorForXScaledNearZero resM xUp xDn xM =
        -- assuming no aliasing between xM and resM
    
        -- set various effort indicators for the following block using implicit parameters: 
        do
        xM </>|= n -- x := x/n
        expOutViaTaylor resM degr xM -- res := exp x
        resM <^>= n -- res := res^n
        where
        n = -- x / n must fall inside [-1,1] 
            (abs xUp) `max` (abs xDn)
    expOutViaTaylor resM degr xM = -- assuming x inside [-1,1]
        -- assuming no aliasing between xM and resM
    
        do
        -- we need a pure version of xM for constructing the error bound:
        x <- unsafeReadMutable xM
        -- unsafe is OK because we do not write into xM and it does not alias with resM
        
        let ?addInOutEffort = ArithInOut.fldEffortAdd x effortField
        let ?mixedMultInOutEffort = ArithInOut.mxfldEffortMult x oneI effortMixedField
        te resM degr oneI x xM -- res := x + x^2/2 + ...
        resM <+>|= oneI -- res := res + 1
        where
        oneI :: Int
        oneI = 1
        te resM steps i x xM
            | steps > 0 =
                do
                -- (x </>| i) <*> (oneI |<+> (te (steps - 1) (i + 1)))
                te resM (steps - 1) (i + 1) x xM
                resM <+>|= oneI
                resM </>|= i
                resM <*>= xM               
            | steps == 0 = 
                do
                -- (x </>| i) <*> ithDerivBound
                unsafeWriteMutable resM ithDerivBound
                resM </>|= i
                resM <*>= xM
                where
                ithDerivBound =
                    case (pNonnegNonposEff effortCompare x) of
                        (Just True, _) -> -- x >= 0:
                            one </\> eUp
                        (_, Just True) -> -- x <= 0:
                            recipEDn </\> one
                        _ -> -- near or crossing zero:
                            recipEDn </\> eUp
                eUp =
                    ArithInOut.convertOutEff effortFromDouble (2.718281829 :: Double)
                recipEDn =
                    ArithInOut.convertOutEff effortFromDouble (0.367879440 :: Double)
