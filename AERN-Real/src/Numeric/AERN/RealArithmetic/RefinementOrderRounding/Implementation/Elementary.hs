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
    x <- unsafeReadMutable xM 
    -- TODO as long as x is used below to construct values no writing to xM is allowed 
    let ?pCompareEffort = effortRefinement
    let ?joinmeetOutEffort = effortMeet
    let ?divInOutEffort = ArithInOut.fldEffortDiv x effortField
    let ?intPowerInOutEffort = ArithInOut.fldEffortPow x effortField
    let ?mixedAddInOutEffort = ArithInOut.mxfldEffortAdd x degr effortMixedField
    let ?mixedDivInOutEffort = ArithInOut.mxfldEffortDiv x degr effortMixedField
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
            expOutViaTaylorForXScaledNearZero resM x xUp xDn xM
        _ -> -- not equal to infinity but not excluding infinity:
            unsafeWriteMutable resM (zero </\> plusInfinity)
             -- this is always a valid outer approx
    where
    expOutViaTaylorForXScaledNearZero resM x xUp xDn xM =
        let ?joinmeetOutEffort = effortMeet in
        let ?addInOutEffort = ArithInOut.fldEffortAdd x effortField in
        let ?multInOutEffort = ArithInOut.fldEffortMult x effortField in
        let ?intPowerInOutEffort = ArithInOut.fldEffortPow x effortField in
        let ?divInOutEffort = ArithInOut.fldEffortDiv x effortField in
        let ?mixedAddInOutEffort = ArithInOut.mxfldEffortAdd x xUp effortMixedField in
        let ?mixedMultInOutEffort = ArithInOut.mxfldEffortMult x xUp effortMixedField in
        let ?mixedDivInOutEffort = ArithInOut.mxfldEffortDiv x xUp effortMixedField in
        do
        tempM <- makeMutable zero -- TODO is it better to initialise with x </>| n ?
        -- TODO is unsafeMakeMutable possible? How does a volitile constant work??
        mixedDivOutInPlace tempM xM n
        expOutViaTaylor tempM degr x tempM
        powerToNonnegIntOutInPlace resM tempM n
        -- writeMutable resM ((expOutViaTaylor degr (x </>| n)) <^> n)
        where
        n = -- x / n must fall inside [-1,1] 
            (abs xUp) `max` (abs xDn)
    expOutViaTaylor resM degr x xM = -- assuming x inside [-1,1]
        do
        tempM <- makeMutable zero 
        -- TODO better to initialise with pure (te degr oneI)?
        --      make unsafe?
        te tempM degr oneI x xM
        mixedAddOutInPlace resM tempM oneI
        -- unsafeWriteMutable resM (oneI |<+> (te degr oneI))
        where
        oneI :: Int
        oneI = 1
        te resM steps i x xM
            | steps > 0 =
                do
                tempM <- makeMutable zero -- TODO make unsafe?
                te tempM (steps - 1) (i + 1) x xM -- te (steps - 1) (i + 1)
                tempM <+>|= oneI
                tempM </>|= i
                multOutInPlace resM xM tempM               
                -- (x </>| i) <*> (oneI |<+> (te (steps - 1) (i + 1)))
            | steps == 0 = 
                do
                tempM <- makeMutable ithDerivBound -- TODO make unsafe or initialise with zero?
                tempM <*>= xM 
                mixedDivOutInPlace resM tempM i 
                -- TODO can the mutable errorBound above be made more efficient?
                -- unsafeWriteMutable resM errorBound
                where
                errorBound = -- TODO why does commenting out errorBound prevent type-checking?
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
