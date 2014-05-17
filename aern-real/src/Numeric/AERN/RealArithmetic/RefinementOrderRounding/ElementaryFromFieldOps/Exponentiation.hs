{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.ElementaryFromFieldOps.Exponentiation
    Description :  implementation of in/out rounded exponentiation
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Implementation of in/out rounded exponentiation.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.ElementaryFromFieldOps.Exponentiation where

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RefinementOrder as RefOrd

import qualified Numeric.AERN.NumericOrder as NumOrd

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.ExactOps

import Test.QuickCheck (Arbitrary(..)) -- , arbitrary, vectorOf)

import Control.Monad.ST (ST)

import Control.Applicative

import Numeric.AERN.Misc.Debug
_ = unsafePrint

data ExpThinEffortIndicator t =
    ExpThinEffortIndicator
    {
        expeff_arith :: ArithInOut.RoundedRealEffortIndicator t,
        expeff_taylorDeg :: Int -- ^ the highest number of iterations of Newton method to make 
    }

instance 
    (
        Arbitrary (ArithInOut.RoundedRealEffortIndicator e)
    )
    =>
    Arbitrary (ExpThinEffortIndicator e)
    where
    arbitrary =
        ExpThinEffortIndicator <$> arbitrary <*> (fromInt1To10 <$> arbitrary)
        
deriving instance
    (
        Show (ArithInOut.RoundedRealEffortIndicator e)
    )
    =>
    Show (ExpThinEffortIndicator e)

instance
    (
        EffortIndicator (ArithInOut.RoundedRealEffortIndicator e)
    )
    =>
    EffortIndicator (ExpThinEffortIndicator e)
    where
    effortIncrementVariants (ExpThinEffortIndicator e1O e2O) =
        [ExpThinEffortIndicator e1 e2 | 
            (e1, e2) <- effortIncrementVariants (e1O, e2O) ]
    effortRepeatIncrement (ExpThinEffortIndicator i1 i2, ExpThinEffortIndicator j1 j2) = 
        ExpThinEffortIndicator (effortRepeatIncrement (i1, j1)) (effortRepeatIncrement (i2, j2)) 
    effortIncrementSequence (ExpThinEffortIndicator e1O e2O) =
        [ExpThinEffortIndicator e1 e2 | 
            (e1, e2) <- effortIncrementSequence (e1O, e2O) ]

expThinDefaultEffort :: 
   (ArithInOut.RoundedReal t) 
   =>
   t -> Int -> ExpThinEffortIndicator t
expThinDefaultEffort x deg =
    ExpThinEffortIndicator
    {
        expeff_arith = ArithInOut.roundedRealDefaultEffort x,
        expeff_taylorDeg = deg
    }


expOutThinArg ::
    (ArithInOut.RoundedReal t, Show t) 
    =>
    ExpThinEffortIndicator t ->
    t {-^ @x@ assumed to be a thin approximation -} -> 
    t {-^ @exp(x)@ -}
expOutThinArg eff x =
    -- infinities not handled well by the Taylor formula,
    -- treat them as special cases, adding also 0 for efficiency:
    case (xTooBig, xTooLow, x |>=? (zero sample)) of
        (True, _, _) -> 
            x </\> (plusInfinity sample) -- x almost oo
        (_, True, _) ->
            (zero sample) </\> ((one sample) </> (neg x)) -- x almost -oo
        (_, _, Just True) -> one sample -- x = 0
        _ | excludesPlusInfinity x && excludesMinusInfinity x ->
            expOutViaTaylorForXScaledNearZero
        _ -> -- not equal to infinity but not excluding infinity:
--            unsafePrint
--            (
--                "expOutThinArg: overflow:"
--                ++ "\n x = " ++ show x
--                ++ "\n xTooLow = " ++ show xTooLow
--                ++ "\n xTooBig = " ++ show xTooBig
--                ++ "\n excludesMinusInfinity x = " ++ show (excludesMinusInfinity x)
--                ++ "\n excludesPlusInfinity x = " ++ show (excludesPlusInfinity x)
--            ) $
            (zero sample) </\> (plusInfinity sample)
             -- this is always a valid outer approx
    where

    (xUp, xTooBig) =
        case ArithUpDn.convertUpEff effortToInt sampleI x of
            Just xUp -> (xUp, False)
            _ -> (error "internal error in expOutThinArg", True)
    (xDn, xTooLow) =
        case ArithUpDn.convertDnEff effortToInt sampleI x of
            Just xDn -> (xDn, False)
            _ -> (error "internal error in expOutThinArg", True)
    expOutViaTaylorForXScaledNearZero =
        (expOutViaTaylor degr (x </>| n)) <^> n
        where
        n = -- x / n must fall inside [-1,1] 
            (abs xUp) `max` (abs xDn)
    expOutViaTaylor degr x = -- assuming x inside [-1,1]
        (1::Int) |<+> (te degr (1::Int))
        where
        te steps i =
--            unsafePrint
--            (
--                "expOutThinArg: expOutViaTaylor:"
--                ++ "\n teResult = " ++ show teResult
--                ++ "\n i = " ++ show i
--                ++ "\n x = " ++ show x
--            ) 
            teResult
            where
            teResult 
                | steps > 0 =
                    (x </>| i) <*> (1 |<+> (te (steps - 1) (i + 1)))
                | steps == 0 = 
                    errorBound
            errorBound = 
                (x </>| i) <*> ithDerivBound
            ithDerivBound =
                case (pNonnegNonposEff effortCompare x) of
                    (Just True, _) -> -- x >= 0:
                        (one x) </\> eUp
                    (_, Just True) -> -- x <= 0:
                        recipEDn </\> (one x)
                    _ -> -- near or crossing zero:
                        recipEDn </\> eUp
            eUp =
                ArithInOut.convertOutEff effortFromDouble sample (2.718281829 :: Double)
            recipEDn =
                ArithInOut.convertOutEff effortFromDouble sample (0.367879440 :: Double)

    (|>=?) = RefOrd.pGeqEff effortRefinement
    (</\>) = RefOrd.meetOutEff effortMeet
    (</>) = ArithInOut.divOutEff divInOutEffort
    (<*>) = ArithInOut.multOutEff multInOutEffort
    (<^>) = ArithInOut.powerToNonnegIntOutEff intPowerInOutEffort
    (</>|) = ArithInOut.mixedDivOutEff mixedDivInOutEffort
    (|<+>) = flip $ ArithInOut.mixedAddOutEff mixedAddInOutEffort

    addInOutEffort = ArithInOut.fldEffortAdd x effortField
    multInOutEffort = ArithInOut.fldEffortMult x effortField
    intPowerInOutEffort = ArithInOut.fldEffortPow x effortField
    divInOutEffort = ArithInOut.fldEffortDiv x effortField
    mixedAddInOutEffort = ArithInOut.mxfldEffortAdd x xUp effortMixedField
    mixedMultInOutEffort = ArithInOut.mxfldEffortMult x xUp effortMixedField
    mixedDivInOutEffort = ArithInOut.mxfldEffortDiv x xUp effortMixedField
    
    effortField = ArithInOut.rrEffortField sample effR
    effortMixedField = ArithInOut.rrEffortIntMixedField sample effR
    effortMeet = ArithInOut.rrEffortJoinMeet sample effR
    effortRefinement = ArithInOut.rrEffortRefComp sample effR
    effortCompare = ArithInOut.rrEffortNumComp sample effR
    effortToInt = ArithInOut.rrEffortToInt sample effR
    effortFromDouble = ArithInOut.rrEffortFromDouble sample effR
    
    effR = expeff_arith eff
    degr = expeff_taylorDeg eff
    
    sample = x
    sampleI = 0 :: Int

expOutThinArgInPlace ::
    (ArithInOut.RoundedRealInPlace t) =>
    ExpThinEffortIndicator t ->
    Mutable t s {-^ out parameter -} ->
    Mutable t s {-^ @xM@ assumed to be a thin approximation -} -> 
    ST s ()
expOutThinArgInPlace
        eff
        resM xM =
    do
    -- clone xM to ensure no aliasing with resM:
    xMNA <- cloneMutable xM
    
    -- we need x - a pure version of xM for branching conditions:
    x <- unsafeReadMutable xMNA
    -- unsafe is OK because we do not write into xMNA while x is in scope

    -- set various effort indicators for the following block using implicit parameters: 
    let sample = x
    let effortField = ArithInOut.rrEffortField sample effR
    let effortMixedField = ArithInOut.rrEffortIntMixedField sample effR
    let effortMeet = ArithInOut.rrEffortJoinMeet sample effR
    let effortRefinement = ArithInOut.rrEffortRefComp sample effR
    let effortCompare = ArithInOut.rrEffortNumComp sample effR
    let effortToInt = ArithInOut.rrEffortToInt sample effR
    let effortFromDouble = ArithInOut.rrEffortFromDouble sample effR
    let divInOutEffort = ArithInOut.fldEffortDiv x effortField
    let multInOutEffort = ArithInOut.fldEffortMult x effortField
    let intPowerInOutEffort = ArithInOut.fldEffortPow x effortField
    let mixedAddInOutEffort = ArithInOut.mxfldEffortAdd x degr effortMixedField
    let mixedDivInOutEffort = ArithInOut.mxfldEffortDiv x degr effortMixedField

    let (|>=?) = RefOrd.pGeqEff effortRefinement
    let (</\>) = RefOrd.meetOutEff effortMeet
    let (</>) = ArithInOut.divOutEff $ divInOutEffort
    
    -- compute integer bounds on x if possible: 
    let (xUp, xTooBig) =
          case ArithUpDn.convertUpEff effortToInt (0::Int) x of
            Just xUp -> (xUp, False)
            _ -> (error "internal error in expOutThinArg", True)
    let (xDn, xTooLow) =
          case ArithUpDn.convertDnEff effortToInt (0::Int) x of
            Just xDn -> (xDn, False)
            _ -> (error "internal error in expOutThinArg", True)

    -- infinities not handled well by the Taylor formula,
    -- treat them as special cases, adding also 0 for efficiency:
    case (xTooBig, xTooLow, x |>=? (zero x)) of
        (True, _, _) -> unsafeWriteMutable resM (x </\> (plusInfinity x)) -- x almost oo
        (_, True, _) -> unsafeWriteMutable resM ((zero x) </\> ((one x) </> (neg x))) -- x almost -oo
        (_, _, Just True) -> unsafeWriteMutable resM (one x) -- x = 0
        _ | excludesPlusInfinity x && excludesMinusInfinity x ->
            -- the main case where Taylor is used:
            expOutViaTaylorForXScaledNearZero x effortField effortMixedField effortCompare effortFromDouble resM xUp xDn xMNA
        _ -> -- not equal to infinity but not excluding infinity:
            unsafeWriteMutable resM ((zero x) </\> (plusInfinity x))
             -- this is always a valid outer approx
    where
    effR = expeff_arith eff
    degr = expeff_taylorDeg eff
    expOutViaTaylorForXScaledNearZero sample effortField effortMixedField effortCompare effortFromDouble resM xUp xDn xM =
        -- assuming no aliasing between xM and resM
    
        -- set various effort indicators for the following block using implicit parameters: 
        do
        xM </>|= n -- x := x/n
        expOutViaTaylor effortField effortMixedField effortCompare effortFromDouble resM degr xM -- res := exp x
        resM <^>= n -- res := res^n
        where
        n = -- x / n must fall inside [-1,1] 
            (abs xUp) `max` (abs xDn)
        
        (</>|=) = mutableNonmutToNonmut $ ArithInOut.mixedDivOutInPlaceEff effortMixedDiv 
        effortMixedDiv = ArithInOut.mxfldEffortDiv sample n effortMixedField
        
        (<^>=) = mutableNonmutToNonmut $ ArithInOut.powerToNonnegIntOutInPlaceEff effPower
        effPower = ArithInOut.fldEffortPow sample effortField
        
    expOutViaTaylor effortField effortMixedField effortCompare effortFromDouble resM degr xM = -- assuming x inside [-1,1]
        -- assuming no aliasing between xM and resM
    
        do
        -- we need a pure version of xM for constructing the error bound:
        x <- unsafeReadMutable xM
        -- unsafe is OK because we do not write into xM and it does not alias with resM
        
        te resM degr oneI x xM -- res := x + x^2/2 + ...
        addOneToResM x
        where
        addOneToResM x =
            do
            resM <+>|= oneI -- res := res + 1
            where
            (<+>|=) = mutableNonmutToNonmut $ ArithInOut.mixedAddOutInPlaceEff effortMixedAdd 
            effortMixedAdd = ArithInOut.mxfldEffortAdd x oneI effortMixedField
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
                        (one x) </\> eUp
                    (_, Just True) -> -- x <= 0:
                        recipEDn </\> (one x)
                    _ -> -- near or crossing zero:
                        recipEDn </\> eUp
            eUp =
                ArithInOut.convertOutEff effortFromDouble sample (2.718281829 :: Double)
            recipEDn =
                ArithInOut.convertOutEff effortFromDouble sample (0.367879440 :: Double)
                
            (</\>) = RefOrd.meetOutEff $ ArithInOut.rrEffortJoinMeet sample effR
            (<+>|=) = mutableNonmutToNonmut $ ArithInOut.mixedAddOutInPlaceEff effortMixedAdd 
            (</>|=) = mutableNonmutToNonmut $ ArithInOut.mixedDivOutInPlaceEff effortMixedDiv 
            effortMixedAdd = ArithInOut.mxfldEffortAdd sample i effortMixedField
            effortMixedDiv = ArithInOut.mxfldEffortDiv sample i effortMixedField
            (<*>=) = mutable2ToMutable1 $ ArithInOut.multOutInPlaceEff effortMult 
            effortMult = ArithInOut.fldEffortMult sample effortField
                
            sample = x