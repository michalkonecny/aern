{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE FlexibleInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.MinimalFnBasis.Basics
    Description :  main typeclass and wrapper type 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    MinimalFnBasis typeclass definition, type-level wrapper for its instances
    and some extra arithmetic instances derived from the MinimalFnBasis instance.
-}

module Numeric.AERN.RmToRn.MinimalFnBasis.Basics 
    (
        MinimalFnBasis(..), FnEndpoint(..)
    )
where

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Interval

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.ShowInternals

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck

class (
--       CanBeMutable fb,
       HasLegalValues fb,

       -- function-specific capabilities:
       HasDomainBox fb,
       ArithInOut.RoundedReal (Domain fb),
--        CanEvaluate fb, -- value at a point
--        CanSubstitute fb, -- substitution
       CanEvaluateOtherType fb, -- another interpretation (eg string or interval)
       ShowInternals fb,
       HasProjections fb, -- variables
       HasConstFns fb, -- constants

       -- conversions: 
       ArithUpDn.Convertible fb (Domain fb), -- bounds

       -- exact ops:
       NegInPlace fb,
       HasZero fb,
       HasOne fb,
       NumOrd.HasExtrema fb,

       -- ring ops rounded in/out by some internal notion of enclosure:
       ArithInOut.RoundedAdd fb,
       ArithInOut.RoundedMultiply fb,
       ArithInOut.RoundedMixedAdd fb (Domain fb),
       ArithInOut.RoundedMixedMultiply fb (Domain fb),
       ArithInOut.RoundedMixedAdd fb Int,
       ArithInOut.RoundedMixedMultiply fb Int,
       ArithInOut.RoundedMixedAdd fb Integer,
       ArithInOut.RoundedMixedMultiply fb Integer,
       ArithInOut.RoundedMixedAdd fb Rational,
       ArithInOut.RoundedMixedMultiply fb Rational,
       ArithInOut.RoundedMixedAdd fb Double,
       ArithInOut.RoundedMixedMultiply fb Double,
       
       -- in-place versions:
--       ArithInOut.RoundedAddInPlace fb,
--       ArithInOut.RoundedMultiplyInPlace fb,
--       ArithInOut.RoundedMixedAddInPlace fb (Domain fb),
--       ArithInOut.RoundedMixedMultiplyInPlace fb (Domain fb),
       
       -- primitive function rounded up/down:
       -- TODO
       
       -- random generation:
       Arbitrary (SizeLimits fb),
       Arbitrary (Domain fb),
       Arbitrary (Var fb), Ord (Var fb),
       NumOrd.ArbitraryOrderedTuple (Domain fb)
      ) => 
    MinimalFnBasis fb
    where
    fitDomainBoxToSizeLimits :: fb -> (SizeLimits fb) -> DomainBox fb -> DomainBox fb
--    fixedDomain :: fb -> (Domain fb, Domain fb)
    
{-| 
    A type-level wrapper for function endpoints to make it easier (possible)
    to make them instances of some classes based only on their instance of
    'MinimalFnBasis'.
-}
newtype FnEndpoint fb = FnEndpoint fb
    deriving 
        (Show,
         HasConstFns, 
         HasProjections, 
         NegInPlace,
--         ArithUpDn.RoundedAddInPlace, 
         ArithInOut.RoundedAddInPlace,
         ArithInOut.RoundedAdd,
--         ArithUpDn.RoundedMultiplyInPlace, 
         ArithInOut.RoundedMultiplyInPlace,
         ArithInOut.RoundedMultiply,
         HasZero,
         HasOne,
         NumOrd.HasGreatest,
         NumOrd.HasLeast,
         NumOrd.HasExtrema,
         HasLegalValues,
         MinimalFnBasis)

instance CanBeMutable fb => CanBeMutable (FnEndpoint fb)
    where
    newtype (Mutable (FnEndpoint fb)) s = FnEndpointMutable (Mutable fb s)
    writeMutable (FnEndpointMutable fM) (FnEndpoint f) = writeMutable fM f
    unsafeWriteMutable (FnEndpointMutable fM) (FnEndpoint f) = unsafeWriteMutable fM f
    makeMutable (FnEndpoint f) = 
        do 
        fM <- makeMutable f
        return $ FnEndpointMutable fM
    unsafeMakeMutable (FnEndpoint f) = 
        do 
        fM <- unsafeMakeMutable f
        return $ FnEndpointMutable fM
    readMutable (FnEndpointMutable fM) =
        do
        f <- readMutable fM
        return $ FnEndpoint f
    unsafeReadMutable (FnEndpointMutable fM) =
        do
        f <- unsafeReadMutable fM
        return $ FnEndpoint f
    assignMutable (FnEndpointMutable resM) (FnEndpointMutable srcM) =
        assignMutable resM srcM
    swapMutable (FnEndpointMutable resM) (FnEndpointMutable srcM) =
        swapMutable resM srcM
    cloneMutable (FnEndpointMutable fM) =
        do 
        resM <- cloneMutable fM
        return $ FnEndpointMutable resM
    sameVariable (FnEndpointMutable f1M) (FnEndpointMutable f2M) =
        sameVariable f1M f2M
    variablesIndependent (FnEndpointMutable f1M) (FnEndpointMutable f2M) =
        variablesIndependent f1M f2M
     
instance ShowInternals fb => ShowInternals (FnEndpoint fb)
    where
    type ShowInternalsIndicator (FnEndpoint fb) = ShowInternalsIndicator fb
    defaultShowIndicator (FnEndpoint f) = defaultShowIndicator f
    showInternals i (FnEndpoint f) = showInternals i f
     
instance HasDomainBox fb => HasDomainBox (FnEndpoint fb)
    where
    type Domain (FnEndpoint fb) = Domain fb
    type VarBox (FnEndpoint fb) = VarBox fb
    type Var (FnEndpoint fb) = Var fb
    getSampleDomValue (FnEndpoint f) = getSampleDomValue f
--    getNVariables (FnEndpoint f) = getNVariables f
    getDomainBox (FnEndpoint f) = getDomainBox f
    defaultDomSplit (FnEndpoint f) i = defaultDomSplit f i
    

instance (ArithUpDn.Convertible fb t) =>
    ArithUpDn.Convertible (FnEndpoint fb) t
    where
    type ArithUpDn.ConvertEffortIndicator (FnEndpoint fb) t = 
        ArithUpDn.ConvertEffortIndicator fb t
    convertDefaultEffort (FnEndpoint f) a = ArithUpDn.convertDefaultEffort f a
    convertUpEff eff (FnEndpoint f) = ArithUpDn.convertUpEff eff f
    convertDnEff eff (FnEndpoint f) = ArithUpDn.convertDnEff eff f
    
    
instance HasSizeLimits fb => HasSizeLimits (FnEndpoint fb)
    where
    type SizeLimits (FnEndpoint fb) = SizeLimits fb
    getSizeLimits (FnEndpoint f) = getSizeLimits f
    defaultSizes (FnEndpoint f) = defaultSizes f
    changeSizeLimits l (FnEndpoint f) = FnEndpoint $ changeSizeLimits l f
    
instance CanEvaluateOtherType fb => CanEvaluateOtherType (FnEndpoint fb)
    where
    type EvalOps (FnEndpoint fb) = EvalOps fb    
    evalOtherType ops box (FnEndpoint f) = evalOtherType ops box f

--instance ArithUpDn.RoundedAddEffort fb => ArithUpDn.RoundedAddEffort (FnEndpoint fb)
--    where
--    type ArithUpDn.AddEffortIndicator (FnEndpoint fb) = ArithUpDn.AddEffortIndicator fb 
--    addDefaultEffort (FnEndpoint f) = ArithUpDn.addDefaultEffort f

instance ArithInOut.RoundedAddEffort fb => ArithInOut.RoundedAddEffort (FnEndpoint fb)
    where
    type ArithInOut.AddEffortIndicator (FnEndpoint fb) = ArithInOut.AddEffortIndicator fb 
    addDefaultEffort (FnEndpoint f) = ArithInOut.addDefaultEffort f

--instance ArithUpDn.RoundedMultiplyEffort fb => ArithUpDn.RoundedMultiplyEffort (FnEndpoint fb)
--    where
--    type ArithUpDn.MultEffortIndicator (FnEndpoint fb) = ArithUpDn.MultEffortIndicator fb 
--    multDefaultEffort (FnEndpoint f) = ArithUpDn.multDefaultEffort f

instance ArithInOut.RoundedMultiplyEffort fb => ArithInOut.RoundedMultiplyEffort (FnEndpoint fb)
    where
    type ArithInOut.MultEffortIndicator (FnEndpoint fb) = ArithInOut.MultEffortIndicator fb 
    multDefaultEffort (FnEndpoint f) = ArithInOut.multDefaultEffort f

--instance ArithUpDn.RoundedMixedAddEffort fb t => ArithUpDn.RoundedMixedAddEffort (FnEndpoint fb) t
--    where
--    type ArithUpDn.MixedAddEffortIndicator (FnEndpoint fb) t = ArithUpDn.MixedAddEffortIndicator fb t 
--    mixedAddDefaultEffort (FnEndpoint f) n = ArithUpDn.mixedAddDefaultEffort f n

--instance ArithUpDn.RoundedMixedAddInPlace fb t => ArithUpDn.RoundedMixedAddInPlace (FnEndpoint fb) t
--    where
--    mixedAddUpInPlaceEff e (FnEndpointMutable resM) (FnEndpointMutable fM) n 
--        = ArithUpDn.mixedAddUpInPlaceEff e resM fM n 
--    mixedAddDnInPlaceEff e (FnEndpointMutable resM) (FnEndpointMutable fM) n 
--        = ArithUpDn.mixedAddDnInPlaceEff e resM fM n 

instance ArithInOut.RoundedMixedAddEffort fb t => ArithInOut.RoundedMixedAddEffort (FnEndpoint fb) t
    where
    type ArithInOut.MixedAddEffortIndicator (FnEndpoint fb) t = ArithInOut.MixedAddEffortIndicator fb t 
    mixedAddDefaultEffort (FnEndpoint f) n = ArithInOut.mixedAddDefaultEffort f n

instance ArithInOut.RoundedMixedAddInPlace fb t => ArithInOut.RoundedMixedAddInPlace (FnEndpoint fb) t
    where
    mixedAddInInPlaceEff e (FnEndpointMutable resM) (FnEndpointMutable fM) n 
        = ArithInOut.mixedAddInInPlaceEff e resM fM n 
    mixedAddOutInPlaceEff e (FnEndpointMutable resM) (FnEndpointMutable fM) n 
        = ArithInOut.mixedAddOutInPlaceEff e resM fM n 

instance ArithInOut.RoundedMixedAdd fb t => ArithInOut.RoundedMixedAdd (FnEndpoint fb) t
    where
    mixedAddInEff e (FnEndpoint fM) n 
        = FnEndpoint $ ArithInOut.mixedAddInEff e fM n 
    mixedAddOutEff e (FnEndpoint fM) n 
        = FnEndpoint $ ArithInOut.mixedAddOutEff e fM n 

--instance ArithUpDn.RoundedMixedMultiplyEffort fb t => ArithUpDn.RoundedMixedMultiplyEffort (FnEndpoint fb) t
--    where
--    type ArithUpDn.MixedMultEffortIndicator (FnEndpoint fb) t = ArithUpDn.MixedMultEffortIndicator fb t 
--    mixedMultDefaultEffort (FnEndpoint f) n = ArithUpDn.mixedMultDefaultEffort f n
--
--instance ArithUpDn.RoundedMixedMultiplyInPlace fb t => ArithUpDn.RoundedMixedMultiplyInPlace (FnEndpoint fb) t
--    where
--    mixedMultUpInPlaceEff e (FnEndpointMutable resM) (FnEndpointMutable fM) n 
--        = ArithUpDn.mixedMultUpInPlaceEff e resM fM n 
--    mixedMultDnInPlaceEff e (FnEndpointMutable resM) (FnEndpointMutable fM) n 
--        = ArithUpDn.mixedMultDnInPlaceEff e resM fM n 

instance ArithInOut.RoundedMixedMultiplyEffort fb t => ArithInOut.RoundedMixedMultiplyEffort (FnEndpoint fb) t
    where
    type ArithInOut.MixedMultEffortIndicator (FnEndpoint fb) t = ArithInOut.MixedMultEffortIndicator fb t 
    mixedMultDefaultEffort (FnEndpoint f) n = ArithInOut.mixedMultDefaultEffort f n

instance ArithInOut.RoundedMixedMultiplyInPlace fb t => ArithInOut.RoundedMixedMultiplyInPlace (FnEndpoint fb) t
    where
    mixedMultInInPlaceEff e (FnEndpointMutable resM) (FnEndpointMutable fM) n 
        = ArithInOut.mixedMultInInPlaceEff e resM fM n 
    mixedMultOutInPlaceEff e (FnEndpointMutable resM) (FnEndpointMutable fM) n 
        = ArithInOut.mixedMultOutInPlaceEff e resM fM n 

instance ArithInOut.RoundedMixedMultiply fb t => ArithInOut.RoundedMixedMultiply (FnEndpoint fb) t
    where
    mixedMultInEff e (FnEndpoint fM) n 
        = FnEndpoint $ ArithInOut.mixedMultInEff e fM n 
    mixedMultOutEff e (FnEndpoint fM) n 
        = FnEndpoint $ ArithInOut.mixedMultOutEff e fM n 
