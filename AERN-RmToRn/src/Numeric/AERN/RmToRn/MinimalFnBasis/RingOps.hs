{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.MinimalFnBasis.RingOps
    Description :  ring operations derived from the minimal set
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Ring operations derived from the operations provided by MinimalFnBasis.
-}

module Numeric.AERN.RmToRn.MinimalFnBasis.RingOps
    (
    )
where

import Numeric.AERN.RmToRn.MinimalFnBasis.Basics

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Evaluation

import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.ShowInternals

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

-- in-place ops:

instance (MinimalFnBasis fb) => ArithInOut.RoundedSubtr (FnEndpoint fb)
instance (MinimalFnBasis fb) => ArithInOut.RoundedSubtrInPlace (FnEndpoint fb)
--     default implementation is fine


-- TODO (need min and max first)

--instance (MinimalFnBasis fb) => ArithUpDn.RoundedPowerToNonnegIntEffort (FnEndpoint fb)
--    where
--    type ArithUpDn.PowerToNonnegIntEffortIndicator (FnEndpoint fb) =
--        ArithUpDn.PowerToNonnegIntEffortIndicatorFromMult (FnEndpoint fb)
--    powerToNonnegIntDefaultEffort = ArithUpDn.powerToNonnegIntDefaultEffortFromMult 
--    
--instance (MinimalFnBasis fb) => ArithUpDn.RoundedPowerToNonnegIntInPlace (FnEndpoint fb)
--    where
    

-- pure ops:

instance (MinimalFnBasis fb) => Neg (FnEndpoint fb)
    where
    neg = mutable1ToPure negInPlace
    
--instance (MinimalFnBasis fb) => ArithUpDn.RoundedAdd (FnEndpoint fb)
--    where
--    addUpEff = mutable2EffToPure ArithUpDn.addUpInPlaceEff 
--    addDnEff = mutable2EffToPure ArithUpDn.addDnInPlaceEff 
--
--instance (MinimalFnBasis fb) => ArithUpDn.RoundedSubtr (FnEndpoint fb)
--    where
--    subtrUpEff = mutable2EffToPure ArithUpDn.subtrUpInPlaceEff 
--    subtrDnEff = mutable2EffToPure ArithUpDn.subtrDnInPlaceEff 
--
--instance (MinimalFnBasis fb) => ArithUpDn.RoundedMultiply (FnEndpoint fb)
--    where
--    multUpEff = mutable2EffToPure ArithUpDn.multUpInPlaceEff 
--    multDnEff = mutable2EffToPure ArithUpDn.multDnInPlaceEff 
--    
--instance 
--    (ArithUpDn.RoundedMixedAddInPlace (FnEndpoint fb) t) 
--    => ArithUpDn.RoundedMixedAdd (FnEndpoint fb) t
--    where
--    mixedAddUpEff = mutableNonmutEffToPure ArithUpDn.mixedAddUpInPlaceEff 
--    mixedAddDnEff = mutableNonmutEffToPure ArithUpDn.mixedAddDnInPlaceEff 
--    
--instance 
--    (ArithUpDn.RoundedMixedMultiplyInPlace (FnEndpoint fb) t) 
--    => ArithUpDn.RoundedMixedMultiply (FnEndpoint fb) t
--    where
--    mixedMultUpEff = mutableNonmutEffToPure ArithUpDn.mixedMultUpInPlaceEff 
--    mixedMultDnEff = mutableNonmutEffToPure ArithUpDn.mixedMultDnInPlaceEff 
    
