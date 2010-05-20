{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.NumericOrderRounding.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations mixing 2 types.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding.MixedFieldOps where

class RoundedMixedAdd s t where
    type MixedAddEffortIndicator s t
    mixedAddUpEff :: MixedAddEffortIndicator s t -> s -> t -> t
    mixedAddDnEff :: MixedAddEffortIndicator s t -> s -> t -> t
    mixedAddDefaultEffort :: s -> t -> MixedAddEffortIndicator s t

class RoundedMixedMultiply s t where
    type MixedMultEffortIndicator s t
    mixedMultUpEff :: MixedMultEffortIndicator s t -> s -> t -> t
    mixedMultDnEff :: MixedMultEffortIndicator s t -> s -> t -> t
    mixedMultDefaultEffort :: s -> t -> MixedMultEffortIndicator s t

class RoundedMixedDivide s t where
    type MixedDivEffortIndicator s t
    mixedDivUpEff :: MixedDivEffortIndicator s t -> s -> t -> t
    mixedDivDnEff :: MixedDivEffortIndicator s t -> s -> t -> t
    mixedDivDefaultEffort :: s -> t -> MixedDivEffortIndicator s t
    