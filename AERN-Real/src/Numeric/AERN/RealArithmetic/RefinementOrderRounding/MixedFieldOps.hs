{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RefinementOrderRounding.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations mixing 2 types.
    
    This module is hidden and reexported via its parent RefinementOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps where

class RoundedMixedAdd s t where
    type MixedAddEffortIndicator s t
    mixedAddInEff :: MixedAddEffortIndicator s t -> s -> t -> t
    mixedAddOutEff :: MixedAddEffortIndicator s t -> s -> t -> t
    mixedAddDefaultEffort :: s -> t -> MixedAddEffortIndicator s t

class RoundedMixedMultiply s t where
    type MixedMultEffortIndicator s t
    mixedMultInEff :: MixedMultEffortIndicator s t -> s -> t -> t
    mixedMultOutEff :: MixedMultEffortIndicator s t -> s -> t -> t
    mixedMultDefaultEffort :: s -> t -> MixedMultEffortIndicator s t

class RoundedMixedDivide s t where
    type MixedDivEffortIndicator s t
    mixedDivInEff :: MixedDivEffortIndicator s t -> s -> t -> t
    mixedDivOutEff :: MixedDivEffortIndicator s t -> s -> t -> t
    mixedDivDefaultEffort :: s -> t -> MixedDivEffortIndicator s t
    