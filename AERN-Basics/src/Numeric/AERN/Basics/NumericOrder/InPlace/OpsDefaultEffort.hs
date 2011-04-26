{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.InPlace.OpsDefaultEffort
    Description :  convenience directed-rounded in-place lattice operations with default effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience directed-rounded in-place lattice operations with default effort parameters.
-}

module Numeric.AERN.Basics.NumericOrder.InPlace.OpsDefaultEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.NumericOrder


minDnInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
minDnInPlace = mutable2EffToMutable2 minDnInPlaceEff minmaxDefaultEffort

minUpInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
minUpInPlace = mutable2EffToMutable2 minUpInPlaceEff minmaxDefaultEffort

maxDnInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
maxDnInPlace = mutable2EffToMutable2 maxDnInPlaceEff minmaxDefaultEffort

maxUpInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
maxUpInPlace = mutable2EffToMutable2 maxUpInPlaceEff minmaxDefaultEffort

-- | Outward rounded in-place minimum
minOutInPlace :: (OuterRoundedLatticeInPlace t) => OpMutable2 t s
minOutInPlace = mutable2EffToMutable2 minOuterInPlaceEff minmaxOuterDefaultEffort

-- | Outward rounded in-place maximum
maxOutInPlace :: (OuterRoundedLatticeInPlace t) => OpMutable2 t s
maxOutInPlace = mutable2EffToMutable2 maxOuterInPlaceEff minmaxOuterDefaultEffort

-- | Inward rounded in-place minimum
minInInPlace :: (InnerRoundedLatticeInPlace t) => OpMutable2 t s
minInInPlace = mutable2EffToMutable2 minInnerInPlaceEff minmaxInnerDefaultEffort

-- | Outward rounded in-place maximum
maxInInPlace :: (InnerRoundedLatticeInPlace t) => OpMutable2 t s
maxInInPlace = mutable2EffToMutable2 maxInnerInPlaceEff minmaxInnerDefaultEffort


