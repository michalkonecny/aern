{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort
    Description :  convenience binary infix operators with default effort parameters  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience binary infix operators with default effort parameters.
-}

module Numeric.AERN.Basics.NumericOrder.InPlace.OpsDefaultEffort where

import Numeric.AERN.Basics.NumericOrder
import Numeric.AERN.Basics.Mutable


minDnInPlace :: (CanBeMutable t, RoundedLattice t) => OpMutable2 t s
minDnInPlace = pureEffToMutable2 minDnEff minmaxDefaultEffort

minUpInPlace :: (CanBeMutable t, RoundedLattice t) => OpMutable2 t s
minUpInPlace = pureEffToMutable2 minUpEff minmaxDefaultEffort

maxDnInPlace :: (CanBeMutable t, RoundedLattice t) => OpMutable2 t s
maxDnInPlace = pureEffToMutable2 maxDnEff minmaxDefaultEffort

maxUpInPlace :: (CanBeMutable t, RoundedLattice t) => OpMutable2 t s
maxUpInPlace = pureEffToMutable2 maxUpEff minmaxDefaultEffort

-- | Outward rounded minimum
minOuterInPlace :: (CanBeMutable t, OuterRoundedLattice t) => OpMutable2 t s
minOuterInPlace = pureEffToMutable2 minOuterEff minmaxOuterDefaultEffort

-- | Outward rounded maximum
maxOuterInPlace :: (CanBeMutable t, OuterRoundedLattice t) => OpMutable2 t s
maxOuterInPlace = pureEffToMutable2 maxOuterEff minmaxOuterDefaultEffort

-- | Inward rounded minimum
minInnerInPlace :: (CanBeMutable t, InnerRoundedLattice t) => OpMutable2 t s
minInnerInPlace = pureEffToMutable2 minInnerEff minmaxInnerDefaultEffort

-- | Outward rounded maximum
maxInnerInPlace :: (CanBeMutable t, InnerRoundedLattice t) => OpMutable2 t s
maxInnerInPlace = pureEffToMutable2 maxInnerEff minmaxInnerDefaultEffort


