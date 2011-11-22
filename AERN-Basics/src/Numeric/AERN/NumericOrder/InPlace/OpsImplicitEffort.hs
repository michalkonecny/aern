{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.InPlace.OpsImplicitEffort
    Description :  convenience directed-rounded in-place lattice operations with implicit effort parameters  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience directed-rounded in-place lattice operations with implicit effort parameters.
-}

module Numeric.AERN.NumericOrder.InPlace.OpsImplicitEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.NumericOrder

-- | Downward rounded in-place minimum
minDnInPlace ::
    (RoundedLatticeInPlace t, 
     ?minmaxEffort :: MinmaxEffortIndicator t) =>
    OpMutable2 t s
minDnInPlace = minDnInPlaceEff ?minmaxEffort

-- | Upward rounded in-place minimum
minUpInPlace ::
    (RoundedLatticeInPlace t, 
     ?minmaxEffort :: MinmaxEffortIndicator t) =>
    OpMutable2 t s
minUpInPlace = minUpInPlaceEff ?minmaxEffort

-- | Downward rounded in-place maximum
maxDnInPlace ::
    (RoundedLatticeInPlace t, 
     ?minmaxEffort :: MinmaxEffortIndicator t) =>
    OpMutable2 t s
maxDnInPlace = maxDnInPlaceEff ?minmaxEffort

-- | Upward rounded in-place maximum
maxUpInPlace ::
    (RoundedLatticeInPlace t, 
     ?minmaxEffort :: MinmaxEffortIndicator t) =>
    OpMutable2 t s
maxUpInPlace = maxUpInPlaceEff ?minmaxEffort

-- | Outward rounded in-place minimum
minOutInPlace :: 
    (RefinementRoundedLatticeInPlace t, 
     ?minmaxInOutEffort :: MinmaxInOutEffortIndicator t) =>
    OpMutable2 t s
minOutInPlace = minOutInPlaceEff ?minmaxInOutEffort

-- | Outward rounded in-place maximum
maxOutInPlace :: 
    (RefinementRoundedLatticeInPlace t, 
     ?minmaxInOutEffort :: MinmaxInOutEffortIndicator t) =>
    OpMutable2 t s
maxOutInPlace = maxOutInPlaceEff ?minmaxInOutEffort

-- | Inward rounded in-place minimum
minInInPlace ::
    (RefinementRoundedLatticeInPlace t,
     ?minmaxInOutEffort :: MinmaxInOutEffortIndicator t) =>
    OpMutable2 t s
minInInPlace = minInInPlaceEff ?minmaxInOutEffort

-- | Inward rounded in-place maximum
maxInInPlace ::
    (RefinementRoundedLatticeInPlace t,
     ?minmaxInOutEffort :: MinmaxInOutEffortIndicator t) =>
    OpMutable2 t s
maxInInPlace = maxInInPlaceEff ?minmaxInOutEffort
