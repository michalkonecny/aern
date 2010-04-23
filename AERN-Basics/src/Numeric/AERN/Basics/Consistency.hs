{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.Consistency
    Description :  types with consistent and inconsistent values
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Types with consistent and inconsistent values.
-}

module Numeric.AERN.Basics.Consistency where

import Numeric.AERN.Misc.Bool
import Numeric.AERN.Misc.Maybe

import Data.Maybe

class HasConsistency t where
    type ConsistencyEffortIndicator t
    consistencyDefaultEffort :: t -> ConsistencyEffortIndicator t
    isConsistentEff :: (ConsistencyEffortIndicator t) -> t -> Maybe Bool
    
class (HasConsistency t) => HasAntiConsistency t where
    isAntiConsistentEff :: (ConsistencyEffortIndicator t) -> t -> Maybe Bool
    flipConsistency :: t -> t
    
propFlipConsistency :: 
    (HasAntiConsistency t, Eq t) => t -> (ConsistencyEffortIndicator t) -> t -> Bool
propFlipConsistency _ effort e =
    (defined eConsistent && defined eFAntiConsistent)
    ===>
    (fromJust eConsistent <===> fromJust eFAntiConsistent)
    where
    eF = flipConsistency e
    eConsistent = isConsistentEff effort e
    eFAntiConsistent =  isAntiConsistentEff effort eF

propConsistencyFlipSelfInverse :: 
    (HasAntiConsistency t, Eq t) => t -> t -> Bool
propConsistencyFlipSelfInverse _ e =
    e == (flipConsistency $ flipConsistency e)

