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
    isConsistent :: t -> Maybe Bool
    
class (HasConsistency t) => HasAntiConsistency t where
    isAntiConsistent :: t -> Maybe Bool
    flipConsistency :: t -> t
    
propFlipConsistency :: 
    (HasAntiConsistency t, Eq t) => t -> t -> Bool
propFlipConsistency _ e =
    (defined (isConsistent e) && defined (isAntiConsistent eF))
    ===>
    ((fromJust $ isConsistent e) <===> (fromJust $ isAntiConsistent eF))
    where
    eF = flipConsistency e

propConsistencyFlipSelfInverse :: 
    (HasAntiConsistency t, Eq t) => t -> t -> Bool
propConsistencyFlipSelfInverse _ e =
    e == (flipConsistency $ flipConsistency e)

