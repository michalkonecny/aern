{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

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

testsConsistency :: 
    (Arbitrary t, Show t, Eq t,
     HasAntiConsistency t,
     Arbitrary (ConsistencyEffortIndicator t),
     Show (ConsistencyEffortIndicator t)
     ) => 
    (String, t) -> Test
testsConsistency (name, sample) =
    testGroup (name ++ " consistency flip")
        [
         testProperty "con<->anticon" (propFlipConsistency sample)
        ,
         testProperty "self inverse" (propConsistencyFlipSelfInverse sample)
        ]
