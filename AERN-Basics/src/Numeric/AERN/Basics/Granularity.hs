{-|
    Module      :  Numeric.AERN.Basics.Granularity
    Description :  finite subsets of continuum sets (eg floats)
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Granularity where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Order
import Numeric.AERN.Basics.Enclosure
import Numeric.AERN.Basics.ApproxOrder
import Numeric.AERN.Basics.ApproxEnclosure

import qualified Prelude
import Prelude hiding (compare, (<=), (>=))
import Control.Monad.ST (ST)
import Test.QuickCheck

{-|
  The bit size of the floating point numbers (or similar)
  used internally in real number and function approximations.
-}
type Granularity = Int

class HasGranularity t where 
    {-| extract the internal granularity of a value of e -}
    getGranularity :: t -> Granularity
    
class (CanBeMutable t, HasGranularity t) => HasGranularityMutable t
    where
    {-| extract the internal granularity of a value stored in an ST variable -}
    getGranularityMutable :: Mutable t s -> ST s Granularity
    
class (HasGranularity t) => (CanSetGranularityOrder t) where
    setGranularityUp :: Granularity -> t -> t
    setGranularityDn :: Granularity -> t -> t
    setMinGranularityUp :: Granularity -> t -> t
    setMinGranularityDn :: Granularity -> t -> t

propSetGranularityUp :: (Poset t, CanSetGranularityOrder t) => t -> Granularity -> Bool
propSetGranularityUp e gr =
        (gr == getGranularity e') && (e' >= e)
        where
        e' = setGranularityUp gr e
 
propSetGranularityDn :: (Poset t, CanSetGranularityOrder t) => t -> Granularity -> Bool
propSetGranularityDn e gr =
        (gr == getGranularity e') && (e' <= e)
        where
        e' = setGranularityDn gr e
 
propSetMinGranularityUp :: (Poset t, CanSetGranularityOrder t) => t -> Granularity -> Bool
propSetMinGranularityUp e gr =
        (gr Prelude.<= getGranularity e') && (e' >= e)
        where
        e' = setMinGranularityUp gr e
 
propSetMinGranularityDn :: (Poset t, CanSetGranularityOrder t) => t -> Granularity -> Bool
propSetMinGranularityDn e gr =
        (gr Prelude.<= getGranularity e') && (e' <= e)
        where
        e' = setMinGranularityDn gr e
 
class (HasGranularity t) => (CanSetGranularityEnclosure t) where
    setGranularityOut :: Granularity -> t -> t
    setGranularityIn :: Granularity -> t -> t
    setMinGranularityOut :: Granularity -> t -> t
    setMinGranularityIn :: Granularity -> t -> t

-- TODO: add analogous properties as for CanSetGranularityOrder

class (CanSetGranularityOrder t, CanBeMutable t) => (CanSetGranularityOrderMutable t) where
    setGranularityUpMutable :: Granularity -> Mutable t s -> ST s ()
    setGranularityDnMutable :: Granularity -> Mutable t s -> ST s ()
    setMinGranularityUpMutable :: Granularity -> Mutable t s -> ST s ()
    setMinGranularityDnMutable :: Granularity -> Mutable t s -> ST s ()

-- TODO: add analogous properties as for CanSetGranularityOrder

class (CanSetGranularityEnclosure t, CanBeMutable t) => (CanSetGranularityEnclosureMutable t) where
    setGranularityOutMutable :: Granularity -> Mutable t s -> ST s ()
    setGranularityInMutable :: Granularity -> Mutable t s -> ST s ()
    setMinGranularityOutMutable :: Granularity -> Mutable t s -> ST s ()
    setMinGranularityInMutable :: Granularity -> Mutable t s -> ST s ()

-- TODO: add analogous properties as for CanSetGranularityOrderMutable
    