{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.NumericOrder.Poset
import Numeric.AERN.Basics.Enclosure
import Numeric.AERN.Basics.ApproxOrder
import Numeric.AERN.Basics.ApproxEnclosure

import qualified Prelude
import Prelude hiding (compare, (<=), (>=))

class HasGranularity t where
    {-|
      A measure used internally to limit the size of the
      values of t eg when constructing new values from old ones.
      (Think eg of the mantisa size of a floating point number
       or the degree limit of a polynomial.)
    -}
    type Granularity t
    {-| extract the internal granularity of a value of e -}
    getGranularity :: t -> Granularity t
    
class (CanBeMutable t, HasGranularity t) => HasGranularityMutable t
    where
    {-| extract the internal granularity of a value stored in an ST variable -}
    getGranularityMutable :: Mutable t s -> ST s (Granularity t)
    
class (HasGranularity t) => (CanSetGranularityRoundedByNumericOrder t) where
    setGranularityUp :: Granularity t -> t -> t
    setGranularityDn :: Granularity t -> t -> t
    setMinGranularityUp :: Granularity t -> t -> t
    setMinGranularityDn :: Granularity t -> t -> t

propSetGranularityUp ::
        (Poset t, CanSetGranularityRoundedByNumericOrder t, Eq (Granularity t)) => 
        t -> Granularity t -> Bool
propSetGranularityUp e gr =
        (gr == getGranularity e') && (e' >= e)
        where
        e' = setGranularityUp gr e
 
propSetGranularityDn :: 
        (Poset t, CanSetGranularityRoundedByNumericOrder t, Eq (Granularity t)) => 
        t -> Granularity t -> Bool
propSetGranularityDn e gr =
        (gr == getGranularity e') && (e' <= e)
        where
        e' = setGranularityDn gr e
 
propSetMinGranularityUp :: 
        (Poset t, CanSetGranularityRoundedByNumericOrder t, Poset (Granularity t)) => 
        t -> Granularity t -> Bool
propSetMinGranularityUp e gr =
        (gr <= getGranularity e') && (e' >= e)
        where
        e' = setMinGranularityUp gr e
 
propSetMinGranularityDn :: 
        (Poset t, CanSetGranularityRoundedByNumericOrder t, Poset (Granularity t)) => 
        t -> Granularity t -> Bool
propSetMinGranularityDn e gr =
        (gr <= getGranularity e') && (e' <= e)
        where
        e' = setMinGranularityDn gr e
 
class (HasGranularity t) => (CanSetGranularityRoundedByRefinementOrder t) where
    setGranularityOut :: Granularity t -> t -> t
    setGranularityIn :: Granularity t -> t -> t
    setMinGranularityOut :: Granularity t -> t -> t
    setMinGranularityIn :: Granularity t -> t -> t

-- TODO: add analogous properties as for CanSetGranularityOrder

class (CanSetGranularityRoundedByNumericOrder t, CanBeMutable t) => 
        (CanSetGranularityRoundedByNumericOrderMutable t) where
    setGranularityUpMutable :: Granularity t -> Mutable t s -> ST s ()
    setGranularityDnMutable :: Granularity t -> Mutable t s -> ST s ()
    setMinGranularityUpMutable :: Granularity t -> Mutable t s -> ST s ()
    setMinGranularityDnMutable :: Granularity t -> Mutable t s -> ST s ()

-- TODO: add analogous properties as for CanSetGranularityOrder

class (CanSetGranularityRoundedByRefinementOrder t, CanBeMutable t) => 
        (CanSetGranularityRoundedByRefinementOrderMutable t) where
    setGranularityOutMutable :: Granularity t -> Mutable t s -> ST s ()
    setGranularityInMutable :: Granularity t -> Mutable t s -> ST s ()
    setMinGranularityOutMutable :: Granularity t -> Mutable t s -> ST s ()
    setMinGranularityInMutable :: Granularity t -> Mutable t s -> ST s ()

-- TODO: add analogous properties as for CanSetGranularityOrderMutable
    