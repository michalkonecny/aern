{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.Granularity
    Description :  granularity of finite subsets of continuum sets
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Specifying granularity of finite subsets of continuum sets, eg 
    granularity of floating point numbers (usually called precision).
-}
module Numeric.AERN.Basics.Granularity where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.PartialComparison ((<=?),(>=?))
import Numeric.AERN.Basics.NumericOrder.Comparison ((<=),(>=))

--import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
--import Numeric.AERN.Basics.RefinementOrder.PartialComparison ((|<=?),(|>=?))
--import Numeric.AERN.Basics.RefinementOrder.Comparison ((|<=),(|>=))

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
    initGranularityRounding :: t -> IO ()
    
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
        (NumOrd.Comparison t, CanSetGranularityRoundedByNumericOrder t, Eq (Granularity t)) => 
        t -> t -> Granularity t -> Bool
propSetGranularityUp _ e gr =
        (gr == getGranularity e') && (e' >= e)
        where
        e' = setGranularityUp gr e
 
propSetGranularityDn :: 
        (NumOrd.Comparison t, CanSetGranularityRoundedByNumericOrder t, Eq (Granularity t)) => 
        t -> t -> Granularity t -> Bool
propSetGranularityDn _ e gr =
        (gr == getGranularity e') && (e' <= e)
        where
        e' = setGranularityDn gr e
 
propSetMinGranularityUp :: 
        (NumOrd.Comparison t, CanSetGranularityRoundedByNumericOrder t, NumOrd.Comparison (Granularity t)) => 
        t -> t -> Granularity t -> Bool
propSetMinGranularityUp _ e gr =
        (gr <= getGranularity e') && (e' >= e)
        where
        e' = setMinGranularityUp gr e
 
propSetMinGranularityDn :: 
        (NumOrd.Comparison t, CanSetGranularityRoundedByNumericOrder t, NumOrd.Comparison (Granularity t)) => 
        t -> t -> Granularity t -> Bool
propSetMinGranularityDn _ e gr =
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
    