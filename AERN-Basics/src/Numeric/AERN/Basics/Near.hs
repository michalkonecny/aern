{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.Near
    Description :  Finite values near some exact values.
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Near where

import Numeric.AERN.Basics.BasicTypes
import Numeric.AERN.Basics.Mutable

import Control.Monad.ST (ST)

{-|
	A class of types whose values approximate some exact
	values by being close to them in some unspecified distance.
	
	The exact values have to have some partial order associated
	with them becuase some operations on them are supposed to 
	round upwards or downwards.  This partial order can be either
    decidable or semi-decidable.
-}
class (CanBeMutable b) => Near b where
    {--------- Granularity operations ------------}	
    {-| extract the internal granularity of a value of b -}
    getGranularity :: b -> Granularity
    {-| extract the internal granularity of a mutable value b -}
    getGranularityM :: Mutable b s -> ST s Granularity
    {-| construct a b value from another one by altering its granularity, rounding upwards -}
    setGranularityUp :: Granularity -> b -> b
    {-| construct a b value from another one by altering its granularity, rounding downwards -}
    setGranularityDn :: Granularity -> b -> b
    {-| modify the granularity of a mutable value, rounding upwards -}
    setGranularityUpM :: Granularity -> Mutable b s -> ST s ()
    {-| modify the granularity of a mutable value, rounding downwards -}
    setGranularityDnM :: Granularity -> Mutable b s -> ST s ()
    {-| construct a b value from another one by altering its granularity, rounding upwards -}
    setMinGranularityUp :: Granularity -> b -> b
    {-| construct a b value from another one by altering its granularity, rounding downwards -}
    setMinGranularityDn :: Granularity -> b -> b
    {-| modify the granularity of a mutable value, rounding upwards -}
    setMinGranularityUpM :: Granularity -> Mutable b s -> ST s ()
    {-| modify the granularity of a mutable value, rounding downwards -}
    setMinGranularityDnM :: Granularity -> Mutable b s -> ST s ()

--propSetGetGranularity :: 
--propSetGetGranularityM ::
--propSetGranularityUp ::
--propSetGranularityDn ::
--propSetMinGranularityUp ::
--propSetMinGranularityDn ::
--propSetGranularityUpM ::
--propSetGranularityDnM ::
--propSetMinGranularityUpM ::
--propSetMinGranularityDnM ::
