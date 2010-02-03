{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Enclosure
    Description :  Enclosures containing some exact values.
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Enclosure where

import Numeric.AERN.BasicTypes
import Numeric.AERN.Mutable

import Control.Monad.ST (ST)

class (CanBeMutable e) => Enclosure e where 
    {--------- Granularity operations ------------} 
    {-| extract the internal granularity of a value of e -}
    getGranularity :: e -> Granularity
    getGranularityM :: Mutable e s -> ST s Granularity
    setGranularityOut :: Granularity -> e -> e
    setGranularityIn :: Granularity -> e -> e
    setGranularityOutM :: Granularity -> Mutable e s -> ST s ()
    setGranularityInM :: Granularity -> Mutable e s -> ST s ()
    setMinGranularityOut :: Granularity -> e -> e
    setMinGranularityIn :: Granularity -> e -> e
    setMinGranularityOutM :: Granularity -> Mutable e s -> ST s ()
    setMinGranularityInM :: Granularity -> Mutable e s -> ST s ()

class (Enclosure e) => EnclosureInterval e where
    type EnclosureIntervalEndpoint e :: *
    getEndpoints :: e -> (EnclosureIntervalEndpoint e, EnclosureIntervalEndpoint e)
    fromEndpoints :: (EnclosureIntervalEndpoint e, EnclosureIntervalEndpoint e) -> e
	
