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

class (CanBeMutable e) => Enclosure e where 
    {--------- Granularity operations ------------} 
    {-| extract the internal granularity of a value of e -}
    getGranularity :: e -> Granularity
    getGranularityM :: Mutable e -> MutableMonad e Granularity
    setGranularityOut :: Granularity -> e -> e
    setGranularityIn :: Granularity -> e -> e
    setGranularityOutM :: Granularity -> Mutable e -> MutableMonad e ()
    setGranularityInM :: Granularity -> Mutable e -> MutableMonad e ()
    setMinGranularityOut :: Granularity -> e -> e
    setMinGranularityIn :: Granularity -> e -> e
    setMinGranularityOutM :: Granularity -> Mutable e -> MutableMonad e ()
    setMinGranularityInM :: Granularity -> Mutable e -> MutableMonad e ()

class (Enclosure e) => EnclosureInterval e where
    type EnclosureIntervalEndpoint e :: *
    getEndpoints :: e -> (EnclosureIntervalEndpoint e, EnclosureIntervalEndpoint e)
    fromEndpoints :: (EnclosureIntervalEndpoint e, EnclosureIntervalEndpoint e) -> e
	
