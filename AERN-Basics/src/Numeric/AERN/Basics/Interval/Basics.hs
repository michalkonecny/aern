{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.Basics
    Description :  interval datatype and its basic instances 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval datatype and its basic instances.
    
    This is a hidden module reexported via its parent.
-}
module Numeric.AERN.Basics.Interval.Basics 
(
   Interval(..)
)
where

import Numeric.AERN.Basics.CInterval
import Numeric.AERN.Basics.Granularity
import Numeric.AERN.Basics.CInterval.Granularity

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder ((<=?))

{-|
    Pairs of endpoints.  An end user should not use this type directly
    but use the classes of which this is an instance.
-}
data Interval e =
    Interval
    { 
        lowEndpoint :: e,
        highEndpoint :: e
    }
    deriving (Eq)
    
instance (Show e) => (Show (Interval e))
    where
    show (Interval l h) = "[" ++ show l ++ "," ++ show h ++ "]"

instance CInterval (Interval e) where
    type Endpoint (Interval e) = e
    getEndpoints (Interval l h) = (l, h)
    fromEndpoints (l,h) = Interval l h     
    mapEndpoints f (Interval l h) = Interval (f l) (f h)
    mapBothEndpoints fl fh (Interval l h) = Interval (fl l) (fh h)
    mapEndpointPair f (Interval l h) = 
        Interval l2 h2
        where
        (l2, h2) = f (l, h)

instance (HasGranularity e, NumOrd.Lattice (Granularity e)) => 
         HasGranularity (Interval e)
    where
    type Granularity (Interval e) = Granularity e
    getGranularity = getGranularityInterval
    initGranularityRounding (Interval l h) = initGranularityRounding l

instance (CanSetGranularityRoundedByNumericOrder e, NumOrd.Lattice (Granularity e)) => 
         CanSetGranularityRoundedByRefinementOrder (Interval e)
    where
    setGranularityOut = setGranularityOutInterval
    setGranularityIn = setGranularityInInterval
    setMinGranularityOut = setMinGranularityOutInterval
    setMinGranularityIn = setMinGranularityInInterval



    