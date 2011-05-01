{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.Basics
    Description :  interval datatype and its basic instances 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval datatype and its basic instances.
    
    This is a hidden module reexported via its parent.
-}
module Numeric.AERN.Basics.Interval.Basics 
(
   Interval(..), 
   getEndpoints, fromEndpoints, mapBothEndpoints, mapEachEndpoint, mapEndpointPair 
)
where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.PartialOrdering

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort

import Control.DeepSeq

{-|
    Pairs of endpoints.  An end user should not use this type directly
    but use the classes of which this is an instance. 
-}
data Interval e =
    Interval
    { 
        lowEndpoint :: ! e,
        highEndpoint :: ! e
    }
    
instance (ShowInternals e, NumOrd.PartialComparison e) => (ShowInternals (Interval e))
    where
    type ShowInternalsIndicator (Interval e) = ShowInternalsIndicator e
    defaultShowIndicator (Interval l h) = defaultShowIndicator l
    showInternals indicator (Interval l h) =
        case (NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort l) l h) of
            Just EQ -> "<" ++ showL ++ ">"
            Just LT -> showConsistent
            Just LEE -> showConsistent
            Just GT -> showAnticonsistent
            Just GEE -> showAnticonsistent
            _ -> showUnknown
        where
        showL = showInternals indicator l
        showH = showInternals indicator h
        showConsistent = "[." ++ showL ++ "," ++ showH ++ "^]"
        showAnticonsistent = "[^" ++ showL ++ "," ++ showH ++ ".]"
        showUnknown = "[?" ++ showL ++ "," ++ showH ++ "?]"

instance (ShowInternals e, NumOrd.PartialComparison e) => (Show (Interval e))
    where
    show i = showInternals (defaultShowIndicator i) i

instance (NFData e) => NFData (Interval e) where
    rnf (Interval l h) =
        rnf l `seq` rnf h `seq` () 
--        l `seq` h `seq` () 

-- | Given an argument interval 'i' 'getEndpoints' returns the endpoint pair 
--   ('lowEndpoint' 'i','highEndpoint' 'i').
getEndpoints :: Interval t -> (t,t)
getEndpoints (Interval l h) = (l, h)

-- | Constructs an interval from an endpoint pair.
fromEndpoints :: (t,t) -> Interval t
fromEndpoints (l,h) = Interval l h  

mapBothEndpoints f (Interval l h) = Interval (f l) (f h)
mapEachEndpoint fl fh (Interval l h) = Interval (fl l) (fh h)
mapEndpointPair f (Interval l h) = 
    Interval fl fh
    where
    (fl, fh) = f (l, h)




    
