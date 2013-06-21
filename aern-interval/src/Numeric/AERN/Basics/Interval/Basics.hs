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

import qualified Numeric.AERN.NumericOrder as NumOrd

import Control.DeepSeq

{-|
    Pairs of endpoints.  An end user should not use this type directly
    but use the classes of which this is an instance. 
-}
data Interval e =
    Interval
    { 
        leftEndpoint :: ! e,
        rightEndpoint :: ! e
    }
    
instance (ShowInternals e, NumOrd.PartialComparison e) => (ShowInternals (Interval e))
    where
    type ShowInternalsIndicator (Interval e) = ShowInternalsIndicator e
    defaultShowIndicator (Interval l r) = defaultShowIndicator l
    showInternals indicator (Interval l r) =
        case (NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort l) l r) of
            Just EQ -> "<" ++ showL ++ ">"
            Just LT -> showConsistent
--            Just LEE -> showConsistent
            Just GT -> showAnticonsistent
--            Just GEE -> showAnticonsistent
            _ -> showUnknown
        where
        showL = showInternals indicator l
        showR = showInternals indicator r
        showConsistent = "[_" ++ showL ++ "," ++ showR ++ "^]"
        showAnticonsistent = "[^" ++ showL ++ "," ++ showR ++ "_]"
        showUnknown = "[?" ++ showL ++ "," ++ showR ++ "?]"

instance (ShowInternals e, NumOrd.PartialComparison e) => (Show (Interval e))
    where
    show i = showInternals (defaultShowIndicator i) i

instance (NFData e) => NFData (Interval e) where
    rnf (Interval l r) =
        rnf l `seq` rnf r `seq` () 
--        l `seq` r `seq` () 

-- | Given an argument interval 'i' 'getEndpoints' returns the endpoint pair 
--   ('leftEndpoint' 'i','rightEndpoint' 'i').
getEndpoints :: Interval t -> (t,t)
getEndpoints (Interval l r) = (l, r)

-- | Constructs an interval from an endpoint pair.
fromEndpoints :: (t,t) -> Interval t
fromEndpoints (l,r) = Interval l r 

mapBothEndpoints f (Interval l r) = Interval (f l) (f r)

mapEachEndpoint fl fh (Interval l r) = Interval (fl l) (fh r)

mapEndpointPair f (Interval l r) = 
    Interval fl fr
    where
    (fl, fr) = f (l, r)




    
