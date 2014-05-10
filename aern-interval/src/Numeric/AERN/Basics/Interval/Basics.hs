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
   IntervalOrderEffort(..), defaultIntervalOrderEffort, 
   getEndpoints, fromEndpoints, mapBothEndpoints, mapEachEndpoint, mapEndpointPair 
)
where

import Prelude hiding (EQ, LT, GT)


import qualified 
       Numeric.AERN.NumericOrder as NumOrd

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.SizeLimits

import Test.QuickCheck (Arbitrary) --, arbitrary, vectorOf)

import Control.DeepSeq

{-|
    Pairs of endpoints.  An end user should not use this type directly
    but use the classes of which this is an instance. 
-}
data Interval e =
    Interval
    { 
        leftEndpoint :: !e,
        rightEndpoint :: !e
    }
    
data IntervalOrderEffort e =
    IntervalOrderEffort
    {
        intordeff_eComp :: NumOrd.PartialCompareEffortIndicator e,
        intordeff_eMinmax :: NumOrd.MinmaxEffortIndicator e
    }
    
defaultIntervalOrderEffort :: 
    (NumOrd.RoundedLatticeEffort e, NumOrd.PartialComparison e)
    =>
    Interval e -> IntervalOrderEffort e
defaultIntervalOrderEffort (Interval sampleE _) =
    IntervalOrderEffort
    {
        intordeff_eComp = NumOrd.pCompareDefaultEffort sampleE,
        intordeff_eMinmax = NumOrd.minmaxDefaultEffort sampleE
    }
    
-- TODO: complete the following instances:
instance Arbitrary (IntervalOrderEffort e)
instance Show (IntervalOrderEffort e)
instance EffortIndicator (IntervalOrderEffort e)

    
instance (ShowInternals e, NumOrd.PartialComparison e) => (ShowInternals (Interval e))
    where
    type ShowInternalsIndicator (Interval e) = ShowInternalsIndicator e
    defaultShowIndicator (Interval l _r) = defaultShowIndicator l
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

instance (HasSizeLimits e) => HasSizeLimits (Interval e)
    where
    type SizeLimits (Interval e) = SizeLimits e
    getSizeLimits (Interval l _r) = getSizeLimits l
    defaultSizeLimits (Interval l _r) = defaultSizeLimits l
    
instance (CanChangeSizeLimits e) => CanChangeSizeLimits (Interval e)
    where
    type SizeLimitsChangeEffort (Interval e) = SizeLimitsChangeEffort e
    sizeLimitsChangeDefaultEffort (Interval l _r) = sizeLimitsChangeDefaultEffort l
    changeSizeLimitsOutEff eff newSizeLimits (Interval l r) = 
        Interval 
            (changeSizeLimitsDnEff eff newSizeLimits l) 
            (changeSizeLimitsUpEff eff newSizeLimits r)
    changeSizeLimitsInEff eff newSizeLimits (Interval l r) = 
        Interval 
            (changeSizeLimitsUpEff eff newSizeLimits l) 
            (changeSizeLimitsDnEff eff newSizeLimits r)
    changeSizeLimitsDnEff = error $ "AERN: changeSizeLimitsDnEff not defined for type Interval."
    changeSizeLimitsUpEff = error $ "AERN: changeSizeLimitsUpEff not defined for type Interval."

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

mapBothEndpoints :: (t -> e) -> Interval t -> Interval e
mapBothEndpoints f (Interval l r) = Interval (f l) (f r)

mapEachEndpoint :: (t -> e) -> (t -> e) -> Interval t -> Interval e
mapEachEndpoint fl fh (Interval l r) = Interval (fl l) (fh r)

mapEndpointPair :: ((t, t) -> (e, e)) -> Interval t -> Interval e
mapEndpointPair f (Interval l r) = 
    Interval fl fr
    where
    (fl, fr) = f (l, r)




    
