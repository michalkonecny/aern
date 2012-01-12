{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RefinementOrder.IntervalLike
    Description :  types with endpoints
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Types whose values have endpoints.
-}

module Numeric.AERN.RefinementOrder.IntervalLike where

import Numeric.AERN.RefinementOrder.PartialComparison

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Laws.RoundedOperation

import Numeric.AERN.Misc.Bool
import Numeric.AERN.Misc.Maybe

import Data.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class
    (EffortIndicator (GetEndpointsEffortIndicator t), 
     EffortIndicator (FromEndpointsEffortIndicator t))
    => 
    IntervalLike t 
    where
    type GetEndpointsEffortIndicator t
    type FromEndpointsEffortIndicator t
    getEndpointsDefaultEffort :: t -> GetEndpointsEffortIndicator t
    fromEndpointsDefaultEffort :: t -> FromEndpointsEffortIndicator t
    getEndpointsInEff :: (GetEndpointsEffortIndicator t) -> t -> (t,t)
    getEndpointsOutEff :: (GetEndpointsEffortIndicator t) -> t -> (t,t)
    fromEndpointsInEff :: (FromEndpointsEffortIndicator t) -> (t,t) -> t 
    fromEndpointsOutEff :: (FromEndpointsEffortIndicator t) -> (t,t) -> t 
    -- versions  with default effort - can be optimised:
    getEndpointsInWithDefaultEffort :: (IntervalLike t) => t -> (t,t)
    getEndpointsInWithDefaultEffort a = getEndpointsInEff (getEndpointsDefaultEffort a) a
    getEndpointsOutWithDefaultEffort :: (IntervalLike t) => t -> (t,t)
    getEndpointsOutWithDefaultEffort a = getEndpointsOutEff (getEndpointsDefaultEffort a) a
    fromEndpointsInWithDefaultEffort :: (IntervalLike t) => (t,t) -> t
    fromEndpointsInWithDefaultEffort p@(l,r) = fromEndpointsInEff (fromEndpointsDefaultEffort l) p
    fromEndpointsOutWithDefaultEffort :: (IntervalLike t) => (t,t) -> t
    fromEndpointsOutWithDefaultEffort p@(l,r) = fromEndpointsOutEff (fromEndpointsDefaultEffort l) p
    
propEndpointsFromGet :: 
    (IntervalLike t, Eq t, 
     HasLegalValues t, PartialComparison t) 
    => 
    t -> 
    (GetEndpointsEffortIndicator t) -> 
    (FromEndpointsEffortIndicator t) -> 
    (PartialCompareEffortIndicator t) -> 
    t -> Bool
propEndpointsFromGet _ effGet effFrom effComp e =
    outerFromGet && innerFromGet
    where
    outerFromGet =
        leqIfDefined "outer (from.get) endpoints" (pLeqEff effComp) 
            (fromEndpointsOutEff effFrom $ getEndpointsOutEff effGet e) e 
    innerFromGet =
        leqIfDefined "outer (from.get) endpoints" (pLeqEff effComp) 
            e (fromEndpointsInEff effFrom $ getEndpointsInEff effGet e) 

testsEndpoints :: 
    (Arbitrary t, Show t, Eq t,
     IntervalLike t,
     HasLegalValues t, PartialComparison t) 
    => 
    (String, t) -> Test
testsEndpoints (name, sample) =
    testGroup (name ++ " consistency flip")
        [
         testProperty "interval <-> endpoints" (propEndpointsFromGet sample)
        ]
