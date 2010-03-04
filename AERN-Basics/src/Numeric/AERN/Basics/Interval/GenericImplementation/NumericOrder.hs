{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.GenericImplementation.NumericOrder
    Description :  numeric-ordered operations for any CInterval instance 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Interval.GenericImplementation.NumericOrder where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Interval
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

maybeEqualDefaultEffortInterval ::
        (CInterval i, SemidecidableEq (Endpoint i)) => 
        i -> [EffortIndicator]
maybeEqualDefaultEffortInterval i =
    zipWith Prelude.max
        (maybeEqualDefaultEffort l)
        (maybeEqualDefaultEffort h)
    where
    (l,h) = getEndpoints i

maybeEqualInterval ::
        (CInterval i, NumOrd.SemidecidablePoset (Endpoint i)) => 
        [EffortIndicator] -> i -> i -> Maybe Bool
maybeEqualInterval effort i1 i2 = 
    case (c l1 l2, c l1 h2, c h1 l2, c h1 h2) of
        (Just EQ, Just EQ, Just EQ, _) -> Just True
        (Just LT, Just LT, Just LT, Just LT) -> Just False  
        (Just GT, Just GT, Just GT, Just GT) -> Just False
        (Just NC, Just NC, Just NC, Just NC) -> Just False
        _ -> Nothing
    where
    c = NumOrd.maybeCompare effort 
    (l1, h1) = getEndpoints i1    
    (l2, h2) = getEndpoints i2
     
maybeCompareDefaultEffortInterval ::
        (CInterval i, NumOrd.SemidecidablePoset (Endpoint i)) => 
        i -> [EffortIndicator]
maybeCompareDefaultEffortInterval i =
    zipWith Prelude.max
        (NumOrd.maybeCompareDefaultEffort l)
        (NumOrd.maybeCompareDefaultEffort h)
    where
    (l,h) = getEndpoints i
        
maybeCompareInterval ::
        (CInterval i, NumOrd.SemidecidablePoset (Endpoint i)) => 
        [EffortIndicator] -> i -> i -> Maybe PartialOrdering
maybeCompareInterval effort i1 i2 = 
    case (c l1 l2, c l1 h2, c h1 l2, c h1 h2) of
        (Just EQ, Just EQ, Just EQ, _) -> Just EQ
        (Just LT, Just LT, Just LT, Just LT) -> Just LT  
        (Just GT, Just GT, Just GT, Just GT) -> Just GT
        (Just NC, Just NC, Just NC, Just NC) -> Just NC
        _ -> Nothing
    where
    c = NumOrd.maybeCompare effort 
    (l1, h1) = getEndpoints i1    
    (l2, h2) = getEndpoints i2

minInterval ::
    (CInterval i, NumOrd.Lattice (Endpoint i)) =>
    i -> i -> i
minInterval i1 i2 =
    fromEndpoints (NumOrd.min l1 l2, NumOrd.min h1 h2)
    where
    (l1, h1) = getEndpoints i1
    (l2, h2) = getEndpoints i2

maxInterval ::
    (CInterval i, NumOrd.Lattice (Endpoint i)) =>
    i -> i -> i
maxInterval i1 i2 =
    fromEndpoints (NumOrd.max l1 l2, NumOrd.max h1 h2)
    where
    (l1, h1) = getEndpoints i1
    (l2, h2) = getEndpoints i2

    