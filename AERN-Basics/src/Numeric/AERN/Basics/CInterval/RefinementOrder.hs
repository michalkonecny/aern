{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.CInterval.NumericOrder
    Description :  refinement-ordered operations for any CInterval instance 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Refinement-ordered operations for any 'CInterval' instance.
-}
module Numeric.AERN.Basics.CInterval.RefinementOrder where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.CInterval
import Numeric.AERN.Basics.CInterval.NumericOrder
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

maybeCompareDefaultEffortIntervalRef ::
        (CInterval i, NumOrd.SemidecidablePoset (Endpoint i)) => 
        i -> [EffortIndicator]
maybeCompareDefaultEffortIntervalRef =
    maybeCompareDefaultEffortInterval
        
{-|
  For two intervals, attempt to decide the inclusion partial order.
-}
maybeCompareIntervalRef ::
        (CInterval i, NumOrd.SemidecidablePoset (Endpoint i)) => 
        [EffortIndicator] -> i -> i -> Maybe PartialOrdering
maybeCompareIntervalRef effort i1 i2 = 
    case (c l1 l2, c h1 h2) of
        (Just EQ, Just EQ) -> Just EQ
        (Just LT, Just GT) -> Just LT  
        (Just LT, Just EQ) -> Just LT  
        (Just EQ, Just GT) -> Just LT  
        (Just GT, Just LT) -> Just GT  
        (Just GT, Just EQ) -> Just GT  
        (Just EQ, Just LT) -> Just GT  
        (Just _, Just _) -> Just NC  
        _ -> Nothing
    where
    c = NumOrd.maybeCompareEff effort 
    (l1, h1) = getEndpoints i1    
    (l2, h2) = getEndpoints i2

{-|
  For two intervals, decide the inclusion partial order.
-}
compareIntervalRef ::
        (CInterval i, NumOrd.Poset (Endpoint i)) => 
        i -> i -> PartialOrdering
compareIntervalRef i1 i2 =
    case maybeCompareIntervalRef effort i1 i2 of
        Just r -> r 
    where
    effort = maybeCompareDefaultEffortIntervalRef i1

