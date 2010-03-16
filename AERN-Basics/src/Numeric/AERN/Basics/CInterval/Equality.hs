{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.CInterval.Equality
    Description :  equality tests for any CInterval instance 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Equality tests for any 'CInterval' instance.
-}
module Numeric.AERN.Basics.CInterval.Equality where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.CInterval
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.PartialOrdering

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

--{-|
--    Default default effort indicators for testing numerical equality for interval types. 
---}
--maybeEqualDefaultEffortInterval ::
--        (CInterval i, SemidecidableEq (Endpoint i)) => 
--        i -> [EffortIndicator]
--maybeEqualDefaultEffortInterval i =
--    zipWith Prelude.max
--        (maybeEqualDefaultEffort l)
--        (maybeEqualDefaultEffort h)
--    where
--    (l,h) = getEndpoints i

{-|
    Default numerical equality test for interval types.
-}
maybeEqualEffInterval ::
        (CInterval i, NumOrd.SemidecidablePoset (Endpoint i)) => 
        [EffortIndicator] -> i -> i -> Maybe Bool
maybeEqualEffInterval effort i1 i2 = 
    case (c l1 l2, c l1 h2, c h1 l2, c h1 h2) of
        (Just EQ, Just EQ, Just EQ, _) -> Just True
        (Just LT, Just LT, Just LT, Just LT) -> Just False  
        (Just GT, Just GT, Just GT, Just GT) -> Just False
        (Just NC, Just NC, Just NC, Just NC) -> Just False
        _ -> Nothing
    where
    c = NumOrd.maybeCompareEff effort 
    (l1, h1) = getEndpoints i1    
    (l2, h2) = getEndpoints i2
     
