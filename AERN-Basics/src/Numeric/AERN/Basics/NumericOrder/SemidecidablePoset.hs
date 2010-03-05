{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.ApproxOrder
    Description :  posets with semideciable order  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    This module is hidden and reexported via its parent NumericOrder. 
-}

module Numeric.AERN.Basics.NumericOrder.SemidecidablePoset 
where

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Extrema
import Numeric.AERN.Basics.Laws.SemidecidableRelation

import Prelude hiding (EQ, LT, GT)


{-|
    A type with semi-decidable equality and partial order
-}
class (SemidecidableEq t) => SemidecidablePoset t where
    maybeCompare :: [EffortIndicator] -> t -> t -> Maybe PartialOrdering
    maybeCompareDefaultEffort :: t -> [EffortIndicator]
    
    -- | Semidecidable `is comparable to`.
    (<==>?)  :: t -> t -> Maybe Bool
    -- | Semidecidable `is not comparable to`.
    (</=>?)  :: t -> t -> Maybe Bool
    (<?)     :: t -> t -> Maybe Bool
    (<=?)    :: t -> t -> Maybe Bool
    (>=?)    :: t -> t -> Maybe Bool
    (>?)     :: t -> t -> Maybe Bool

    -- defaults for all convenience operations:
    a <?    b = fmap (== LT) (maybeCompare (maybeCompareDefaultEffort a) a b)
    a >?    b = fmap (== GT) (maybeCompare (maybeCompareDefaultEffort a) a b)
    a <==>? b = fmap (/= NC) (maybeCompare (maybeCompareDefaultEffort a) a b)
    a </=>? b = fmap (== NC) (maybeCompare (maybeCompareDefaultEffort a) a b)
    a <=?   b = 
        (a <? b) ||? (a ==? b)
    a >=?   b =
        (a >? b) ||? (a ==? b)

instance SemidecidablePoset Int where
    maybeCompare = maybeComparePreludeCompare    
    maybeCompareDefaultEffort _ = []
    
maybeComparePreludeCompare _ a b =
    Just $ toPartialOrdering $ Prelude.compare a b

propExtremaInSemidecidablePoset :: (SemidecidablePoset t, HasExtrema t) => t -> Bool
propExtremaInSemidecidablePoset = semidecidableOrderExtrema (<=?) bottom top

-- TODO: other properties of semidecidable posets    

