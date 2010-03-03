{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.ApproxOrder
    Description :  posets with semideciable order  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}

module Numeric.AERN.Basics.RefinementOrder.SemidecidablePoset 
where

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Extrema
import Numeric.AERN.Basics.Laws.SemidecidableRelation

import Prelude hiding (LT, GT)


{-|
    A type with semi-decidable equality and partial order
-}
class (SemidecidableEq t) => SemidecidablePoset t where
    maybeCompare :: [EffortIndicator] -> t -> t -> Maybe PartialOrdering
    maybeCompareDefaultEffort :: t -> [EffortIndicator]
    
    -- | Semidecidable `is comparable to`.
    (|<==>?)  :: t -> t -> Maybe Bool
    -- | Semidecidable `is not comparable to`.
    (|</=>?)  :: t -> t -> Maybe Bool
    (|<?)     :: t -> t -> Maybe Bool
    (|<=?)    :: t -> t -> Maybe Bool
    (|>=?)    :: t -> t -> Maybe Bool
    (|>?)     :: t -> t -> Maybe Bool

    -- defaults for all convenience operations:
    a |<?    b = fmap (== LT) (maybeCompare (maybeCompareDefaultEffort a) a b)
    a |>?    b = fmap (== GT) (maybeCompare (maybeCompareDefaultEffort a) a b)
    a |<==>? b = fmap (/= NC) (maybeCompare (maybeCompareDefaultEffort a) a b)
    a |</=>? b = fmap (== NC) (maybeCompare (maybeCompareDefaultEffort a) a b)
    a |<=?   b = 
        (a |<? b) ||? (a ==? b)
    a |>=?   b =
        (a |>? b) ||? (a ==? b)

-- convenience Unicode math operator notation:
(⊏?) :: (SemidecidablePoset t) => t -> t -> Maybe Bool
(⊏?) = (|<?)
(⊑?) :: (SemidecidablePoset t) => t -> t -> Maybe Bool
(⊑?) = (|<=?)
(⊒?) :: (SemidecidablePoset t) => t -> t -> Maybe Bool
(⊒?) = (|>=?)
(⊐?) :: (SemidecidablePoset t) => t -> t -> Maybe Bool
(⊐?) = (|>?)

propExtremaInSemidecidablePoset :: (SemidecidablePoset t, HasExtrema t) => t -> Bool
propExtremaInSemidecidablePoset = semidecidableOrderExtrema (|<=?) bottom top

-- TODO: other properties of semidecidable posets    

