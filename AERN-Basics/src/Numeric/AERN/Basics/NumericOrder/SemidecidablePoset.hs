{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.ApproxOrder
    Description :  posets with semideciable order  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Posets with semideciable order.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}

module Numeric.AERN.Basics.NumericOrder.SemidecidablePoset 
where

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Extrema
import Numeric.AERN.Basics.Laws.SemidecidableRelation

import Prelude hiding (EQ, LT, GT)


{-|
    A type with semi-decidable equality and partial order
-}
class (SemidecidableEq t) => SemidecidablePoset t where
    maybeCompareEff :: [EffortIndicator] -> t -> t -> Maybe PartialOrdering
    maybeCompareDefaultEffort :: t -> [EffortIndicator]
    
    maybeCompare :: t -> t -> Maybe PartialOrdering
    maybeCompare a = maybeCompareEff (maybeCompareDefaultEffort a) a
    
    -- | Semidecidable `is comparable to`.
    (<==>?)  :: t -> t -> Maybe Bool
    -- | Semidecidable `is not comparable to`.
    (</=>?)  :: t -> t -> Maybe Bool
    (<?)     :: t -> t -> Maybe Bool
    (<=?)    :: t -> t -> Maybe Bool
    (>=?)    :: t -> t -> Maybe Bool
    (>?)     :: t -> t -> Maybe Bool

    -- defaults for all convenience operations:
    a <?    b = fmap (== LT) (maybeCompare a b)
    a >?    b = fmap (== GT) (maybeCompare a b)
    a <==>? b = fmap (/= NC) (maybeCompare a b)
    a </=>? b = fmap (== NC) (maybeCompare a b)
    a <=?   b = 
        (a <? b) ||? (a ==? b)
    a >=?   b =
        (a >? b) ||? (a ==? b)

instance SemidecidablePoset Int where
    maybeCompareEff = maybeComparePreludeCompare    
    maybeCompareDefaultEffort _ = []
    
maybeComparePreludeCompare _ a b =
    Just $ toPartialOrdering $ Prelude.compare a b

propSemidecidablePosetEqCompatible :: (SemidecidablePoset t) => t -> t -> Bool
propSemidecidablePosetEqCompatible e1 e2 =
    (Just True == (e1 /=? e2) || (nothingOr EQ $ maybeCompare e1 e2))
    &&    
    (Just EQ /= (maybeCompare e1 e2) || nothingOr True (e1 ==? e2) )

--propSemidecidablePosetAntiSymmetric :: (SemidecidablePoset t) => t -> t -> Bool
--propSemidecidablePosetAntiSymmetric e1 e2 = 
--    compare e2 e1 == (partialOrderingTranspose $ compare e1 e2) 

propSemidecidablePosetTransitive :: (SemidecidablePoset t) => t -> t -> t -> Bool
propSemidecidablePosetTransitive = semidecidableTransitive (<=?)


propExtremaInSemidecidablePoset :: (SemidecidablePoset t, HasExtrema t) => t -> Bool
propExtremaInSemidecidablePoset = semidecidableOrderExtrema (<=?) least highest

-- TODO: other properties of semidecidable posets    

