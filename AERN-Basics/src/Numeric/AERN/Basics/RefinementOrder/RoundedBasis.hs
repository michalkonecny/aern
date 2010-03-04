{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.RoundedBasis
    Description :  domain bases with outwards and inwards rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}
module Numeric.AERN.Basics.RefinementOrder.RoundedBasis 
where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Extrema

{-|
    A type with outward-rounding lattice operations.
-}
class OuterRoundedBasis t where
    partialJoinOut :: [EffortIndicator] -> t -> t -> Maybe t
    partialJoinOutDefaultEffort :: t -> [EffortIndicator]

    (<|\/>?) :: t -> t -> Maybe t
    
    a <|\/>? b = partialJoinOut (partialJoinOutDefaultEffort a) a b 

(<⊔>?) :: (OuterRoundedBasis t) => t -> t -> Maybe t
(<⊔>?) = (<|\/>?)

-- properties of OuterRoundedBasis (TODO)

{-|
    A type with outward-rounding lattice operations.
-}
class InnerRoundedBasis t where
    partialJoinIn :: [EffortIndicator] -> t -> t -> Maybe t
    partialJoinInDefaultEffort :: t -> [EffortIndicator]

    (>|\/<?) :: t -> t -> Maybe t
    
    a >|\/<? b = partialJoinIn (partialJoinInDefaultEffort a) a b 

(>⊔<?) :: (InnerRoundedBasis t) => t -> t -> Maybe t
(>⊔<?) = (>|\/<?)

-- properties of InnerRoundedBasis (TODO)
    
-- mutable versions (TODO)    