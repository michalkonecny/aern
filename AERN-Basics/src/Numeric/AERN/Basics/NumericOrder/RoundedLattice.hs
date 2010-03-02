{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.RoundedLattice
    Description :  posets and lattices with semideciable order and approximate ops  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.NumericOrder.RoundedLattice 
where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Extrema
--import Numeric.AERN.Basics.Laws.SemidecidableRelation

{-|
    A type with directed-rounding lattice operations.
-}
class RoundedLattice t where
    maxUp :: [EffortIndicator] -> t -> t -> t
    maxDn :: [EffortIndicator] -> t -> t -> t
    minUp :: [EffortIndicator] -> t -> t -> t
    minDn :: [EffortIndicator] -> t -> t -> t
    
{-|
    A type with directed-rounding lattice operations
    that also supported in-place.
-}
class (RoundedLattice t, CanBeMutable t) => RoundedLatticeMutable t where
    {-| maxUpMutable e a b c means a := maxUp e b c; a can be the same as b and/or c -}
    maxUpMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| maxDnMutable e a b c means a := maxDn e b c; a can be the same as b and/or c -}
    maxDnMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| minUpMutable e a b c means a := minUp e b c; a can be the same as b and/or c -}
    minUpMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| minDnMutable e a b c means a := minDn e b c; a can be the same as b and/or c -}
    minDnMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

    -- TODO: add default implementations using read/write
    