{-|
    Module      :  Numeric.AERN.Basics.ApproxOrder
    Description :  posets and lattices with semideciable order and approximate ops  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.ApproxOrder where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Extrema

import Prelude hiding (LT, GT)
import Test.QuickCheck

{-|
    A type with semi-decidable equality and partial order
-}
class (SemidecidableEq t) => SemidecidablePoset t where
    maybeCompare :: t -> t -> Maybe PartialOrdering
    -- | Semidecidable `is comparable to`.
    (<==>?)  :: t -> t -> Maybe Bool
    -- | Semidecidable `is not comparable to`.
    (</=>?)  :: t -> t -> Maybe Bool
    (<?)     :: t -> t -> Maybe Bool
    (<=?)    :: t -> t -> Maybe Bool
    (>=?)    :: t -> t -> Maybe Bool
    (>?)     :: t -> t -> Maybe Bool

    -- defaults for all but maybeCompare:
    a <?    b = fmap (== LT) (a `maybeCompare` b)  
    a >?    b = fmap (== GT) (a `maybeCompare` b)
    a <==>? b = fmap (/= NC) (a `maybeCompare` b)
    a </=>? b = fmap (== NC) (a `maybeCompare` b)
    a <=?   b = 
        (a <? b) ||? (a ==? b)
    a >=?   b =
        (a >? b) ||? (a ==? b)

propExtremaInSemidecidablePoset :: (SemidecidablePoset t, HasExtrema t) => t -> Bool
propExtremaInSemidecidablePoset e =
    (trueOrNothing $ bottom <=? e) 
    && 
    (trueOrNothing $ e <=? top)

-- TODO: other properties of semidecidable posets    

{-|
    A type with directed-rounding lattice operations.
-}
class ApproxLattice t where
    joinUp :: t -> t -> t
    joinDn :: t -> t -> t
    meetUp :: t -> t -> t
    meetDn :: t -> t -> t
    
(\/^) :: (ApproxLattice t) => t -> t -> t
(\/^) = joinUp

(∨^) :: (ApproxLattice t) => t -> t -> t
(∨^) = joinUp

(\/.) :: (ApproxLattice t) => t -> t -> t
(\/.) = joinDn 
    
(∨.) :: (ApproxLattice t) => t -> t -> t
(∨.) = joinDn 
    
(/\^) :: (ApproxLattice t) => t -> t -> t
(/\^) = meetUp

(∧^) :: (ApproxLattice t) => t -> t -> t
(∧^) = meetUp

(/\.) :: (ApproxLattice t) => t -> t -> t
(/\.) = meetDn 
    
(∧.) :: (ApproxLattice t) => t -> t -> t
(∧.) = meetDn 
    
-- TODO: properties of approximate lattices 
--       with semidecidable partial order     

{-|
    A type with directed-rounding lattice operations
    that also supported in-place.
-}
class (ApproxLattice t, CanBeMutable t) => ApproxLatticeMutable t where
    {-| joinUpMutable a b c means a := b \/^ c; a can be the same as b and/or c -}
    joinUpMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| joinDnMutable a b c means a := b \/. c; a can be the same as b and/or c -}
    joinDnMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| meetUpMutable a b c means a := b /\^ c; a can be the same as b and/or c -}
    meetUpMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| meetDnMutable a b c means a := b /\. c; a can be the same as b and/or c -}
    meetDnMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

    -- TODO: add default implementations using read/write
    