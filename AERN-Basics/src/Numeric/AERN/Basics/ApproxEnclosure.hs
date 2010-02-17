{-|
    Module      :  Numeric.AERN.Basics.ApproxEnclosure
    Description :  sets with semideciable inclusion and approximate ops  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.ApproxEnclosure where

import Numeric.AERN.Basics.Laws
import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.Order
import Numeric.AERN.Basics.ApproxOrder
import Numeric.AERN.Basics.Mutable

import Prelude hiding (LT, GT)
import Control.Monad.ST (ST)
import Test.QuickCheck

{-|
    A type with semi-decidable equality and inclusion
-}
class (SemidecidableEq t) => SemidecidableEnclosure t where
    maybeCompareEncl :: t -> t -> Maybe PartialOrdering
    -- | Semidecidable `is comparable to`.
    (@<==>?)  :: t -> t -> Maybe Bool
    -- | Semidecidable `is not comparable to`.
    (@</=>?)  :: t -> t -> Maybe Bool
    (@<?)     :: t -> t -> Maybe Bool
    (@<=?)    :: t -> t -> Maybe Bool
    (@>=?)    :: t -> t -> Maybe Bool
    (@>?)     :: t -> t -> Maybe Bool

    -- defaults for all but maybeCompare:
    a @<?    b = fmap (== LT) (a `maybeCompareEncl` b)  
    a @>?    b = fmap (== GT) (a `maybeCompareEncl` b)
    a @<==>? b = fmap (/= NC) (a `maybeCompareEncl` b)
    a @</=>? b = fmap (== NC) (a `maybeCompareEncl` b)
    a @<=?   b = 
        (a @<? b) ||? (a ==? b)
    a @>=?   b =
        (a @>? b) ||? (a ==? b)

-- convenience Unicode math operator notation:
(⊂?) :: (SemidecidableEnclosure t) => t -> t -> Maybe Bool
(⊂?) = (@<?)
(⊆?) :: (SemidecidableEnclosure t) => t -> t -> Maybe Bool
(⊆?) = (@<=?)
(⊇?) :: (SemidecidableEnclosure t) => t -> t -> Maybe Bool
(⊇?) = (@>=?)
(⊃?) :: (SemidecidableEnclosure t) => t -> t -> Maybe Bool
(⊃?) = (@>?)



propExtremaForSemidecidableEnclosures :: (SemidecidableEnclosure t, HasExtrema t) => t -> Bool
propExtremaForSemidecidableEnclosures e =
    (trueOrNothing $ bottom ⊆? e) 
    && 
    (trueOrNothing $ e ⊆? top)

-- TODO: adapt all semidecidable poset properties for semideciable enclosures

{-|
    A set-based lattice with directed-rounding operations.  
-}
class (Eq t) => EnclosureApproxLattice t where
    intersectionOut :: t -> t -> t
    intersectionIn :: t -> t -> t
    unionOut :: t -> t -> t
    unionIn :: t -> t -> t

(<@\/>) :: (EnclosureApproxLattice t) => t -> t -> t
(<@\/>) = unionOut

(<∪>) :: (EnclosureApproxLattice t) => t -> t -> t
(<∪>) = unionOut

(>@\/<) :: (EnclosureApproxLattice t) => t -> t -> t
(>@\/<) = unionIn

(>∪<) :: (EnclosureApproxLattice t) => t -> t -> t
(>∪<) = unionIn

(<@/\>) :: (EnclosureApproxLattice t) => t -> t -> t
(<@/\>) = intersectionOut

(<∩>) :: (EnclosureApproxLattice t) => t -> t -> t
(<∩>) = intersectionOut

(>@/\<) :: (EnclosureApproxLattice t) => t -> t -> t
(>@/\<) = intersectionIn

(>∩<) :: (EnclosureApproxLattice t) => t -> t -> t
(>∩<) = intersectionIn

{-|
    A lattice that supports in-place operations.
-}
class (Lattice t, CanBeMutable t) => EnclosureApproxLatticeMutable t where
    {-| unionOutMutable a b c means a := b <∪> c; a can be the same as b and/or c -}
    unionOutMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| unionInMutable a b c means a := b >∪< c; a can be the same as b and/or c -}
    unionInMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| intersectionOutMutable a b c means a := b <∩> c; a can be the same as b and/or c -}
    intersectionOutMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| intersectionInMutable a b c means a := b >∩< c; a can be the same as b and/or c -}
    intersectionInMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

    -- TODO: add default implementations using read/write

-- TODO: adapt all approximate lattice properties for approximate enclosure lattices
