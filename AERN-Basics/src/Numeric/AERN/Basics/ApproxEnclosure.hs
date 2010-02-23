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
import Data.Maybe (isJust, isNothing)
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
   A type with a semidecidable disjointness test.
-}
class (Eq t) => EnclosureSemidecidableDisjoint t where
    maybeDisjoint :: t -> t -> Maybe Bool

-- TODO: add symmetry

{-|
    A set-based outer-rounded disjoint-intersection partial semi-lattice 
    behaving similarly to a basis of a domain.
    
    Intersection should be idempotent
    as well as commutative and associative where defined.  
-}
class (EnclosureSemidecidableDisjoint t) => EnclosureOuterRoundedBasis t where
    maybeIntersectionOut :: t -> t -> Maybe t

-- convenient notation:
(<@/\>?) :: (EnclosureOuterRoundedBasis t) => t -> t -> Maybe t
(<@/\>?) = maybeIntersectionOut

(<∩>?) :: (EnclosureOuterRoundedBasis t) => t -> t -> Maybe t
(<∩>?) = maybeIntersectionOut

{-| when disjointness is dedicable, two elements are disjoint iff they have no intersection -}
propEnclosureNonDisjointOuterIntersection :: (EnclosureOuterRoundedBasis t) => t -> t -> Bool
propEnclosureNonDisjointOuterIntersection e1 e2 =
    (notJustTrue (e1 `maybeDisjoint` e2) || (isJust $ e1 <∩>? e2))
    &&
    ((falseOrNothing (e1 `maybeDisjoint` e2)) || (isNothing $ e1 <∩>? e2))

-- TODO: add outerPartialIntersection, outerIdempotency, partialOuterCommutativity and partialOuterAssociativity

{-|
    A set-based inner-rounded disjoint-intersection partial semi-lattice 
    behaving similarly to a basis of a domain.
    
    Intersection should be idempotent
    as well as commutative and associative where defined.  
-}
class (EnclosureSemidecidableDisjoint t) => EnclosureInnerRoundedBasis t where
    maybeIntersectionIn :: t -> t -> Maybe t

-- convenient notation:
(>@/\<?) :: (EnclosureInnerRoundedBasis t) => t -> t -> Maybe t
(>@/\<?) = maybeIntersectionIn

(>∩<?) :: (EnclosureInnerRoundedBasis t) => t -> t -> Maybe t
(>∩<?) = maybeIntersectionIn

{-| when disjointness is dedicable, two elements are disjoint iff they have no intersection -}
propEnclosureNonDisjointInnerIntersection :: (EnclosureInnerRoundedBasis t) => t -> t -> Bool
propEnclosureNonDisjointInnerIntersection e1 e2 =
    (notJustTrue (e1 `maybeDisjoint` e2) || (isJust $ e1 >∩<? e2))
    &&
    ((falseOrNothing (e1 `maybeDisjoint` e2)) || (isNothing $ e1 >∩<? e2))

-- TODO: add innerPartialIntersection, innerIdempotency, partialInnerCommutativity and partialInnerAssociativity


{-|
    A set-based lattice with outwards and inwards directed-rounding operations.  
-}
class (Eq t) => EnclosureOuterLattice t where
    intersectionOut :: t -> t -> t
    unionOut :: t -> t -> t

{-|
    A set-based lattice with outwards and inwards directed-rounding operations.  
-}
class (Eq t) => EnclosureInnerLattice t where
    intersectionIn :: t -> t -> t
    unionIn :: t -> t -> t

{-|
    A set-based lattice with outwards and inwards directed-rounding operations.  
-}
class (EnclosureInnerLattice t, EnclosureOuterLattice t) => EnclosureOuterInnerLattice t where

(<@\/>) :: (EnclosureOuterLattice t) => t -> t -> t
(<@\/>) = unionOut

(<∪>) :: (EnclosureOuterLattice t) => t -> t -> t
(<∪>) = unionOut

(>@\/<) :: (EnclosureInnerLattice t) => t -> t -> t
(>@\/<) = unionIn

(>∪<) :: (EnclosureInnerLattice t) => t -> t -> t
(>∪<) = unionIn

(<@/\>) :: (EnclosureOuterLattice t) => t -> t -> t
(<@/\>) = intersectionOut

(<∩>) :: (EnclosureOuterLattice t) => t -> t -> t
(<∩>) = intersectionOut

(>@/\<) :: (EnclosureInnerLattice t) => t -> t -> t
(>@/\<) = intersectionIn

(>∩<) :: (EnclosureInnerLattice t) => t -> t -> t
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
