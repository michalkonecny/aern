{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.Enclosure
    Description :  set notation (⊂,∪,∩) and intervals
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Enclosure where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Extrema

import Prelude hiding (LT, GT, EQ)
import Data.Maybe (isJust)
import Control.Monad.ST (ST)

{-|
    A partially ordered set using set inclusion notation.
    
    (More-or-less copied from Data.Poset 
     in package altfloat-0.3 by Nick Bowler.) 
-} 
class (Eq t) => EnclosurePoset t where
    compareEncl :: t -> t -> PartialOrdering
    -- | Is comparable to.
    (@<==>)  :: t -> t -> Bool
    -- | Is not comparable to.
    (@</=>)  :: t -> t -> Bool
    (@<)     :: t -> t -> Bool
    (@<=)    :: t -> t -> Bool
    (@>=)    :: t -> t -> Bool
    (@>)     :: t -> t -> Bool

    -- defaults for all but compare:
    a @<    b = a `compareEncl` b == LT
    a @>    b = a `compareEncl` b == GT
    a @<==> b = a `compareEncl` b /= NC
    a @</=> b = a `compareEncl` b == NC
    a @<=   b = a @< b || a `compareEncl` b == EQ
    a @>=   b = a @> b || a `compareEncl` b == EQ

-- convenience Unicode math operator notation:
(⊂) :: (EnclosurePoset t) => t -> t -> Bool
(⊂) = (@<)
(⊆) :: (EnclosurePoset t) => t -> t -> Bool
(⊆) = (@<=)
(⊇) :: (EnclosurePoset t) => t -> t -> Bool
(⊇) = (@>=)
(⊃) :: (EnclosurePoset t) => t -> t -> Bool
(⊃) = (@>)


propExtremaForEnclosures :: (EnclosurePoset t, HasExtrema t) => t -> Bool
propExtremaForEnclosures e =
    (bottom ⊆ e) && (e ⊆ top)

-- TODO: adapt all poset properties for enclosures

{-|
    A set-based disjoint-intersection partial semi-lattice 
    behaving similarly to a basis of a domain.
    
    Intersection should be idempotent
    as well as commutative and associative where defined.  
-}
class (Eq t) => EnclosureBasis t where
    disjoint :: t -> t -> Bool
    maybeIntersection :: t -> t -> Maybe t

-- TODO: add symmetry for disjointness

-- convenient notation:
(@/\?) :: (EnclosureBasis t) => t -> t -> Maybe t
(@/\?) = maybeIntersection

(∩?) :: (EnclosureBasis t) => t -> t -> Maybe t
(∩?) = maybeIntersection

{-| two elements are disjoint iff they have no intersection -}
propEnclosureNonDisjointIntersection :: (EnclosureBasis t) => t -> t -> Bool
propEnclosureNonDisjointIntersection e1 e2 =
    ((e1 `disjoint` e2) || (isJust $ e1 ∩? e2))
    &&
    ((not (e1 `disjoint` e2)) || (not $ isJust $ e1 ∩? e2))

-- TODO: add idempotency, partialCommutativity and partialAssociativity

{-|
    A set-based lattice.  Union and intersection should be compatible with inclusion.
    Both operations should be idempotent, commutative and associative.
-}
class (Eq t) => EnclosureLattice t where
    union :: t -> t -> t
    intersection :: t -> t -> t

-- convenient notation:
(@\/) :: (EnclosureLattice t) => t -> t -> t
(@\/) = union

(∪) :: (EnclosureLattice t) => t -> t -> t
(∪) = union

(@/\) :: (EnclosureLattice t) => t -> t -> t
(@/\) = intersection

(∩) :: (EnclosureLattice t) => t -> t -> t
(∩) = intersection


-- TODO: adapt all lattice properties for enclosure lattices

-- TODO: add properties linking a basis with a lattice

{-|
    A lattice that supports in-place operations.
-}
class (EnclosureLattice t, CanBeMutable t) => EnclosureLatticeMutable t where
    {-| unionMutable a b c means a := b ∪ c; a can be the same as b and/or c -}
    unionMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| intersectionMutable a b c means a := b ∩ c; a can be the same as b and/or c -}
    intersectionMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

    -- TODO: add default implementations using read/write
    