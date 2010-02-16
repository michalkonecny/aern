{-|
    Module      :  Numeric.AERN.Order
    Description :  posets and lattices - classical and semi-decidable  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Order 

where

import Numeric.AERN.Laws
import Numeric.AERN.MaybeBool

import Prelude hiding (compare, EQ, LT, GT, (<), (<=), (>))
import Test.QuickCheck

propEqReflexive :: (Eq t) => t -> Bool
propEqReflexive = reflexive (==)

propEqSymmetric :: (Eq t) => t -> t -> Bool
propEqSymmetric = symmetric (==)

propEqTransitive :: (Eq t) => t -> t -> t -> Bool
propEqTransitive = transitive (==)

{-|
    A partially ordered set.
    
    (More-or-less copied from Data.Poset 
     in package altfloat-0.3 by Nick Bowler.) 
-} 
class (Eq t) => Poset t where
    compare :: t -> t -> PartialOrdering
    -- | Is comparable to.
    (<==>)  :: t -> t -> Bool
    -- | Is not comparable to.
    (</=>)  :: t -> t -> Bool
    (<)     :: t -> t -> Bool
    (<=)    :: t -> t -> Bool
    (>=)    :: t -> t -> Bool
    (>)     :: t -> t -> Bool

    -- defaults for all but compare:
    a <    b = a `compare` b == LT
    a >    b = a `compare` b == GT
    a <==> b = a `compare` b /= NC
    a </=> b = a `compare` b == NC
    a <=   b = a < b || a `compare` b == EQ
    a >=   b = a > b || a `compare` b == EQ

{-| Like 'Prelude.Ordering' but with a non-comparable option -}
data PartialOrdering = EQ | LT | GT | NC
    deriving (Eq)

{-| flip an ordering relation -}
partialOrderingTranspose :: PartialOrdering -> PartialOrdering
partialOrderingTranspose LT = GT
partialOrderingTranspose GT = LT
partialOrderingTranspose a = a

{-|
    Poset with the ability to randomly generate
    pairs of its own elements that are in 
    a specific order relation (eg LT or NC).
    
    This is to help with checking properties that
    make sense only for pairs in a certain relation
    where such pairs are rare.
-}
class PosetArbitraryRelatedPair t where
    {-| generator of pairs that satisfy the chosen relation -}
    arbitraryPosetRelatedPair :: PartialOrdering -> Gen (t,t)    
    {-| generator of pairs distributed in such a way that all ordering relations 
       permitted by this structure have similar probabilities of occurrence -}
    arbitraryPosetPair :: Gen (t,t)    

propPosetEqCompatible :: (Poset t) => t -> t -> Bool
propPosetEqCompatible e1 e2 =
    (e1 /= e2 || compare e1 e2 == EQ)
    &&    
    (e1 == e2 || compare e1 e2 /= EQ)    

propPosetAntiSymmetric :: (Poset t) => t -> t -> Bool
propPosetAntiSymmetric e1 e2 = 
    compare e2 e1 == (partialOrderingTranspose $ compare e1 e2) 

propPosetTransitive :: (Poset t) => t -> t -> t -> Bool
propPosetTransitive = transitive (<=)
    

{-|
    A lattice.  Join and meet should be compatible with some partial order.
    Both operations should be idempotent, commutative and associative.
-}
class (Eq t) => Lattice t where
    join :: t -> t -> t
    meet :: t -> t -> t

{-| the usual infix notation for lattice binary join -}
(\/) :: (Lattice t) => t -> t -> t
(\/) = join
{-| the usual infix notation for lattice binary meet -}
(/\) :: (Lattice t) => t -> t -> t
(/\) = meet

propLatticePosetCompatible :: (Poset t, Lattice t) => t -> t -> Bool
propLatticePosetCompatible e1 e2 =
    ((not (e1 <= e2)) || ((e1 \/ e2 == e2) && (e1 /\ e2 == e1)))
    &&
    (((e1 <= e2) && (e1 \/ e2 == e2)) || (not (e1 /\ e2 == e1)))
    &&
    (((e1 <= e2) && (e1 /\ e2 == e1)) || (not (e1 \/ e2 == e2)))

propLatticeJoinAboveBoth :: (Poset t, Lattice t) => t -> t -> Bool
propLatticeJoinAboveBoth e1 e2 =
    (e1 <= (e1 `join` e2))
    &&
    (e2 <= (e1 `join` e2))

propLatticeMeetBelowBoth :: (Poset t, Lattice t) => t -> t -> Bool
propLatticeMeetBelowBoth e1 e2 =
    ((e1 `meet` e2) <= e1)
    &&
    ((e1 `meet` e2) <= e2)

propLatticeJoinIdempotent :: (Lattice t) => t -> Bool
propLatticeJoinIdempotent = idempotent join

propLatticeJoinCommutative :: (Lattice t) => t -> t -> Bool
propLatticeJoinCommutative = commutative join

propLatticeJoinAssocative :: (Lattice t) => t -> t -> t -> Bool
propLatticeJoinAssocative = associative join

propLatticeMeetIdempotent :: (Lattice t) => t -> Bool
propLatticeMeetIdempotent = idempotent meet

propLatticeMeetCommutative :: (Lattice t) => t -> t -> Bool
propLatticeMeetCommutative = commutative meet

propLatticeMeetAssocative :: (Lattice t) => t -> t -> t -> Bool
propLatticeMeetAssocative = associative meet

{- optional properties: -}
--propLatticeModular
--propLatticeDistributive

{-|
    A poset type with extrema.
-}
class Extrema t where
    top :: t
    bottom :: t

propExtremaInPoset :: (Poset t, Extrema t) => t -> Bool
propExtremaInPoset e =
    (bottom <= e) && (e <= top)
    

{-|
    A type with semi-decidable equality
-}
class SemidecidableEq t where
    maybeEqual :: t -> t -> Maybe Bool

(==?) :: (SemidecidableEq t) => t -> t -> Maybe Bool
(==?) = maybeEqual

propSemidecidableEqReflexive :: (SemidecidableEq t) => t -> Bool
propSemidecidableEqReflexive = semidecidableReflexive (==?)

propSemidecidableEqSymmetric :: (SemidecidableEq t) => t -> t -> Bool
propSemidecidableEqSymmetric = semidecidableSymmetric (==?)

propSemidecidableEqTransitive :: (SemidecidableEq t) => t -> t -> t -> Bool
propSemidecidableEqTransitive = semidecidableTransitive (==?)


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

propExtremaInSemidecidablePoset :: (SemidecidablePoset t, Extrema t) => t -> Bool
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
    
