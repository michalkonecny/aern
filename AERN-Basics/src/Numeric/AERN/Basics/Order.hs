{-|
    Module      :  Numeric.AERN.Basics.Order
    Description :  classical posets and lattices (<,\/,/\)  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Order 

where

import Numeric.AERN.Basics.Laws
import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.Mutable

import qualified Prelude 
import Prelude hiding (compare, EQ, LT, GT, (<), (<=), (>=), (>))
import Control.Monad.ST (ST)
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
    
toPartialOrdering :: Ordering -> PartialOrdering
toPartialOrdering Prelude.EQ = EQ 
toPartialOrdering Prelude.LT = LT 
toPartialOrdering Prelude.GT = GT 

{-| flip an ordering relation -}
partialOrderingTranspose :: PartialOrdering -> PartialOrdering
partialOrderingTranspose LT = GT
partialOrderingTranspose GT = LT
partialOrderingTranspose a = a

instance Poset Int where
    compare a b = toPartialOrdering $ Prelude.compare a b  

instance Poset Double where
    compare a b =
        case (isNaN a, isNaN b) of
           (False, False) -> toPartialOrdering $ Prelude.compare a b  
           (True, True) -> EQ
           _ -> NC 

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

{-| the usual infix notation for lattice binary join -}
(∨) :: (Lattice t) => t -> t -> t
(∨) = join

{-| the usual infix notation for lattice binary meet -}
(/\) :: (Lattice t) => t -> t -> t
(/\) = meet

{-| the usual infix notation for lattice binary meet -}
(∧) :: (Lattice t) => t -> t -> t
(∧) = meet

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
propLatticeModular :: (Lattice t) => t -> t -> t -> Bool
propLatticeModular = modular join meet

propLatticeDistributive :: (Lattice t) => t -> t -> t -> Bool
propLatticeDistributive e1 e2 e3 = 
        (leftDistributive join meet e1 e2 e3)
        && 
        (leftDistributive meet join e1 e2 e3)

{-|
    A lattice that supports in-place operations.
-}
class (Lattice t, CanBeMutable t) => LatticeMutable t where
    {-| joinMutable a b c means a := b \/ c; a can be the same as b and/or c -}
    joinMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
    {-| meetMutable a b c means a := b /\ c; a can be the same as b and/or c -}
    meetMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

    -- TODO: add default implementations using read/write

{-|
    A type with extrema.
-}
class HasExtrema t where
    top :: t
    bottom :: t

propExtremaInPoset :: (Poset t, HasExtrema t) => t -> Bool
propExtremaInPoset e =
    (bottom <= e) && (e <= top)
    

    