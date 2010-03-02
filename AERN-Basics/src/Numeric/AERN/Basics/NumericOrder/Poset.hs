{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.Poset
    Description :  partially ordered sets using numeric order notation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    This module is hidden and reexported via its parent NumericOrder. 
-}

module Numeric.AERN.Basics.NumericOrder.Poset where

import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Extrema
import Numeric.AERN.Basics.Laws.Relation

import qualified Prelude 
import Prelude hiding (compare, EQ, LT, GT, (<), (<=), (>=), (>))
import Test.QuickCheck


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
    
propExtremaInPoset :: (Poset t, HasExtrema t) => t -> Bool
propExtremaInPoset e =
    (bottom <= e) && (e <= top)
    