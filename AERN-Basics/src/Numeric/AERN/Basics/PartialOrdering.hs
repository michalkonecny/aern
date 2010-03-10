{-|
    Module      :  Numeric.AERN.Basics.PartialOrdering
    Description :  extension of Prelude.Ordering with non-comparable variant  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Extension of 'Prelude.Ordering' with non-comparable variant.
-}
module Numeric.AERN.Basics.PartialOrdering where

import qualified Prelude
import Prelude hiding (EQ, LT, GT)
import Data.Maybe
import Test.QuickCheck
import System.IO.Unsafe


{-| Like 'Prelude.Ordering' but with a non-comparable option -}
data PartialOrdering = EQ | LT | GT | NC
    deriving (Eq, Show, Enum, Bounded)
    
instance Arbitrary PartialOrdering
    where
    arbitrary = elements partialOrderingVariants
    
partialOrderingVariants = [EQ, LT, GT, NC]
    
toPartialOrdering :: Ordering -> PartialOrdering
toPartialOrdering Prelude.EQ = EQ 
toPartialOrdering Prelude.LT = LT 
toPartialOrdering Prelude.GT = GT 

{-| flip an ordering relation -}
partialOrderingTranspose :: PartialOrdering -> PartialOrdering
partialOrderingTranspose LT = GT
partialOrderingTranspose GT = LT
partialOrderingTranspose a = a

{-|
    Poset with the ability to randomly generate
    pairs and triples of its own elements that are in 
    a specific order relation (eg LT or NC).
    
    This is to help with checking properties that
    make sense only for pairs in a certain relation
    where such pairs are rare.
-}
class ArbitraryOrderedTuple t where
    {-| generator of pairs that satisfy the chosen relation, 
        nothing if there are no pairs satisfying this relation in the structure -}
    arbitraryPairRelatedBy :: PartialOrdering -> Maybe (Gen (t,t))    
    {-| generator of pairs distributed in such a way that all ordering relations 
       permitted by this structure have similar probabilities of occurrence -}
    arbitraryPairUniformRelation :: Gen (t,t)
    arbitraryPairUniformRelation =
        do
        gen <- elements gens
        gen
        where
        gens = catMaybes $ map arbitraryPairRelatedBy partialOrderingVariants  
    {-| generator of triples that satisfy the chosen relations: @e1 r1 e2, e2 r2 e3, e1 r3 e3@,
        nothing if there are no pairs satisfying this relation in the structure -}
    arbitraryTripleRelatedBy :: (PartialOrdering, PartialOrdering, PartialOrdering) -> Maybe (Gen (t,t,t))    
    {-| generator of triples distributed in such a way that all ordering relation combinations 
       permitted by this structure have similar probabilities of occurrence -}
    arbitraryTripleUniformRelation :: Gen (t,t,t)
    arbitraryTripleUniformRelation =
        do
        gen <- elements gens
        gen
        where
        gens = catMaybes $ map arbitraryTripleRelatedBy partialOrderingVariantsTriples

{-|
    All 29 triples of poset orderings @(r1, r2, r3)@ for which
    there could be elements satisfying
    @e1 `r1` e2 && e2 `r2` e3 && e1 `r3` e3@
    (ie not breaking transitivity).
-}
partialOrderingVariantsTriples :: [(PartialOrdering, PartialOrdering, PartialOrdering)]
partialOrderingVariantsTriples =
    [(r1,r2,r3)| 
        r1 <- partialOrderingVariants, 
        r2 <- partialOrderingVariants, 
        r3 <- partialOrderingVariants,
        respectsTransitivity (r1, r2, r3)]

{-|
    Are there any elements satisfying
    @e1 `r1` e2 && e2 `r2` e3 && e1 `r3` e3@
    assuming equality and order are transitive.
-}
respectsTransitivity :: 
    (PartialOrdering, PartialOrdering, PartialOrdering) {-^ @(r1,r2,r3)@ -} -> 
    Bool 
respectsTransitivity rels =
    case rels of
        -- when a pair is equal:
        (EQ,r2,r3) -> r2 == r3 -- e1 = e2
        (r1,EQ,r3) -> r1 == r3 -- e2 = e3
        (r1,r2,EQ) -> r1 == partialOrderingTranspose r2 -- e1 = e3
        -- 6 permutations of strict inequalities:
        (LT,LT,LT) -> True -- e1 < e2 < e3 (1)
        (LT,LT,_ ) -> False -- but not e1 < e3
        (LT,GT,LT) -> True -- e1 < e3 < e2 (2)
        (_ ,GT,LT) -> False -- but not e1 < e2
        (GT,LT,LT) -> True -- e2 < e1 < e3 (3)
        (GT,_ ,LT) -> False -- but not e2 < e3
        (GT,LT,GT) -> True -- e2 < e3 < e1 (4)
        (_ ,LT,GT) -> False -- but not e2 < e1
        (LT,GT,GT) -> True -- e3 < e1 < e2 (5)
        (LT,_ ,GT) -> False -- but not e3 < e2
        (GT,GT,GT) -> True -- e3 < e2 < e1 (6)
        (GT,GT,_ ) -> False -- but not e3 < e1
        --
        _ -> True -- all else is OK

propArbitraryOrderedPair compare rel =
     case arbitraryPairRelatedBy rel of
        Nothing -> True
        Just gen ->
             and $ map relOK theSample 
             where
             theSample = unsafePerformIO $ sample' gen 
             relOK (e1, e2) = compare e1 e2 == rel

propArbitraryOrderedTriple compare rels@(r1,r2,r3) =
     case arbitraryTripleRelatedBy rels of
        Nothing -> True
        Just gen ->
             and $ map relOK theSample 
             where
             theSample = unsafePerformIO $ sample' $ gen
             relOK (e1, e2, e3) = 
                and [compare e1 e2 == r1, compare e2 e3 == r2, compare e1 e3 == r3]

