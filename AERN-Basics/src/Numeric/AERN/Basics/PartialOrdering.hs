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

import qualified Data.Map as Map 
import qualified Data.Set as Set 

{-| Like 'Prelude.Ordering' but with a non-comparable option
      + LE does not imply LT or EQ, thus has to be separately available 
 -}
data PartialOrdering = EQ | LT | LE | GT | GE | NC
    deriving (Eq, Show, Enum, Bounded)
    
instance Arbitrary PartialOrdering
    where
    arbitrary = elements partialOrderingVariants
    
partialOrderingVariants = [minBound..maxBound]
    
toPartialOrdering :: Ordering -> PartialOrdering
toPartialOrdering Prelude.EQ = EQ 
toPartialOrdering Prelude.LT = LT 
toPartialOrdering Prelude.GT = GT 

{-| flip an ordering relation -}
partialOrderingTranspose :: PartialOrdering -> PartialOrdering
partialOrderingTranspose LT = GT
partialOrderingTranspose LE = GE
partialOrderingTranspose GT = LT
partialOrderingTranspose GE = LE
partialOrderingTranspose a = a


{-|
    Comparison with the ability to randomly generate
    pairs and triples of its own elements that are in 
    a specific order relation (eg LT or NC).
    
    This is to help with checking properties that
    make sense only for pairs in a certain relation
    where such pairs are rare.
-}
class ArbitraryOrderedTuple t where
    {-| generator of tuples that satisfy the given relation requirements, 
        nothing if in this structure there are no tuples satisfying these requirements -}
    arbitraryTupleRelatedBy ::
        (Ord ix) => 
        Set.Set ix {-^ how many elements should be generated and with what names -} -> 
        Map.Map (ix, ix) [PartialOrdering]
           {-^ required orderings for some elements -} -> 
        Maybe (Gen (Map.Map ix t)) {-^ generator for the indexed element tuples if the requirements make sense -}   

arbitraryPairRelatedBy ::
    (ArbitraryOrderedTuple t) => PartialOrdering -> Maybe (Gen (t,t))
arbitraryPairRelatedBy rel =
    case arbitraryTupleRelatedBy set12 constraints of
        Nothing -> Nothing
        Just gen -> Just $
            do
            tupleMap <- gen 
            return (lk tupleMap 1, lk tupleMap 2)
    where
    set12 = Set.fromList [1,2]
    constraints = Map.fromList [((1,2),[rel])]
    lk tupleMap ix = 
        Map.findWithDefault (error "internal error in arbitraryPairRelatedBy") ix tupleMap 

arbitraryTripleRelatedBy ::
    (ArbitraryOrderedTuple t) => 
    (PartialOrdering, PartialOrdering, PartialOrdering) -> Maybe (Gen (t,t,t))
arbitraryTripleRelatedBy (r1, r2, r3) =
    case arbitraryTupleRelatedBy set123 constraints of
        Nothing -> Nothing
        Just gen -> Just $
            do
            tupleMap <- gen
            return (lk tupleMap 1, lk tupleMap 2, lk tupleMap 3)
    where
    set123 = Set.fromList [1,2,3]
    constraints = Map.fromList [((1,2),[r1]), ((2,3),[r2]), ((1,3),[r3])]
    lk tupleMap ix = 
        Map.findWithDefault (error "internal error in arbitraryTripleRelatedBy") ix tupleMap 

{-| type for generating pairs distributed in such a way that all ordering relations 
    permitted by this structure have similar probabilities of occurrence -}
data UniformlyOrderedPair t = UniformlyOrderedPair (t,t) deriving (Show)
data LTPair t = LTPair (t,t) deriving (Show)
data LEPair t = LEPair (t,t) deriving (Show)
data NCPair t = NCPair (t,t) deriving (Show)

{-| type for generating triples distributed in such a way that all ordering relation combinations 
    permitted by this structure have similar probabilities of occurrence -}
data UniformlyOrderedTriple t = UniformlyOrderedTriple (t,t,t) deriving (Show)
data LTLTLTTriple t = LTLTLTTriple (t,t,t) deriving (Show)
data LELELETriple t = LELELETriple (t,t,t) deriving (Show)
data NCLTLTTriple t = NCLTLTTriple (t,t,t) deriving (Show)
data NCGTGTTriple t = NCGTGTTriple (t,t,t) deriving (Show)
data NCLTNCTriple t = NCLTNCTriple (t,t,t) deriving (Show)

instance (ArbitraryOrderedTuple t) => Arbitrary (UniformlyOrderedPair t) where
    arbitrary =
        do
        gen <- elements gens
        pair <- gen
        return $ UniformlyOrderedPair pair
        where
        gens = catMaybes $ map arbitraryPairRelatedBy partialOrderingVariants  

instance (ArbitraryOrderedTuple t) => Arbitrary (LEPair t) where
    arbitrary =
        do
        gen <- elements gens
        pair <- gen
        return $ LEPair pair
        where
        gens = catMaybes $ map arbitraryPairRelatedBy [LT, LT, LT, EQ]  

instance (ArbitraryOrderedTuple t) => Arbitrary (LTPair t) where
    arbitrary =
        case arbitraryPairRelatedBy LT of
            Nothing -> error $ "LTPair used with an incompatible type"
            Just gen ->
                do
                pair <- gen
                return $ LTPair pair

instance (ArbitraryOrderedTuple t) => Arbitrary (NCPair t) where
    arbitrary =
        case arbitraryPairRelatedBy NC of
            Nothing -> error $ "NCPair used with an incompatible type"
            Just gen ->
                do
                pair <- gen
                return $ NCPair pair

instance (ArbitraryOrderedTuple t) => Arbitrary (UniformlyOrderedTriple t) where
    arbitrary = 
        do
        gen <- elements gens
        triple <- gen
        return $ UniformlyOrderedTriple triple
        where
        gens = catMaybes $ map arbitraryTripleRelatedBy partialOrderingVariantsTriples

instance (ArbitraryOrderedTuple t) => Arbitrary (LELELETriple t) where
    arbitrary =
        do
        gen <- elements gens
        triple <- gen
        return $ LELELETriple triple
        where
        gens = 
            catMaybes $ 
                map arbitraryTripleRelatedBy 
                    [(LT,LT,LT), (LT,LT,LT), (LT,LT,LT), (LT,LT,LT), (LT,LT,LT), 
                     (EQ,LT,LT), (EQ,LT,LT),
                     (LT,EQ,LT), (LT,EQ,LT),
                     (EQ,EQ,EQ)]  

instance (ArbitraryOrderedTuple t) => Arbitrary (LTLTLTTriple t) where
    arbitrary =
        case arbitraryTripleRelatedBy (LT, LT, LT) of
            Nothing -> error $ "LTLTLTTriple used with an incompatible type"
            Just gen ->
                do
                triple <- gen
                return $ LTLTLTTriple triple

{-|
    All 29 triples of Comparison orderings @(r1, r2, r3)@ for which
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

