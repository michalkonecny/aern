{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.Arbitrary
    Description :  random generation of tuples with various relation constraints  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Random generation of tuples with various relation constraints.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}
module Numeric.AERN.Basics.NumericOrder.Arbitrary where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.PartialOrdering

import Data.Maybe
import qualified Data.Map as Map 
import qualified Data.Set as Set 

import Test.QuickCheck

import System.IO.Unsafe

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

