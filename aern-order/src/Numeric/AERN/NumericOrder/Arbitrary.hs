{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.Arbitrary
    Description :  random generation of tuples with various relation constraints  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Random generation of tuples with various relation constraints.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}
module Numeric.AERN.NumericOrder.Arbitrary where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.Misc.Debug

import Data.Maybe
import Data.Ratio
import qualified Data.Map as Map 
import qualified Data.Set as Set 


import Test.QuickCheck
import System.Random
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Numeric.AERN.Misc.QuickCheck

import Numeric.AERN.Misc.List
import qualified Data.List as List
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
    {-| a type of meaningful constraints to place on generation of arbitrary values -}
    type Area t
    {-| a special area that puts no constaints on the values -}
    areaWhole :: t -> Area t
    {-| generator of tuples that satisfy the given relation requirements
        and area restriction, 
        nothing if in this structure there are no tuples satisfying these requirements -}
    arbitraryTupleInAreaRelatedBy ::
        (Ord ix, Show ix) =>
        (Area t) -> 
        [ix] {-^ how many elements should be generated and with what names -} -> 
        [((ix, ix),[PartialOrdering])]
           {-^ required orderings for some pairs of elements -} -> 
        Maybe (Gen [t]) {-^ generator for tuples if the requirements make sense -}   
    {-| generator of tuples that satisfy the given relation requirements, 
        nothing if in this structure there are no tuples satisfying these requirements -}
    arbitraryTupleRelatedBy ::
        (Ord ix, Show ix) => 
        [ix] {-^ how many elements should be generated and with what names -} -> 
        [((ix, ix),[PartialOrdering])]
           {-^ required orderings for some pairs of elements -} -> 
        Maybe (Gen [t]) {-^ generator for tuples if the requirements make sense -}   
    arbitraryTuple ::   
        Int {-^ how many elements should be generated -} -> 
        Maybe (Gen [t]) {-^ generator for tuples if the requirements make sense -}
    arbitraryTuple n = arbitraryTupleRelatedBy [1..n] [] 


{--  specific instances of ArbitraryOrderedTuple: --}

data AreaWholeOnly t =
    AreaWholeOnly 
    {
        areaWholeSpecialValues :: [t]
    }

arbitraryWhole ::
    (Arbitrary t) =>
    AreaWholeOnly t ->
    Gen t
arbitraryWhole (AreaWholeOnly []) = incrSize arbitrary
arbitraryWhole (AreaWholeOnly specialValues) =
    incrSize $ -- at size 0 we get only 0s...
    do
    useSpecial <- elements [False, True, False, False] 
                        -- 1 in 4 values should be special
    case useSpecial of
        True -> elements specialValues
        False -> arbitrary

data AreaLinear t = 
    AreaLinear
    {
        areaLinLowerBound :: Maybe t,
        areaLinLowerBoundStrict :: Bool,
        areaLinUpperBound :: Maybe t,
        areaLinUpperBoundStrict :: Bool,
        areaLinSpecialValues :: [t]
    } 

areaLinearWhole :: [t] -> AreaLinear t
areaLinearWhole = AreaLinear Nothing True Nothing True 

arbitraryLinear ::
    (Arbitrary t) =>
    (t,t) {-^ least and greatest element -} -> 
    (t -> t) {-^ successor function -} ->
    (t -> t) {-^ predecessor function -} ->
    ((t,t) -> Gen t) {-^ choose function -} -> 
    AreaLinear t ->
    Gen t
arbitraryLinear (least, greatest) succ pred choose 
        (AreaLinear mlb lbStrict mub ubStrict specialValues) =
    incrSize $ -- at size 0 we get only 0s...
    do
    useSpecial <-
        case specialValues of
            [] -> return False
            _ -> elements [False, True, False, False] 
                 -- 1 in 4 values should be special
    case useSpecial of
        True -> elements specialValues
        False -> 
            case (mlb, mub) of 
                (Nothing, Nothing) -> arbitrary
                _ -> choose (lb, ub) 
    where
    lb = 
        case (mlb, lbStrict) of
            (Nothing, _) -> least
            (Just lb, True) -> lb
            (Just lb, False) -> succ lb
    ub = 
        case (mub, ubStrict) of
            (Nothing, _) -> greatest
            (Just ub, True) -> ub
            (Just ub, False) -> pred ub


instance ArbitraryOrderedTuple Int where
    type (Area Int) = AreaWholeOnly Int
    areaWhole _ = AreaWholeOnly [-1,0,1]
    arbitraryTupleInAreaRelatedBy area = 
        linearArbitraryTupleRelatedBy $ arbitraryWhole area
    arbitraryTupleRelatedBy =
        linearArbitraryTupleRelatedBy $ incrSize arbitrary

instance ArbitraryOrderedTuple Integer where
    type (Area Integer) = AreaWholeOnly Integer
    areaWhole _ = AreaWholeOnly [-1,0,1]
    arbitraryTupleInAreaRelatedBy area = 
        linearArbitraryTupleRelatedBy $ arbitraryWhole area
    arbitraryTupleRelatedBy =
        linearArbitraryTupleRelatedBy $ incrSize arbitrary

instance ArbitraryOrderedTuple Rational where
    type (Area Rational) = (AreaLinear Int, AreaLinear Int)
    areaWhole _ = (areaLinearWhole [-1,0,1], areaLinearWhole [0])
    arbitraryTupleInAreaRelatedBy (numeratorArea, preDenominatorArea) = 
        linearArbitraryTupleRelatedBy chooseRational
        where
        chooseRational = 
            do
            num <- arbitraryIntInArea numeratorArea
            preDenom <- arbitraryIntInArea preDenominatorArea
            return $ (toInteger num) % (1 + (abs $ toInteger preDenom))
        arbitraryIntInArea = arbitraryLinear (minInt, maxInt) succ pred choose  
        maxInt = maxBound
        minInt = minBound
    arbitraryTupleRelatedBy =
        linearArbitraryTupleRelatedBy $ incrSize arbitrary

--data AreaDouble =
--    AreaDouble
--    {
--        areaDblExp :: AreaLinear Int,
--        areaDblEncourageOne :: Bool,
--        areaDblAllowPos :: Bool,
--        areaDblAllowNeg :: Bool
--    }
    
areaDoubleSmall :: AreaLinear Double
areaDoubleSmall =
    AreaLinear (Just $ -256) False (Just 256) False [0,-1,1]

instance ArbitraryOrderedTuple Double where
    type (Area Double) = AreaLinear Double
    areaWhole _ = areaLinearWhole [-1/0,-1,0,1,1/0]
    arbitraryTupleInAreaRelatedBy area = 
        linearArbitraryTupleRelatedBy 
                (arbitraryLinear (-maxDbl, maxDbl) id id chooseDbl area)
        where
        maxDbl = encodeFloat 1 (maxExp - 1)
        chooseDbl (lb, ub) =
            do
            exp <- chooseNearerZero (expLb, expUb)
            case exp == expUb of
                True -> choose (lb, ub)
                False -> chooseWithExp exp
            where
            chooseNearerZero (lo, hi) =
--                unsafePrint ("chooseNearerZero: "
--                    ++ "\n loDT = " ++ show loDT
--                    ++ "\n hiDT = " ++ show hiDT
--                ) $
                do
                eDT <- choose (loDT, hiDT)
                return $ round $ transform eDT 
                where
                loDT = transformInv loD
                hiDT = transformInv hiD
                transform :: Double -> Double
                transform x = x*x*x/1000000 -- 100^3 = 1000000
                transformInv x 
                    | x > 0 = 100 * exp ((log x)/3)
                    | x < 0 = - (transformInv (-x))
                    | otherwise = 0
                loD = fromInteger $ toInteger lo
                hiD = fromInteger $ toInteger hi
            expLb = 
                case (lb <= 0 && ub >= 0) of
                    True -> minExp
                    False -> min lbExp ubExp
            expUb = max lbExp ubExp
            lbExp = case lb == 0 of True -> minExp; False -> exponent lb
            ubExp = case ub == 0 of True -> minExp; False -> exponent ub
            chooseWithExp exp =
                do
                signif <- choose (0.5,2)
                let validCandidates = deriveValidCandidates $ signif * (encodeFloat 1 exp)
                case validCandidates of
                    [] -> chooseWithExp exp
                    _ -> elements validCandidates
            deriveValidCandidates d =
                filter valid [d,-d]
                where
                valid d = lb <= d && d <= ub
        (minExp, maxExp) = floatRange (0 :: Double)
    arbitraryTupleRelatedBy =
        arbitraryTupleInAreaRelatedBy areaDoubleSmall
       -- When generating Double numbers for testing, try to avoid overflows
       -- as we cannot usually overcome overflows when we cannot increase 
       -- the granularity (aka precision) of the floating point type.
       -- Exp overflows at around 700.

--        where
--        AreaLinear mmin minStrict mmax maxStrict = bounds
--        min = 
--            case mmin of 
--                Nothing -> 
--                Just m -> 
--                    case expMinStrict of
--                        True -> m+1
--                        False -> m
--        expMax = 
--            case mexpMax of 
--                Nothing -> 480; -- encourage infinity 
--                Just m -> 
--                    case expMaxStrict of
--                        True -> m-1
--                        False -> m
--        arbitaryBoundedDouble =
--           do
--           d <- arbitrary
--           e <- case (expMin < 0 && expMax > 0) of
--               True -> 
--                   do 
--                   e1 <- choose (0,expMax)
--                   e2 <- choose (-380,-1)
--                   elements [e1,e2]
--               False -> choose (expMin, expMax)
--           s <- elements $ case (encourage [1,1,1,1,0]
--           a <- elements $ case (encourageZero, encourageOne) [-1,0,1]
--           return (buildDouble d e s a)
--        buildDouble d e s a =
--           (s * dE + a)
--           where
--           dE = encodeFloat m (e - 53)
--           (m,_) = decodeFloat (d :: Double)
--           

{-| Default implementation of linearArbitraryTupleRelatedBy for Ord instances -}   
linearArbitraryTupleRelatedBy ::
    (Ord ix, Show ix, Ord a) =>
    (Gen a) ->
    [ix] ->
    [((ix,ix),[PartialOrdering])] ->
    Maybe (Gen [a])
linearArbitraryTupleRelatedBy givenArbitrary indices constraints =   
       case consistentUnambiguousConstraints of
          [] -> Nothing
          _ -> Just $
              do
              unambiguousConstraints <- elements consistentUnambiguousConstraints
              let cMap = Map.fromList unambiguousConstraints
              let sortedIndices = List.sortBy (turnIntoOrdering cMap) indices
              let sortedIndicesGrouped = List.groupBy (turnIntoEquality cMap) sortedIndices
              ds <- vectorOf (3 * (length sortedIndicesGrouped)) givenArbitrary
              seed <- arbitrary
              let dsDistinctSorted = getNDistinctSorted seed (length sortedIndicesGrouped) ds
                                      -- here we rely on the following property:
                                      -- it is very unlikely to get less than n distinct
                                      -- elements among 3*n elements generated by givenArbitrary 
              return $ 
                  map snd $ 
                      List.sort $ 
                          concat $ 
                              zipWith zip sortedIndicesGrouped $ 
                                  map repeat dsDistinctSorted
       where
       consistentUnambiguousConstraints =
           pickConsistentOrderings permittedInLinearOrder indices constraints
       turnIntoOrdering cMap a b =
           case (Map.lookup (a,b) cMap, Map.lookup (b,a) cMap) of
               (Just pord, _) -> fromPartialOrdering pord
               (_, Just pord) -> fromPartialOrdering $ partialOrderingTranspose pord
       turnIntoEquality cMap a b =
           case (Map.lookup (a,b) cMap, Map.lookup (b,a) cMap) of
               (Just pord, _) -> pord == EQ
               (_, Just pord) -> pord == EQ

{-| Default implementation of linearArbitraryTupleRelatedBy for types in which
    sequences of values are first transformed into linearly ordered ones. 
     -}   
forcedLinearArbitraryTupleRelatedBy ::
    (Ord ix, Show ix) =>
    (Gen a) ->
    (Int -> Int -> [a] -> [a]) ->
    [ix] ->
    [((ix,ix),[PartialOrdering])] ->
    Maybe (Gen [a])
forcedLinearArbitraryTupleRelatedBy givenArbitrary sortNSquashFn indices constraints =   
       case consistentUnambiguousConstraints of
          [] -> Nothing
          _ -> Just $
              do
              unambiguousConstraints <- elements consistentUnambiguousConstraints
              let cMap = Map.fromList unambiguousConstraints
              let sortedIndices = List.sortBy (turnIntoOrdering cMap) indices
              let sortedIndicesGrouped = List.groupBy (turnIntoEquality cMap) sortedIndices
              ds <- vectorOf (3 * (length sortedIndicesGrouped)) givenArbitrary
              seed <- arbitrary
              let dsDistinctSorted = sortNSquashFn seed (length sortedIndicesGrouped) ds
                                      -- here we rely on the following property:
                                      -- it is very unlikely to get less than n distinct
                                      -- elements among 3*n elements generated by givenArbitrary 
              return $ 
                  map snd $ 
                      List.sortBy (\(a,_) -> \(b,_) -> compare a b) $ 
                          concat $ 
                              zipWith zip sortedIndicesGrouped $ 
                                  map repeat dsDistinctSorted
       where
       consistentUnambiguousConstraints =
           pickConsistentOrderings permittedInLinearOrder indices constraints
       turnIntoOrdering cMap a b =
           case (Map.lookup (a,b) cMap, Map.lookup (b,a) cMap) of
               (Just pord, _) -> fromPartialOrdering pord
               (_, Just pord) -> fromPartialOrdering $ partialOrderingTranspose pord
       turnIntoEquality cMap a b =
           case (Map.lookup (a,b) cMap, Map.lookup (b,a) cMap) of
               (Just pord, _) -> pord == EQ
               (_, Just pord) -> pord == EQ

arbitraryPairRelatedBy ::
    (ArbitraryOrderedTuple t) => 
    PartialOrdering -> 
    Maybe (Gen (t,t))
arbitraryPairRelatedBy rel =
    case arbitraryTupleRelatedBy [1,2] [((1,2),[rel])] of
        Nothing -> Nothing
        Just gen -> Just $
            do
            [e1,e2] <- gen 
            return (e1,e2)

arbitraryPairInAreaRelatedBy ::
    (ArbitraryOrderedTuple t) =>
    Area t -> 
    PartialOrdering -> 
    Maybe (Gen (t,t))
arbitraryPairInAreaRelatedBy area rel =
    case arbitraryTupleInAreaRelatedBy area [1,2] [((1,2),[rel])] of
        Nothing -> Nothing
        Just gen -> Just $
            do
            [e1,e2] <- gen 
            return (e1,e2)

arbitraryTripleRelatedBy ::
    (ArbitraryOrderedTuple t) => 
    (PartialOrdering, PartialOrdering, PartialOrdering) -> 
    Maybe (Gen (t,t,t))
arbitraryTripleRelatedBy (r1, r2, r3) =
    case arbitraryTupleRelatedBy [1,2,3] constraints of
        Nothing -> Nothing
        Just gen -> Just $
            do
            [e1,e2,e3] <- gen
            return (e1, e2, e3)
    where
    constraints = [((1,2),[r1]), ((2,3),[r2]), ((1,3),[r3])]

arbitraryTripleInAreaRelatedBy ::
    (ArbitraryOrderedTuple t) => 
    Area t ->
    (PartialOrdering, PartialOrdering, PartialOrdering) -> 
    Maybe (Gen (t,t,t))
arbitraryTripleInAreaRelatedBy area (r1, r2, r3) =
    case arbitraryTupleInAreaRelatedBy area [1,2,3] constraints of
        Nothing -> Nothing
        Just gen -> Just $
            do
            [e1,e2,e3] <- gen
            return (e1, e2, e3)
    where
    constraints = [((1,2),[r1]), ((2,3),[r2]), ((1,3),[r3])]

{-| generic mechanism for adding common restriction on the area of random generation -}
class AreaHasNonNegativeOption a where
    restrictAreaToNonNeg :: a -> a
class AreaHasNonZeroOption a where
    restrictAreaToNonZero :: a -> a

{-| type for randomly generating single elements using the distribution of the 'ArbitraryOrderedTuple' instance -}
newtype UniformlyOrderedSingleton t = UniformlyOrderedSingleton t deriving (Show)
{-| type for randomly generating pairs of unrelated elements using the distribution of the 'ArbitraryOrderedTuple' instance -}
data TwoUniformlyOrderedSingletons t = TwoUniformlyOrderedSingletons (t,t) deriving (Show)
{-| type for randomly generating triples of unrelated elements using the distribution of the 'ArbitraryOrderedTuple' instance -}
data ThreeUniformlyOrderedSingletons t = ThreeUniformlyOrderedSingletons (t,t,t) deriving (Show)

{-| type for generating pairs distributed in such a way that all ordering relations 
    permitted by this structure have similar probabilities of occurrence -}
data UniformlyOrderedPair t = UniformlyOrderedPair (t,t) deriving (Show)
data TwoUniformlyOrderedPairs t = TwoUniformlyOrderedPairs ((t,t),(t,t)) deriving (Show)
data ThreeUniformlyOrderedPairs t = ThreeUniformlyOrderedPairs ((t,t),(t,t),(t,t)) deriving (Show)

data LEPair t = LEPair (t,t) deriving (Show)

{-| type for generating triples distributed in such a way that all ordering relation combinations 
    permitted by this structure have similar probabilities of occurrence -}
data UniformlyOrderedTriple t = UniformlyOrderedTriple (t,t,t) deriving (Show)

instance 
    (ArbitraryOrderedTuple t) 
    => 
    Arbitrary (UniformlyOrderedSingleton t) 
    where
    arbitrary =
        do
        [elem] <- gen
        return $ UniformlyOrderedSingleton elem
        where
        Just gen = arbitraryTupleRelatedBy [1] []

instance
    (ArbitraryOrderedTuple t, a ~ Area t) 
    => 
    ArbitraryWithParam (TwoUniformlyOrderedSingletons t) a 
    where
    arbitraryWithParam area =
        do
        (UniformlyOrderedSingleton e1) <- arbitraryWithParam area
        (UniformlyOrderedSingleton e2) <- arbitraryWithParam area
        return $ TwoUniformlyOrderedSingletons (e1,e2)

instance
    (ArbitraryOrderedTuple t, a ~ Area t)
    => 
    ArbitraryWithParam (ThreeUniformlyOrderedSingletons t) a 
    where
    arbitraryWithParam area =
        do
        (UniformlyOrderedSingleton e1) <- arbitraryWithParam area
        (UniformlyOrderedSingleton e2) <- arbitraryWithParam area
        (UniformlyOrderedSingleton e3) <- arbitraryWithParam area
        return $ ThreeUniformlyOrderedSingletons (e1,e2,e3)

instance
    (ArbitraryOrderedTuple t, a ~ Area t) 
    => 
    ArbitraryWithParam (UniformlyOrderedSingleton t) a 
    where
    arbitraryWithParam area =
        do
        [elem] <- gen
        return $ UniformlyOrderedSingleton elem
        where
        Just gen = arbitraryTupleInAreaRelatedBy area [1] []

instance
    (ArbitraryOrderedTuple t) 
    => 
    Arbitrary (UniformlyOrderedPair t) 
    where
    arbitrary =
        do
        gen <- elements gens
        pair <- gen
        return $ UniformlyOrderedPair pair
        where
        gens = catMaybes $ map (arbitraryPairRelatedBy) partialOrderingVariants  

instance
    (ArbitraryOrderedTuple t, a ~ Area t) 
    => 
    ArbitraryWithParam (UniformlyOrderedPair t) a 
    where
    arbitraryWithParam area =
        do
        gen <- elements gens
        pair <- gen
        return $ UniformlyOrderedPair pair
        where
        gens = catMaybes $ map (arbitraryPairInAreaRelatedBy area) partialOrderingVariants  

instance
    (ArbitraryOrderedTuple t, a ~ Area t) 
    => 
    ArbitraryWithParam (TwoUniformlyOrderedPairs t) a 
    where
    arbitraryWithParam area =
        do
        (UniformlyOrderedPair p1) <- arbitraryWithParam area
        (UniformlyOrderedPair p2) <- arbitraryWithParam area
        return $ TwoUniformlyOrderedPairs (p1,p2)

instance
    (ArbitraryOrderedTuple t, a ~ Area t)
    => 
    ArbitraryWithParam (ThreeUniformlyOrderedPairs t) a 
    where
    arbitraryWithParam area =
        do
        (UniformlyOrderedPair p1) <- arbitraryWithParam area
        (UniformlyOrderedPair p2) <- arbitraryWithParam area
        (UniformlyOrderedPair p3) <- arbitraryWithParam area
        return $ ThreeUniformlyOrderedPairs (p1,p2,p3)

instance (ArbitraryOrderedTuple t) => Arbitrary (LEPair t) where
    arbitrary =
        do
        gen <- elements gens
        pair <- gen
        return $ LEPair pair
        where
        gens = catMaybes $ map arbitraryPairRelatedBy [LT, LT, LT, EQ]  

instance
    (ArbitraryOrderedTuple t, a ~ Area t) 
    => 
    ArbitraryWithParam (LEPair t) a
    where
    arbitraryWithParam area =
        do
        gen <- elements gens
        pair <- gen
        return $ LEPair pair
        where
        gens = catMaybes $ map (arbitraryPairInAreaRelatedBy area) [LT, LT, LT, EQ]  

instance (ArbitraryOrderedTuple t) => Arbitrary (UniformlyOrderedTriple t) where
    arbitrary = 
        do
        gen <- elements gens
        triple <- gen
        return $ UniformlyOrderedTriple triple
        where
        gens = catMaybes $ map arbitraryTripleRelatedBy partialOrderingVariantsTriples

instance
    (ArbitraryOrderedTuple t, a ~ Area t) 
    => 
    ArbitraryWithParam (UniformlyOrderedTriple t) a 
    where
    arbitraryWithParam area =
        do
        gen <- elements gens
        triple <- gen
        return $ UniformlyOrderedTriple triple
        where
        gens = catMaybes $ map (arbitraryTripleInAreaRelatedBy area) partialOrderingVariantsTriples  

propArbitraryOrderedPair ::
    (ArbitraryOrderedTuple t) =>
    (t -> t -> PartialOrdering) -> PartialOrdering -> Bool 
propArbitraryOrderedPair compare rel =
     case arbitraryPairRelatedBy rel of
        Nothing -> True
        Just gen ->
             and $ map relOK theSample 
             where
             theSample = unsafePerformIO $ sample' gen 
             relOK (e1, e2) = compare e1 e2 == rel

propArbitraryOrderedTriple ::
    (ArbitraryOrderedTuple t) =>
    (t -> t -> PartialOrdering) -> (PartialOrdering, PartialOrdering, PartialOrdering) -> Bool 
propArbitraryOrderedTriple compare rels@(r1,r2,r3) =
     case arbitraryTripleRelatedBy rels of
        Nothing -> True
        Just gen ->
             and $ map relOK theSample 
             where
             theSample = unsafePerformIO $ sample' $ gen
             relOK (e1, e2, e3) = 
                and [compare e1 e2 == r1, compare e2 e3 == r2, compare e1 e3 == r3]


testsArbitraryTuple ::
    (Arbitrary t,
     ArbitraryOrderedTuple t) =>
    (String, t, t -> t -> PartialOrdering) -> Test
testsArbitraryTuple (name, sample, compare)  =
    testGroup (name ++ " arbitrary ordered") $ 
        [
         testProperty "pairs" (propArbitraryOrderedPair compare)
        ,
         testProperty "triples" (propArbitraryOrderedTriple compare)
        ]
