{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.NumericOrder.Arbitrary.Linear
    Description :  instances for linearly ordered types   
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    'ArbitraryOrderedTuple' instances for linearly ordered types.
    
    This module is hidden and reexported via its grand-parent NumericOrder. 
-}
module Numeric.AERN.NumericOrder.Arbitrary.Linear where

import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.NumericOrder.Arbitrary

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

data AreaLinear t = 
    AreaLinear
    {
        areaLinLowerBound :: Maybe t,
        areaLinLowerBoundStrict :: Bool,
        areaLinUpperBound :: Maybe t,
        areaLinUpperBoundStrict :: Bool,
        areaLinForbiddenValues :: [t],
        areaLinIsValueForbidden :: t -> Bool,
        areaLinSpecialValues :: [t]
    } 

areaLinearWhole :: [t] -> AreaLinear t
areaLinearWhole specialValues = AreaLinear Nothing True Nothing True [] (const False) specialValues

linearAreaRestrictToNonNeg ::
    (Ord t) =>
    t {-^ the zero -} ->
    (AreaLinear t) ->
    (AreaLinear t)
linearAreaRestrictToNonNeg z area = 
    area
        {
            areaLinLowerBound = 
                fmap (max z) $ areaLinLowerBound area
            ,
            areaLinSpecialValues =
                filter (>= z) $ areaLinSpecialValues area
        }

linearAreaAddForbiddenValues ::
    (Ord t) =>
    [t] {-^ newly forbidden values -} ->
    (AreaLinear t) ->
    (AreaLinear t)
linearAreaAddForbiddenValues values area =
    area
        {
            areaLinIsValueForbidden = 
                \value ->
                    (value `elem` values)
                    ||
                    (areaLinIsValueForbidden area value)
            ,
            areaLinForbiddenValues =
                areaLinForbiddenValues area ++ values
        }

arbitraryLinear ::
    (Arbitrary t) =>
    (t,t) {-^ least and greatest element -} -> 
    (t -> t) {-^ successor function -} ->
    (t -> t) {-^ predecessor function -} ->
    ((t,t) -> Gen t) {-^ choose function -} -> 
    AreaLinear t ->
    Gen t
arbitraryLinear (least, greatest) succ pred choose 
        (AreaLinear mlb lbStrict mub ubStrict _ isForbidden specialValues) =
    incrSize $ -- at size 0 we get only 0s...
    do
    useSpecial <-
        case specialValues of
            [] -> return False
            _ -> elements [False, True, False, False] 
                 -- 1 in 4 values should be special
    case useSpecial of
        True -> elements specialValues
        False -> avoidForbidden
    where
    avoidForbidden =
        do
        result <-
            case (mlb, mub) of 
                (Nothing, Nothing) -> arbitrary
                _ -> choose (lb, ub)
        if isForbidden result
            then avoidForbidden 
            else return result
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

areaDoubleSmall :: AreaLinear Double
areaDoubleSmall =
    AreaLinear (Just $ -256) False (Just 256) False [] (const False) [0,-1,1]

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

instance (AreaHasNonNegativeOption Double)
    where
    areaRestrictToNonNeg _ =
        linearAreaRestrictToNonNeg 0

instance (AreaHasForbiddenValues Double)
    where
    areaGetForbiddenValues = areaLinForbiddenValues
    areaAddForbiddenValues = linearAreaAddForbiddenValues

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

