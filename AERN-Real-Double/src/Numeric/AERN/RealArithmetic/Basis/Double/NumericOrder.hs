{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder
    Description :  numeric order instances for Double  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Numeric order Comparison and lattice instances for Double.
    
    This is a private module reexported publicly via its parent.
-}
module Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder
where

import Prelude hiding (EQ,LT,GT)

import Numeric.AERN.Basics.Exception
import Control.Exception

import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Numeric.AERN.Misc.List
import Numeric.AERN.Misc.Debug

import Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map
--import qualified Data.Set as Set

import Data.Convertible

sampleD :: Double
sampleD = 0

nanD :: Double
nanD = 0/0

instance NumOrd.HasLeast Double where
    least = - 1/0

instance NumOrd.HasHighest Double where
    highest = 1/0

instance NumOrd.HasExtrema Double where

instance NumOrd.PartialComparison Double where
    type NumOrd.PartialCompareEffortIndicator Double = ()
    pCompareEff _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Just $ toPartialOrdering $ Prelude.compare a b  
           (True, True) -> Just EQ
           _ -> Just NC 
    pCompareDefaultEffort _ = ()

instance NumOrd.Comparison Double where
    compare a b =
        case (isNaN a, isNaN b) of
           (False, False) -> toPartialOrdering $ Prelude.compare a b  
           _ -> throw (AERNException $ "illegal Double argument: NumOrd.Comparison.compare " 
                        ++ show a ++ " " ++ show b) 

instance NumOrd.Lattice Double where
    max a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Prelude.max a b  
           _ -> throw (AERNException $ "illegal Double argument: NumOrd.Lattice.max " 
                        ++ show a ++ " " ++ show b) 
    min a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Prelude.min a b  
           _ -> throw (AERNException $ "illegal Double argument: NumOrd.Lattice.min " 
                        ++ show a ++ " " ++ show b) 
    
instance NumOrd.RoundedLattice Double where
    type NumOrd.MinmaxEffortIndicator Double = ()
    maxUpEff _ = NumOrd.max
    maxDnEff _ = NumOrd.max
    minUpEff _ = NumOrd.min
    minDnEff _ = NumOrd.min
    minmaxDefaultEffort _ = ()
--    -- a version with artificially added rounding for "testing" the tests
--    maxUpEff [effort] e1 e2 = NumOrd.max e1 e2 + (1/(convert effort))
--    maxDnEff [effort] e1 e2 = NumOrd.max e1 e2 - (1/(convert effort))
--    minUpEff [effort] e1 e2 = NumOrd.min e1 e2 + (1/(convert effort))
--    minDnEff [effort] e1 e2 = NumOrd.min e1 e2 - (1/(convert effort))
--    minmaxDefaultEffort _ = [10]



instance NumOrd.ArbitraryOrderedTuple Double where
   arbitraryTupleRelatedBy indices constraints =
       case consistentUnambiguousConstraints of
          [] -> Nothing
          _ -> Just $
              do
              unambiguousConstraints <- elements consistentUnambiguousConstraints
              let cMap = Map.fromList unambiguousConstraints
              let sortedIndices = List.sortBy (turnIntoOrdering cMap) indices
              let sortedIndicesGrouped = List.groupBy (turnIntoEquality cMap) sortedIndices
              ds <- vectorOf (length sortedIndicesGrouped) arbitrary
              return $ 
                  map snd $ 
                      List.sort $ 
                          concat $ 
                              zipWith zip sortedIndicesGrouped $ 
                                  map repeat $ 
                                      List.sort ds 
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
       

--    {--------------- pairs --------------}
--    arbitraryPairRelatedBy EQ = Just $
--        do
--        d <- arbitrary
--        return (d,d)
--    arbitraryPairRelatedBy LT = Just gen
--        where
--        gen =
--            do
--            (d1u, d2u) <- incrSize arbitrary
--            let [d1,d2] = sort [d1u,d2u]
--            if d1 < d2
--                then return (d1,d2)
--                else  gen -- bad luck, try again
--    arbitraryPairRelatedBy GT = Just $
--        do
--        (d1,d2) <- fromJust $ arbitraryPairRelatedBy LT
--        return (d2,d1)
--    arbitraryPairRelatedBy NC = Nothing
----        Just gen
----        where
----        gen =
----            do
----            d <- arbitrary
----            case isNaN d of
----               True -> gen -- bad luck, try again
----               _ -> elements [(nan,d), (d,nan)]
----        nan = 0/0
--    {--------------- no NC allowed in triples ----------------}
--    arbitraryTripleRelatedBy (NC,_ ,_ ) = Nothing
--    arbitraryTripleRelatedBy (_ ,NC,_ ) = Nothing
--    arbitraryTripleRelatedBy (_ ,_ ,NC) = Nothing
--    {--------------- triples with some equality --------------}
--    arbitraryTripleRelatedBy (EQ, r2, r3) | r2 == r3 = Just $ -- e1 = e2
--        do
--        (d2,d3) <- fromJust $ arbitraryPairRelatedBy r2
--        return (d2,d2,d3)
--    arbitraryTripleRelatedBy (r1, EQ, r3) | r1 == r3 = Just $
--        do
--        (d1,d3) <- fromJust $ arbitraryPairRelatedBy r3
--        return (d1,d3,d3)
--    arbitraryTripleRelatedBy (r1, r2, EQ) | r1 == partialOrderingTranspose r2 = Just $
--        do
--        (d1,d2) <- fromJust $ arbitraryPairRelatedBy r1
--        return (d1,d2,d1)
----    {--------------- triples with some NaN but no equality -------}
----    arbitraryTripleRelatedBy (NC, NC, NC) = Nothing -- Double consists of 2 linearly ordered components
----    arbitraryTripleRelatedBy (NC, NC, r3) = Just $ -- e2 = NaN
----        do
----        (d1,d3) <- fromJust $ arbitraryPairRelatedBy r3
----        return (d1,0/0,d3)
----    arbitraryTripleRelatedBy (NC, r2, NC) = Just $ -- e1 = NaN
----        do
----        (d2,d3) <- fromJust $ arbitraryPairRelatedBy r2
----        return (0/0,d2,d3)
----    arbitraryTripleRelatedBy (r1, NC, NC) = Just $ -- e3 = NaN
----        do
----        (d1,d2) <- fromJust $ arbitraryPairRelatedBy r1
----        return (d1,d2,0/0)
----    -- cannot have single NC because it forces NaN which forces two NCs 
----    -- (except equality which was sorted out earlier)
--    {--------------- strictly ordered triples ----------}
--    arbitraryTripleRelatedBy (LT, LT, LT) = Just gen
--        where
--        gen =
--            do
--            (d1u, d2u, d3u) <- incrSize arbitrary
--            let [d1,d2,d3] = sort [d1u,d2u,d3u] 
--            if d1 < d2 && d2 < d3 
--                then return (d1,d2,d3)
--                else gen -- bad luck, try again
--    arbitraryTripleRelatedBy (LT, GT, LT) = Just $
--        do
--        (d1,d2,d3) <- fromJust $ arbitraryTripleRelatedBy (LT, LT, LT)
--        return (d1,d3,d2)
--    arbitraryTripleRelatedBy (GT, LT, LT) = Just $
--        do
--        (d1,d2,d3) <- fromJust $ arbitraryTripleRelatedBy (LT, LT, LT)
--        return (d2,d1,d3)
--    arbitraryTripleRelatedBy (GT, LT, GT) = Just $
--        do
--        (d1,d2,d3) <- fromJust $ arbitraryTripleRelatedBy (LT, LT, LT)
--        return (d3,d1,d2)
--    arbitraryTripleRelatedBy (LT, GT, GT) = Just $
--        do
--        (d1,d2,d3) <- fromJust $ arbitraryTripleRelatedBy (LT, LT, LT)
--        return (d2,d3,d1)
--    arbitraryTripleRelatedBy (GT, GT, GT) = Just $
--        do
--        (d1,d2,d3) <- fromJust $ arbitraryTripleRelatedBy (LT, LT, LT)
--        return (d3,d2,d1)
--
