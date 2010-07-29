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

--instance NumOrd.Comparison Double where
--    compare a b =
--        case (isNaN a, isNaN b) of
--           (False, False) -> toPartialOrdering $ Prelude.compare a b  
--           _ -> throw (AERNException $ "illegal Double argument: NumOrd.Comparison.compare " 
--                        ++ show a ++ " " ++ show b) 

instance NumOrd.RoundedLattice Double where
    type NumOrd.MinmaxEffortIndicator Double = ()
    minmaxDefaultEffort _ = ()
    maxUpEff _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Prelude.max a b  
           _ -> throw (AERNException $ "illegal Double argument: NumOrd.Lattice.max " 
                        ++ show a ++ " " ++ show b)
    maxDnEff = NumOrd.maxUpEff 
    minUpEff _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Prelude.min a b  
           _ -> throw (AERNException $ "illegal Double argument: NumOrd.Lattice.min " 
                        ++ show a ++ " " ++ show b) 
    minDnEff = NumOrd.minUpEff 
    
--    -- a version with artificially added rounding for "testing" the tests
--    maxUpEff [effort] e1 e2 = NumOrd.max e1 e2 + (1/(convert effort))
--    maxDnEff [effort] e1 e2 = NumOrd.max e1 e2 - (1/(convert effort))
--    minUpEff [effort] e1 e2 = NumOrd.min e1 e2 + (1/(convert effort))
--    minDnEff [effort] e1 e2 = NumOrd.min e1 e2 - (1/(convert effort))
--    minmaxDefaultEffort _ = [10]



instance NumOrd.ArbitraryOrderedTuple Double where
   arbitraryTupleRelatedBy = 
       NumOrd.linearArbitraryTupleRelatedBy (incrSize $ choose (-300,300))
       -- When generating Double numbers for testing, try to avoid overflows
       -- as we cannot usually overcome overflows when we cannot increase 
       -- the granularity (aka precision) of the floating point type.
       -- Exp overflows at around 700.
       

