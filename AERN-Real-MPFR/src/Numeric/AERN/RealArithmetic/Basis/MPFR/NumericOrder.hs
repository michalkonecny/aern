{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder
    Description :  numeric order instances for MPFR  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Numeric order Comparison and lattice instances for MPFR.
    
    This is a private module reexported publicly via its parent.
-}
module Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder
where

import Prelude hiding (EQ,LT,GT)

import Numeric.AERN.Basics.Exception
import Control.Exception

import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import qualified Data.Number.MPFR as M
import Data.Number.MPFR (MPFR)
import Data.Number.MPFR.Instances.Up
import qualified Data.Number.MPFR.Mutable as MM

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Numeric.AERN.Misc.List
import Numeric.AERN.Misc.Debug

import Data.Maybe

import System.IO.Unsafe

sampleM :: MPFR
sampleM = 0

nanM :: MPFR
nanM = 0/0

instance NumOrd.HasLeast MPFR where
    least = - 1/0

instance NumOrd.HasHighest MPFR where
    highest = 1/0

instance NumOrd.HasExtrema MPFR where

instance NumOrd.PartialComparison MPFR where
    type NumOrd.PartialCompareEffortIndicator MPFR = ()
    pCompareEff _ a b = Just $ toPartialOrdering $ Prelude.compare a b
--        case (M.isNaN a, M.isNaN b) of
--           (False, False) -> Just $ toPartialOrdering $ Prelude.compare a b  
--           (True, True) -> Just EQ
--           _ -> Just NC 
    pCompareDefaultEffort _ = ()

instance NumOrd.RoundedLatticeEffort MPFR where
    type NumOrd.MinmaxEffortIndicator MPFR = ()
    minmaxDefaultEffort _ = ()

instance NumOrd.RoundedLattice MPFR where
    maxUpEff _ a b = Prelude.max a b
--        case (M.isNaN a, M.isNaN b) of
--           (False, False) -> Prelude.max a b  
--           _ -> throw (AERNException $ "illegal MPFR argument: NumOrd.Lattice.max " 
--                        ++ show a ++ " " ++ show b)
    maxDnEff = NumOrd.maxUpEff 
    minUpEff _ a b = Prelude.min a b
--        case (M.isNaN a, M.isNaN b) of
--           (False, False) -> Prelude.min a b  
--           _ -> throw (AERNException $ "illegal MPFR argument: NumOrd.Lattice.min " 
--                        ++ show a ++ " " ++ show b) 
    minDnEff = NumOrd.minUpEff 
    
--    -- a version with artificially added rounding for "testing" the tests
--    maxUpEff [effort] e1 e2 = NumOrd.max e1 e2 + (1/(convert effort))
--    maxDnEff [effort] e1 e2 = NumOrd.max e1 e2 - (1/(convert effort))
--    minUpEff [effort] e1 e2 = NumOrd.min e1 e2 + (1/(convert effort))
--    minDnEff [effort] e1 e2 = NumOrd.min e1 e2 - (1/(convert effort))
--    minmaxDefaultEffort _ = [10]


instance Arbitrary MPFR where
    arbitrary =
        sized $ \size ->
        do
        let sizeI = toInteger size
        precisionI <- choose (50,50+2*sizeI)
        significandI <- arbitrary
        expI <- choose (-10,10) 
           -- it is not essential to test with massive exponents 
           -- except when we test overflows - can set the exponent
           -- high artificially using M.setExp
        let result = constructMPFR precisionI significandI expI
        return result
        where
        constructMPFR precisionI significandI expI =
            M.setExp resultPre exp
            where
            resultPre = M.fromIntegerA M.Near precision significand
            precision = fromInteger precisionI 
            significand
                | significandI == 0 = 1 
                | otherwise = fromInteger significandI 
            exp = fromInteger expI 

instance NumOrd.ArbitraryOrderedTuple MPFR where
    type (NumOrd.Area MPFR) = NumOrd.AreaLinear MPFR
    areaWhole _ = NumOrd.areaLinearWhole [-1/0,-1,0,1,1/0]
    arbitraryTupleInAreaRelatedBy area = 
        NumOrd.linearArbitraryTupleRelatedBy 
                (NumOrd.arbitraryLinear (-1/0, 1/0) id id chooseNearerZero area)
        where
        chooseMPFR (lb, ub) 
            | lbBounded && ubBounded =
                do
                precI <- choose (100,10000)
                let prec = fromIntegral (precI :: Int) 
                return $ lb + ((xUniform prec)*(ub-lb))
            | lbBounded =
                do
                x <- arbitrary
                return $ lb + (abs x :: MPFR) 
            | ubBounded =
                do
                x <- arbitrary
                return $ ub - (abs x :: MPFR)
            | otherwise = arbitrary 
            where
            lbBounded = -1/0 < lb
            ubBounded = ub < 1/0 
            xUniform = M.urandomb M.newRandomStatePointer  
        chooseNearerZero (lo, hi) =
--                unsafePrint ("chooseNearerZero: "
--                    ++ "\n loT = " ++ show loT
--                    ++ "\n hiT = " ++ show hiT
--                ) $
            do
            eT <- chooseMPFR (loT, hiT)
            return $ transform eT 
            where
            loT = transformInv lo
            hiT = transformInv hi
            transform :: MPFR -> MPFR
            transform x = x*x*x/1000000 -- 100^3 = 1000000
            transformInv x 
                | x > 0 = 100 * exp ((log x)/3)
                | x < 0 = - (transformInv (-x))
                | otherwise = 0
    arbitraryTupleRelatedBy =
        NumOrd.arbitraryTupleInAreaRelatedBy (NumOrd.areaWhole (0::MPFR))
    
