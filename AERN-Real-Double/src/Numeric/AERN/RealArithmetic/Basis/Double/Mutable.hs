{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.Mutable
    Description :  mutable version of Double
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    One, zero, negation etc for Double numbers.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.Double.Mutable where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.NumericOrder

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding 

import Numeric.AERN.RealArithmetic.Basis.Double.FieldOps
import Numeric.AERN.RealArithmetic.Basis.Double.MixedFieldOps

import Data.STRef
import Data.Ratio

import Control.Monad.ST (unsafeIOToST)


instance CanBeMutable Double where
    data Mutable Double s = 
        MDouble { unMDouble :: STRef s Double }
    makeMutable a = 
        do
        v <- newSTRef a
        return $ MDouble v  
    unsafeMakeMutable = makeMutable
    writeMutable (MDouble v) a = writeSTRef v a  
    unsafeWriteMutable = writeMutable
    readMutable (MDouble v) = readSTRef v  
    unsafeReadMutable = readMutable 
    sameVariable (MDouble v1) (MDouble v2) = v1 == v2

instance RoundedLatticeInPlace Double
    where
    maxUpInPlaceEff = maxUpInPlaceEffFromPure
    maxDnInPlaceEff = maxDnInPlaceEffFromPure
    minUpInPlaceEff = minUpInPlaceEffFromPure
    minDnInPlaceEff = minDnInPlaceEffFromPure

instance NegInPlace Double
    where
    negInPlace = pureToMutable1 neg
--    negInPlace resM pM =
--        do
--        res <- readMutable resM
--        p <- readMutable pM
--        unsafeIOToST $ putStrLn $ "Double negInPlace starting: (resM = " ++ show res ++ ", pM = " ++ show p ++ ")"
--        pureToMutable1 neg resM pM
--        res <- readMutable resM
--        p <- readMutable pM
--        unsafeIOToST $ putStrLn $ "Double negInPlace finishing: (resM = " ++ show res ++ ", pM = " ++ show p ++ ")"
     
instance RoundedAddInPlace Double
    where
    addUpInPlaceEff = addUpInPlaceEffFromPure
    addDnInPlaceEff = addDnInPlaceEffFromPure
    
instance RoundedSubtrInPlace Double

instance RoundedAbsInPlace Double
    where
    absUpInPlaceEff = absUpInPlaceEffFromPure
    absDnInPlaceEff = absDnInPlaceEffFromPure

instance RoundedMultiplyInPlace Double
    where
    multUpInPlaceEff = multUpInPlaceEffFromPure
    multDnInPlaceEff = multDnInPlaceEffFromPure

instance RoundedPowerNonnegToNonnegIntInPlace Double
    where
    powerNonnegToNonnegIntUpInPlaceEff = powerNonnegToNonnegIntUpInPlaceEffFromMult
    powerNonnegToNonnegIntDnInPlaceEff = powerNonnegToNonnegIntDnInPlaceEffFromMult
    
instance RoundedPowerToNonnegIntInPlace Double
    where
    powerToNonnegIntUpInPlaceEff = powerToNonnegIntUpInPlaceEffFromPure
    powerToNonnegIntDnInPlaceEff = powerToNonnegIntDnInPlaceEffFromPure
    
instance RoundedDivideInPlace Double
    where
    divUpInPlaceEff = divUpInPlaceEffFromPure
    divDnInPlaceEff = divDnInPlaceEffFromPure
    
instance RoundedRingInPlace Double
instance RoundedFieldInPlace Double
  
instance RoundedMixedAddInPlace Double Int
    where
    mixedAddUpInPlaceEff = mixedAddUpInPlaceEffFromPure
    mixedAddDnInPlaceEff = mixedAddDnInPlaceEffFromPure
    
instance RoundedMixedAddInPlace Double Integer
    where
    mixedAddUpInPlaceEff = mixedAddUpInPlaceEffFromPure
    mixedAddDnInPlaceEff = mixedAddDnInPlaceEffFromPure

instance RoundedMixedAddInPlace Double Double
    where
    mixedAddUpInPlaceEff = mixedAddUpInPlaceEffFromPure
    mixedAddDnInPlaceEff = mixedAddDnInPlaceEffFromPure

instance RoundedMixedAddInPlace Double Rational
    where
    mixedAddUpInPlaceEff = mixedAddUpInPlaceEffFromPure
    mixedAddDnInPlaceEff = mixedAddDnInPlaceEffFromPure
 

instance RoundedMixedMultiplyInPlace Double Int
    where
    mixedMultUpInPlaceEff = mixedMultUpInPlaceEffFromPure
    mixedMultDnInPlaceEff = mixedMultDnInPlaceEffFromPure

instance RoundedMixedMultiplyInPlace Double Integer
    where
    mixedMultUpInPlaceEff = mixedMultUpInPlaceEffFromPure
    mixedMultDnInPlaceEff = mixedMultDnInPlaceEffFromPure

instance RoundedMixedMultiplyInPlace Double Double
    where
    mixedMultUpInPlaceEff = mixedMultUpInPlaceEffFromPure
    mixedMultDnInPlaceEff = mixedMultDnInPlaceEffFromPure

instance RoundedMixedMultiplyInPlace Double Rational
    where
    mixedMultUpInPlaceEff = mixedMultUpInPlaceEffFromPure
    mixedMultDnInPlaceEff = mixedMultDnInPlaceEffFromPure

 
instance RoundedMixedDivideInPlace Double Int
    where
    mixedDivUpInPlaceEff = mixedDivUpInPlaceEffFromPure
    mixedDivDnInPlaceEff = mixedDivDnInPlaceEffFromPure

instance RoundedMixedDivideInPlace Double Integer
    where
    mixedDivUpInPlaceEff = mixedDivUpInPlaceEffFromPure
    mixedDivDnInPlaceEff = mixedDivDnInPlaceEffFromPure

instance RoundedMixedDivideInPlace Double Double
    where
    mixedDivUpInPlaceEff = mixedDivUpInPlaceEffFromPure
    mixedDivDnInPlaceEff = mixedDivDnInPlaceEffFromPure

instance RoundedMixedDivideInPlace Double Rational
    where
    mixedDivUpInPlaceEff = mixedDivUpInPlaceEffFromPure
    mixedDivDnInPlaceEff = mixedDivDnInPlaceEffFromPure

instance RoundedMixedRingInPlace Double Int
instance RoundedMixedRingInPlace Double Integer
instance RoundedMixedRingInPlace Double Double
instance RoundedMixedRingInPlace Double Rational
 
instance RoundedMixedFieldInPlace Double Int
instance RoundedMixedFieldInPlace Double Integer
instance RoundedMixedFieldInPlace Double Double
instance RoundedMixedFieldInPlace Double Rational
 
 

