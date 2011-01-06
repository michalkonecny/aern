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

newtype MDouble s = MDouble { unMDouble :: STRef s Double }

instance CanBeMutable Double where
    type Mutable Double = MDouble
    makeMutable a = 
        do
        v <- newSTRef a
        return $ MDouble v  
    unsafeMakeMutable = makeMutable
    writeMutable (MDouble v) a = writeSTRef v a  
    unsafeWriteMutable = writeMutable
    readMutable (MDouble v) = readSTRef v  
    unsafeReadMutable = readMutable 

instance RoundedLatticeInPlace Double
instance NegInPlace Double
instance RoundedAddInPlace Double
instance RoundedSubtrInPlace Double
instance RoundedAbsInPlace Double
instance RoundedMultiplyInPlace Double
instance RoundedPowerNonnegToNonnegIntInPlace Double
    where
    powerNonnegToNonnegIntUpInPlaceEff = powerNonnegToNonnegIntUpInPlaceEffFromMult
    powerNonnegToNonnegIntDnInPlaceEff = powerNonnegToNonnegIntDnInPlaceEffFromMult
instance RoundedPowerToNonnegIntInPlace Double
instance RoundedDivideInPlace Double
instance RoundedRingInPlace Double
instance RoundedFieldInPlace Double
  
instance RoundedMixedAddInPlace Double Int
instance RoundedMixedAddInPlace Double Integer
instance RoundedMixedAddInPlace Double Double
instance RoundedMixedAddInPlace Double Rational
 
instance RoundedMixedMultiplyInPlace Double Int
instance RoundedMixedMultiplyInPlace Double Integer
instance RoundedMixedMultiplyInPlace Double Double
instance RoundedMixedMultiplyInPlace Double Rational
 
instance RoundedMixedDivideInPlace Double Int
instance RoundedMixedDivideInPlace Double Integer
instance RoundedMixedDivideInPlace Double Double
instance RoundedMixedDivideInPlace Double Rational
 
instance RoundedMixedRingInPlace Double Int
instance RoundedMixedRingInPlace Double Integer
instance RoundedMixedRingInPlace Double Double
instance RoundedMixedRingInPlace Double Rational
 
instance RoundedMixedFieldInPlace Double Int
instance RoundedMixedFieldInPlace Double Integer
instance RoundedMixedFieldInPlace Double Double
instance RoundedMixedFieldInPlace Double Rational
 
 

