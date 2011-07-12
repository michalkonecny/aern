{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#include <basics.h>
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics
    Description :  simple Haskell definitions etc for polynomials in C
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Simple Haskell definitions etc for polynomials in C.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST, unsafeIOToST, unsafeSTToIO)
import Numeric.AERN.Basics.PartialOrdering

import Foreign.C.Types (CDouble)
import Data.Word

{- common function types -}

type UnaryOp t = t -> t
type BinaryOp t =  t -> t -> t
type ConvertOp t1 t2 = t1 -> t2
type ConvertFromDoubleOp t2 = ConvertOp Double t2
type ComparisonOpMutable s t =  Mutable t s -> Mutable t s -> (Maybe PartialOrdering)

type NewOpMutable s t = t -> ST s (Mutable t s)
type CloneOpMutable s t = Mutable t s -> ST s (Mutable t s)
type UnaryFromPureOpMutable s t = Mutable t s -> t -> ST s ()
type UnaryOpMutable s t = Mutable t s -> Mutable t s -> ST s ()
type BinaryOpMutable s t = Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
type MixedIntOpMutable s t = Mutable t s -> Mutable t s -> Int -> ST s ()

{- C <-> Haskell conversions -}

double2CDouble :: Double -> CDouble
double2CDouble = realToFrac

cDouble2Double :: CDouble -> Double
cDouble2Double = realToFrac

type CVar = #type Var
type CSize = #type Size
type CPower = #type Power

{- Haskell analogues of some C types defined in Basics.h -}

newtype Var = Var { unVar :: Word32 } deriving (Eq, Ord, Show, Enum)
{-# INLINE fromCVar #-}
fromCVar :: CVar -> Var
fromCVar v = Var (fromIntegral v)
{-# INLINE toCVar #-}
toCVar :: Var -> CVar
toCVar (Var v) = fromIntegral v

newtype Size = Size { unSize :: Word32 } deriving (Eq, Ord, Show, Enum)
{-# INLINE fromCSize #-}
fromCSize :: CSize -> Size
fromCSize s = Size (fromIntegral s)
{-# INLINE toCSize #-}
toCSize :: Size -> CSize
toCSize (Size s) = fromIntegral s

newtype Power = Power { unPower :: Word32 } deriving (Eq, Ord, Show, Enum)
{-# INLINE fromCPower #-}
fromCPower :: CPower -> Power
fromCPower pwr = Power (fromIntegral pwr)
{-# INLINE toCPower #-}
toCPower :: Power -> CPower
toCPower (Power pwr) = fromIntegral pwr
    

