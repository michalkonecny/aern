{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.DoubleBasis.MRealApprox
    Description :  Mutable Double intervals for approximating real numbers  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Mutable version of the abstract data type 'RealApprox'.
-}
module Numeric.AERN.DoubleBasis.MRealApprox
(
    -- |
    -- A convenience module re-exporting various in-place interval 
    -- operations with default effort indicators.

    -- * Main type
    MRealApprox,

    -- * Outward rounded operations

    -- | 
    -- In-place interval extensions of common functions.
    --
    -- The /first/ parameter of the in-place operations listed below
    -- is the /out/ parameter. Actual parameters are allowed to appear 
    -- both as in and out paramers as in e.g.
    --
    -- > meetOutInPlace xM xM yM
    --
    -- which is equivalent to the assignment version 
    --
    -- > xM </\>= yM

    -- ** Order operations
    
    -- *** Numerical order
    -- | 
    -- Outward rounded in-place interval extensions of the corresponding 
    -- operations on Double.
    minOutInPlace,maxOutInPlace,

    -- *** Refinement order
    -- | 
    -- Outward rounded in-place lattice operations in the interval poset.

    -- **** Operations with explicit out parameter    
    meetOutInPlace,partialJoinOutInPlace,

    -- **** Assignment operations 

    -- ***** ASCII versions
    (</\>=),

    -- ***** Unicode versions
    (<⊓>=),

    -- ** Field operations

    -- *** Interval operations

    -- **** Operations with explicit out parameter    
    addOutInPlace,subtrOutInPlace,
    multOutInPlace,divOutInPlace,
    
    -- **** Assignment operations 
    (<+>=),(<->=),(<*>=),(</>=),

    -- *** Mixed type operations

    -- **** Operations with explicit out parameter    
    mixedAddOutInPlace,mixedMultOutInPlace,mixedDivOutInPlace,
    powerToNonnegIntOutInPlace,

    -- **** Assignment operations
    (<+>|=),(<*>|=),(</>|=),(<^>=),
    
    -- ** Elementary functions
    absOutInPlace,expOutInPlace,sqrtOutInPlace,

    -- *** Elementary functions with iteration effort control
    -- |
    -- To be used eg as follows:
    -- 
    -- > expOutInPlaceIters 10 resM xM
    --
    -- which means that at most 10 iterations should be used while computing exp of x
    expOutInPlaceIters,sqrtOutInPlaceIters,
    
    -- * Base class and associted type
    CanBeMutable(..)
)
where

import Numeric.AERN.Basics.Mutable
  (CanBeMutable(..),OpMutable2,OpMutable1,OpPartialMutable2,OpMutableNonmut,OpNonmut)

import qualified Numeric.AERN.NumericOrder as BNO
  (minOutInPlace,maxOutInPlace)

import qualified Numeric.AERN.RefinementOrder as BRO
  (meetOutInPlace,(</\>=),(<⊓>=),
   partialJoinOutInPlace)

import Numeric.AERN.RealArithmetic.Basis.Double()
import Numeric.AERN.RealArithmetic.Interval.Mutable()

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as NumOrd

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as RAROR
  (RoundedMixedAddInPlace(..),RoundedMixedMultiplyInPlace(..),RoundedMixedDivideInPlace(..),
   addOutInPlace,(<+>=),
   subtrOutInPlace,(<->=),
   multOutInPlace,(<*>=),
   divOutInPlace,(</>=),
   absOutInPlace,
   expOutInPlace,
   sqrtOutInPlace,
   mixedAddOutInPlace,(<+>|=),
   mixedMultOutInPlace,(<*>|=),
   mixedDivOutInPlace,(</>|=),
   powerToNonnegIntOutInPlace,(<^>=)
  )

import qualified Numeric.AERN.RealArithmetic.Interval.Mutable.ElementaryFromFieldOps as RAIMEFFO
    (expOutInPlaceIters, sqrtOutInPlaceIters)

import Numeric.AERN.DoubleBasis.RealApprox (RealApprox)
import Control.Monad.ST (runST)

infixr 3 </\>=, <⊓>=

infixl 6 <+>=, <->=
infixl 7 <*>=
infixl 8 <^>=
infixl 7 </>=

infixl 6 <+>|=
infixl 7 <*>|=
infixl 7 </>|=

-- | 
-- Mutable 'RealApprox'. Created and handled using
-- the methods of 'CanBeMutable' as in e.g.
-- 
-- > square :: RealApprox -> RealApprox 
-- > square x =
-- >   runST $
-- >     do
-- >     xM <- makeMutable x
-- >     xM <*>= xM
-- >     result <- readMutable xM
-- >     return result
type MRealApprox = Mutable RealApprox

-- | Outward rounded in-place minimum
minOutInPlace :: OpMutable2 RealApprox s
minOutInPlace = BNO.minOutInPlace

-- | Outward rounded in-place maximum
maxOutInPlace :: OpMutable2 RealApprox s
maxOutInPlace = BNO.maxOutInPlace

-- | Outward rounded in-place meet
meetOutInPlace :: OpMutable2 RealApprox s
meetOutInPlace = BRO.meetOutInPlace

-- | Outward rounded meet assignment
(</\>=) :: OpMutable1 RealApprox s
(</\>=) = (BRO.</\>=)

-- | Partial outward rounded in-place join
partialJoinOutInPlace :: OpPartialMutable2 RealApprox s
partialJoinOutInPlace = BRO.partialJoinOutInPlace

{-| Convenience Unicode notation for '</\>=' -}
(<⊓>=) :: OpMutable1 RealApprox s
(<⊓>=) = (</\>=)

-- | Outward rounded in-place addition
addOutInPlace :: OpMutable2 RealApprox s
addOutInPlace = RAROR.addOutInPlace

-- | Outward rounded addition assignment
(<+>=) :: OpMutable1 RealApprox s
(<+>=) = (RAROR.<+>=)

-- | Outward rounded in-place subtraction
subtrOutInPlace :: OpMutable2 RealApprox s
subtrOutInPlace = RAROR.subtrOutInPlace

-- | Outward rounded subtraction assignment
(<->=) :: OpMutable1 RealApprox s
(<->=) = (RAROR.<->=)

-- | Outward rounded in-place absolute value
absOutInPlace :: OpMutable1 RealApprox s
absOutInPlace = RAROR.absOutInPlace 

-- | Outward rounded in-place multiplication
multOutInPlace :: OpMutable2 RealApprox s
multOutInPlace = RAROR.multOutInPlace

-- | Outward rounded multiplication assignment
(<*>=) :: OpMutable1 RealApprox s
(<*>=) = (RAROR.<*>=)

-- | Outward rounded in-place power
powerToNonnegIntOutInPlace :: OpMutableNonmut RealApprox Int s
powerToNonnegIntOutInPlace = RAROR.powerToNonnegIntOutInPlace

-- | Inward rounded in-place power assignment
(<^>=) :: OpNonmut RealApprox Int s
(<^>=) = (RAROR.<^>=)

-- | Outward rounded in-place division
divOutInPlace :: OpMutable2 RealApprox s
divOutInPlace = RAROR.divOutInPlace

-- | Outward rounded division assignment
(</>=) :: OpMutable1 RealApprox s
(</>=) = (RAROR.</>=)

-- | Outward rounded in-place mixed addition
mixedAddOutInPlace :: (RAROR.RoundedMixedAddInPlace RealApprox tn) =>
    OpMutableNonmut RealApprox tn s
mixedAddOutInPlace = RAROR.mixedAddOutInPlace

-- | Outward rounded additive scalar action assignment
(<+>|=) :: (RAROR.RoundedMixedAddInPlace RealApprox tn) => OpNonmut RealApprox tn s
(<+>|=) = (RAROR.<+>|=)

-- | Outward rounded in-place mixed multiplication
mixedMultOutInPlace :: (RAROR.RoundedMixedMultiplyInPlace RealApprox tn) => 
    OpMutableNonmut RealApprox tn s
mixedMultOutInPlace = RAROR.mixedMultOutInPlace

-- | Outward rounded multiplicative scalar action assignment
(<*>|=) :: (RAROR.RoundedMixedMultiplyInPlace RealApprox tn) => OpNonmut RealApprox tn s
(<*>|=) = (RAROR.<*>|=)

-- | Outward rounded in-place mixed reciprocal action
mixedDivOutInPlace :: (RAROR.RoundedMixedDivideInPlace RealApprox tn) => 
    OpMutableNonmut RealApprox tn s
mixedDivOutInPlace = RAROR.mixedDivOutInPlace

-- | Outward rounded multiplicative scalar reciprocal action assignment
(</>|=) :: (RAROR.RoundedMixedDivideInPlace RealApprox tn) => OpNonmut RealApprox tn s
(</>|=) = (RAROR.</>|=)

-- | Outward rounded in-place exponential
expOutInPlace :: OpMutable1 RealApprox s
expOutInPlace = RAROR.expOutInPlace 

-- | Outward rounded in-place square root
sqrtOutInPlace :: OpMutable1 RealApprox s
sqrtOutInPlace = RAROR.sqrtOutInPlace 

expOutInPlaceIters, sqrtOutInPlaceIters :: Int -> OpMutable1 RealApprox s
expOutInPlaceIters = RAIMEFFO.expOutInPlaceIters
sqrtOutInPlaceIters = RAIMEFFO.sqrtOutInPlaceIters
