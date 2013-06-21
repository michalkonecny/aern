{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.DoubleBasis.MRealIntervalApprox
    Description :  Mutable Double intervals for approximating real intervals  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Mutable versions of the abstract data type 'RealIntervalApprox'
-}
module Numeric.AERN.DoubleBasis.MRealIntervalApprox
(
    -- |
    -- A convenience module re-exporting various in-place interval 
    -- operations with default effort indicators.

    -- * Main type
    MRealIntervalApprox,

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
    meetOutInPlace,joinOutInPlace,partialJoinOutInPlace,

    -- **** Assignment operations 

    -- ***** ASCII versions
    (</\>=),(<\/>=),

    -- ***** Unicode versions
    (<⊓>=),(<⊔>=),

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
    
    -- * Inward rounded operations

    -- ** Order operations
    
    -- *** Numerical order
    -- | 
    -- Inward rounded in-place interval extensions of the corresponding 
    -- operations on Double.
    minInInPlace,maxInInPlace,

    -- *** Refinement order
    -- | 
    -- Inward rounded in-place lattice operations in the interval poset.

    -- **** Operations with explicit out parameter    
    meetInInPlace,joinInInPlace,partialJoinInInPlace,

    -- **** Assignment operations 

    -- ***** ASCII versions
    (>/\<=),(>\/<=),

    -- ***** Unicode versions
    (>⊓<=),(>⊔<=),

    -- ** Field operations

    -- *** Interval operations

    -- **** Operations with explicit out parameter    
    addInInPlace,subtrInInPlace,
    multInInPlace,divInInPlace,

    -- **** Assignment operations 
    (>+<=),(>-<=),(>*<=),(>/<=),

    -- *** Mixed type operations

    -- **** Operations with explicit out parameter    
    mixedAddInInPlace,mixedMultInInPlace,mixedDivInInPlace,
    powerToNonnegIntInInPlace,

    -- **** Assignment operations 
    (>+<|=),(>*<|=),(>/<|=),(>^<=),
    
    -- ** Elementary functions
    absInInPlace,expInInPlace,sqrtInInPlace,
    
    -- *** Elementary functions with iteration effort control
    -- |
    -- To be used eg as follows:
    -- 
    -- > expInInPlaceIters 10 resM xM
    --
    -- which means that at most 10 iterations should be used while computing exp of x
    expInInPlaceIters,sqrtInInPlaceIters,
    
    -- * Base class and associted type
    CanBeMutable(..)
)
where

import Numeric.AERN.Basics.Mutable
  (CanBeMutable(..),OpMutable2,OpMutable1,OpPartialMutable2,OpMutableNonmut,OpNonmut)

import qualified Numeric.AERN.NumericOrder as BNO
  (minOutInPlace,maxOutInPlace,
   minInInPlace,maxInInPlace)

import qualified Numeric.AERN.RefinementOrder as BRO
  (meetOutInPlace,(</\>=),(<⊓>=),
   joinOutInPlace,(<\/>=),(<⊔>=),
   partialJoinOutInPlace,
   meetInInPlace,(>/\<=),(>⊓<=),
   joinInInPlace,(>\/<=),(>⊔<=),
   partialJoinInInPlace)

import Numeric.AERN.RealArithmetic.Basis.Double()
import Numeric.AERN.RealArithmetic.Interval.Mutable()

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as NumOrd

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as RAROR
  (RoundedMixedAddInPlace(..),RoundedMixedMultiplyInPlace(..),RoundedMixedDivideInPlace(..))

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsDefaultEffort as RARORIPODE
  (addOutInPlace,(<+>=),
   subtrOutInPlace,(<->=),
   multOutInPlace,(<*>=),
   divOutInPlace,(</>=),
   absOutInPlace,expOutInPlace,sqrtOutInPlace,
   mixedAddOutInPlace,(<+>|=),
   mixedMultOutInPlace,(<*>|=),
   mixedDivOutInPlace,(</>|=),
   powerToNonnegIntOutInPlace,(<^>=),
   addInInPlace,(>+<=),
   subtrInInPlace,(>-<=),
   multInInPlace,(>*<=),
   divInInPlace,(>/<=),
   absInInPlace,expInInPlace,sqrtInInPlace,
   mixedAddInInPlace,(>+<|=),
   mixedMultInInPlace,(>*<|=),
   mixedDivInInPlace,(>/<|=),
   powerToNonnegIntInInPlace,(>^<=))

import qualified Numeric.AERN.RealArithmetic.Interval.Mutable.ElementaryFromFieldOps as RAIMEFFO
    (expOutInPlaceIters, expInInPlaceIters, sqrtOutInPlaceIters, sqrtInInPlaceIters)

import Numeric.AERN.DoubleBasis.RealIntervalApprox (RealIntervalApprox)
import Control.Monad.ST (runST)

infixr 3 </\>=, >/\<=, <⊓>=, >⊓<= 
infixr 2 <\/>=, >\/<=, <⊔>=, >⊔<= 

infixl 6 <+>=, >+<=, <->=, >-<=
infixl 7 <*>=, >*<=
infixl 8 <^>=, >^<=
infixl 7 </>=, >/<=

infixl 6 <+>|=, >+<|=
infixl 7 <*>|=, >*<|=
infixl 7 </>|=, >/<|=

-- | 
-- Mutable 'RealIntervalApprox'. Created and handled using
-- the methods of 'CanBeMutable' as in e.g.
-- 
-- > square :: RealIntervalApprox -> RealIntervalApprox 
-- > square x =
-- >   runST $
-- >     do
-- >     xM <- makeMutable x
-- >     xM <*>= xM
-- >     result <- readMutable xM
-- >     return result
type MRealIntervalApprox = Mutable RealIntervalApprox

-- | Outward rounded in-place minimum
minOutInPlace :: OpMutable2 RealIntervalApprox s
minOutInPlace = BNO.minOutInPlace

-- | Outward rounded in-place maximum
maxOutInPlace :: OpMutable2 RealIntervalApprox s
maxOutInPlace = BNO.maxOutInPlace

-- | Inward rounded in-place minimum
minInInPlace :: OpMutable2 RealIntervalApprox s
minInInPlace = BNO.minInInPlace

-- | Inward rounded in-place maximum
maxInInPlace :: OpMutable2 RealIntervalApprox s
maxInInPlace = BNO.maxInInPlace

-- | Outward rounded in-place meet
meetOutInPlace :: OpMutable2 RealIntervalApprox s
meetOutInPlace = BRO.meetOutInPlace

-- | Outward rounded meet assignment
(</\>=) :: OpMutable1 RealIntervalApprox s
(</\>=) = (BRO.</\>=)

-- | Inward rounded in-place meet
meetInInPlace :: OpMutable2 RealIntervalApprox s
meetInInPlace = BRO.meetInInPlace

-- | Inward rounded meet assignment
(>/\<=) :: OpMutable1 RealIntervalApprox s
(>/\<=) = (BRO.>/\<=)

-- | Outward rounded in-place join
joinOutInPlace :: OpMutable2 RealIntervalApprox s
joinOutInPlace = BRO.joinOutInPlace

-- | Outward rounded join assignment
(<\/>=) :: OpMutable1 RealIntervalApprox s
(<\/>=) = (BRO.<\/>=)

-- | Inward rounded in-place join
joinInInPlace :: OpMutable2 RealIntervalApprox s
joinInInPlace = BRO.joinInInPlace 

-- | Inward rounded join assignment
(>\/<=) :: OpMutable1 RealIntervalApprox s
(>\/<=) = (BRO.>\/<=)

-- | Partial outward rounded in-place join
partialJoinOutInPlace :: OpPartialMutable2 RealIntervalApprox s
partialJoinOutInPlace = BRO.partialJoinOutInPlace

-- | Partial inward rounded in-place join
partialJoinInInPlace :: OpPartialMutable2 RealIntervalApprox s
partialJoinInInPlace = BRO.partialJoinInInPlace

{-| Convenience Unicode notation for '<\/>=' -}
(<⊔>=) :: OpMutable1 RealIntervalApprox s
(<⊔>=) = (<\/>=)

{-| Convenience Unicode notation for '</\>=' -}
(<⊓>=) :: OpMutable1 RealIntervalApprox s
(<⊓>=) = (</\>=)

{-| Convenience Unicode notation for '>\/<=' -}
(>⊔<=) :: OpMutable1 RealIntervalApprox s
(>⊔<=) = (>\/<=)

{-| Convenience Unicode notation for '>/\<=' -}
(>⊓<=) :: OpMutable1 RealIntervalApprox s
(>⊓<=) = (>/\<=)

-- | Inward rounded in-place addition
addInInPlace :: OpMutable2 RealIntervalApprox s
addInInPlace = RARORIPODE.addInInPlace

-- | Inward rounded addition assignment
(>+<=) :: OpMutable1 RealIntervalApprox s
(>+<=) = (RARORIPODE.>+<=)

-- | Outward rounded in-place addition
addOutInPlace :: OpMutable2 RealIntervalApprox s
addOutInPlace = RARORIPODE.addOutInPlace

-- | Outward rounded addition assignment
(<+>=) :: OpMutable1 RealIntervalApprox s
(<+>=) = (RARORIPODE.<+>=)

-- | Inward rounded in-place subtraction
subtrInInPlace :: OpMutable2 RealIntervalApprox s
subtrInInPlace = RARORIPODE.subtrInInPlace

-- | Inward rounded subtraction assignment
(>-<=) :: OpMutable1 RealIntervalApprox s
(>-<=) = (RARORIPODE.>-<=)

-- | Outward rounded in-place subtraction
subtrOutInPlace :: OpMutable2 RealIntervalApprox s
subtrOutInPlace = RARORIPODE.subtrOutInPlace

-- | Outward rounded subtraction assignment
(<->=) :: OpMutable1 RealIntervalApprox s
(<->=) = (RARORIPODE.<->=)

-- | Inward rounded in-place absolute value
absInInPlace :: OpMutable1 RealIntervalApprox s
absInInPlace = RARORIPODE.absInInPlace 

-- | Outward rounded in-place absolute value
absOutInPlace :: OpMutable1 RealIntervalApprox s
absOutInPlace = RARORIPODE.absOutInPlace 

-- | Inward rounded in-place multiplication
multInInPlace :: OpMutable2 RealIntervalApprox s
multInInPlace = RARORIPODE.multInInPlace

-- | Inward rounded multiplication assignment
(>*<=) :: OpMutable1 RealIntervalApprox s
(>*<=) = (RARORIPODE.>*<=)

-- | Outward rounded in-place multiplication
multOutInPlace :: OpMutable2 RealIntervalApprox s
multOutInPlace = RARORIPODE.multOutInPlace

-- | Outward rounded multiplication assignment
(<*>=) :: OpMutable1 RealIntervalApprox s
(<*>=) = (RARORIPODE.<*>=)

-- | Inward rounded in-place power
powerToNonnegIntInInPlace :: OpMutableNonmut RealIntervalApprox Int s
powerToNonnegIntInInPlace = RARORIPODE.powerToNonnegIntInInPlace

-- | Inward rounded in-place power assignment
(>^<=) :: OpNonmut RealIntervalApprox Int s
(>^<=) = (RARORIPODE.>^<=)

-- | Outward rounded in-place power
powerToNonnegIntOutInPlace :: OpMutableNonmut RealIntervalApprox Int s
powerToNonnegIntOutInPlace = RARORIPODE.powerToNonnegIntOutInPlace

-- | Inward rounded in-place power assignment
(<^>=) :: OpNonmut RealIntervalApprox Int s
(<^>=) = (RARORIPODE.<^>=)

-- | Inward rounded in-place division
divInInPlace :: OpMutable2 RealIntervalApprox s
divInInPlace = RARORIPODE.divInInPlace

-- | Inward rounded division assignment
(>/<=) :: OpMutable1 RealIntervalApprox s
(>/<=) = (RARORIPODE.>/<=)

-- | Outward rounded in-place division
divOutInPlace :: OpMutable2 RealIntervalApprox s
divOutInPlace = RARORIPODE.divOutInPlace

-- | Outward rounded division assignment
(</>=) :: OpMutable1 RealIntervalApprox s
(</>=) = (RARORIPODE.</>=)

-- | Inward rounded in-place mixed addition
mixedAddInInPlace :: (RAROR.RoundedMixedAddInPlace RealIntervalApprox tn) => 
    OpMutableNonmut RealIntervalApprox tn s
mixedAddInInPlace = RARORIPODE.mixedAddInInPlace

-- | Inward rounded additive scalar action assignment
(>+<|=) :: (RAROR.RoundedMixedAddInPlace RealIntervalApprox tn) => OpNonmut RealIntervalApprox tn s
(>+<|=) = (RARORIPODE.>+<|=)

-- | Outward rounded in-place mixed addition
mixedAddOutInPlace :: (RAROR.RoundedMixedAddInPlace RealIntervalApprox tn) =>
    OpMutableNonmut RealIntervalApprox tn s
mixedAddOutInPlace = RARORIPODE.mixedAddOutInPlace

-- | Outward rounded additive scalar action assignment
(<+>|=) :: (RAROR.RoundedMixedAddInPlace RealIntervalApprox tn) => OpNonmut RealIntervalApprox tn s
(<+>|=) = (RARORIPODE.<+>|=)

-- | Inward rounded in-place mixed multiplication
mixedMultInInPlace :: (RAROR.RoundedMixedMultiplyInPlace RealIntervalApprox tn) => 
    OpMutableNonmut RealIntervalApprox tn s
mixedMultInInPlace = RARORIPODE.mixedMultInInPlace

-- | Inward rounded multiplicative scalar action assignment
(>*<|=) :: (RAROR.RoundedMixedMultiplyInPlace RealIntervalApprox tn) => OpNonmut RealIntervalApprox tn s
(>*<|=) = (RARORIPODE.>*<|=)

-- | Outward rounded in-place mixed multiplication
mixedMultOutInPlace :: (RAROR.RoundedMixedMultiplyInPlace RealIntervalApprox tn) => 
    OpMutableNonmut RealIntervalApprox tn s
mixedMultOutInPlace = RARORIPODE.mixedMultOutInPlace

-- | Outward rounded multiplicative scalar action assignment
(<*>|=) :: (RAROR.RoundedMixedMultiplyInPlace RealIntervalApprox tn) => OpNonmut RealIntervalApprox tn s
(<*>|=) = (RARORIPODE.<*>|=)

-- | Inward rounded in-place mixed reciprocal action
mixedDivInInPlace :: (RAROR.RoundedMixedDivideInPlace RealIntervalApprox tn) => 
    OpMutableNonmut RealIntervalApprox tn s
mixedDivInInPlace = RARORIPODE.mixedDivInInPlace

-- | Inward rounded multiplicative scalar reciprocal action assignment
(>/<|=) :: (RAROR.RoundedMixedDivideInPlace RealIntervalApprox tn) => OpNonmut RealIntervalApprox tn s
(>/<|=) = (RARORIPODE.>/<|=)

-- | Outward rounded in-place mixed reciprocal action
mixedDivOutInPlace :: (RAROR.RoundedMixedDivideInPlace RealIntervalApprox tn) => 
    OpMutableNonmut RealIntervalApprox tn s
mixedDivOutInPlace = RARORIPODE.mixedDivOutInPlace

-- | Outward rounded multiplicative scalar reciprocal action assignment
(</>|=) :: (RAROR.RoundedMixedDivideInPlace RealIntervalApprox tn) => OpNonmut RealIntervalApprox tn s
(</>|=) = (RARORIPODE.</>|=)

-- | Inward rounded in-place exponential
expInInPlace :: OpMutable1 RealIntervalApprox s
expInInPlace = RARORIPODE.expInInPlace 

-- | Outward rounded in-place exponential
expOutInPlace :: OpMutable1 RealIntervalApprox s
expOutInPlace = RARORIPODE.expOutInPlace 

-- | Inward rounded in-place square root
sqrtInInPlace :: OpMutable1 RealIntervalApprox s
sqrtInInPlace = RARORIPODE.sqrtInInPlace 

-- | Outward rounded in-place square root
sqrtOutInPlace :: OpMutable1 RealIntervalApprox s
sqrtOutInPlace = RARORIPODE.sqrtOutInPlace 

expOutInPlaceIters, sqrtOutInPlaceIters,
 expInInPlaceIters, sqrtInInPlaceIters :: Int -> OpMutable1 RealIntervalApprox s
expOutInPlaceIters = RAIMEFFO.expOutInPlaceIters
sqrtOutInPlaceIters = RAIMEFFO.sqrtOutInPlaceIters
expInInPlaceIters = RAIMEFFO.expInInPlaceIters
sqrtInInPlaceIters = RAIMEFFO.sqrtInInPlaceIters

