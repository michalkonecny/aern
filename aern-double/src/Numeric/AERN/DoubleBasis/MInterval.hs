{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.DoubleBasis.MInterval
    Description :  Interval Double type and operations  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Mutable intervals with Double endpoints.
-}
module Numeric.AERN.DoubleBasis.MInterval 
(
    -- |
    -- A convenience module re-exporting various in-place interval 
    -- operations with default effort indicators.

    -- * Main type
    MDI,

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

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as RAROR
  (RoundedMixedAddInPlace(..),RoundedMixedMultiplyInPlace(..),RoundedMixedDivideInPlace(..),
   addOutInPlace,(<+>=),
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
   powerToNonnegIntInInPlace,(>^<=)
  )

import Numeric.AERN.DoubleBasis.Interval
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
-- Mutable 'DI'. Created and handled using
-- the methods of 'CanBeMutable' as in e.g.
-- 
-- > square :: DI -> DI 
-- > square x =
-- >   runST $
-- >     do
-- >     xM <- makeMutable x
-- >     xM <*>= xM
-- >     result <- readMutable xM
-- >     return result
type MDI = Mutable DI

-- | Outward rounded in-place minimum
minOutInPlace :: OpMutable2 DI s
minOutInPlace = BNO.minOutInPlace

-- | Outward rounded in-place maximum
maxOutInPlace :: OpMutable2 DI s
maxOutInPlace = BNO.maxOutInPlace

-- | Inward rounded in-place minimum
minInInPlace :: OpMutable2 DI s
minInInPlace = BNO.minInInPlace

-- | Inward rounded in-place maximum
maxInInPlace :: OpMutable2 DI s
maxInInPlace = BNO.maxInInPlace

-- | Outward rounded in-place meet
meetOutInPlace :: OpMutable2 DI s
meetOutInPlace = BRO.meetOutInPlace

-- | Outward rounded meet assignment
(</\>=) :: OpMutable1 DI s
(</\>=) = (BRO.</\>=)

-- | Inward rounded in-place meet
meetInInPlace :: OpMutable2 DI s
meetInInPlace = BRO.meetInInPlace

-- | Inward rounded meet assignment
(>/\<=) :: OpMutable1 DI s
(>/\<=) = (BRO.>/\<=)

-- | Outward rounded in-place join
joinOutInPlace :: OpMutable2 DI s
joinOutInPlace = BRO.joinOutInPlace

-- | Outward rounded join assignment
(<\/>=) :: OpMutable1 DI s
(<\/>=) = (BRO.<\/>=)

-- | Inward rounded in-place join
joinInInPlace :: OpMutable2 DI s
joinInInPlace = BRO.joinInInPlace 

-- | Inward rounded join assignment
(>\/<=) :: OpMutable1 DI s
(>\/<=) = (BRO.>\/<=)

-- | Partial outward rounded in-place join
partialJoinOutInPlace :: OpPartialMutable2 DI s
partialJoinOutInPlace = BRO.partialJoinOutInPlace

-- | Partial inward rounded in-place join
partialJoinInInPlace :: OpPartialMutable2 DI s
partialJoinInInPlace = BRO.partialJoinInInPlace

{-| Convenience Unicode notation for '<\/>=' -}
(<⊔>=) :: OpMutable1 DI s
(<⊔>=) = (<\/>=)

{-| Convenience Unicode notation for '</\>=' -}
(<⊓>=) :: OpMutable1 DI s
(<⊓>=) = (</\>=)

{-| Convenience Unicode notation for '>\/<=' -}
(>⊔<=) :: OpMutable1 DI s
(>⊔<=) = (>\/<=)

{-| Convenience Unicode notation for '>/\<=' -}
(>⊓<=) :: OpMutable1 DI s
(>⊓<=) = (>/\<=)

-- | Inward rounded in-place addition
addInInPlace :: OpMutable2 DI s
addInInPlace = RAROR.addInInPlace

-- | Inward rounded addition assignment
(>+<=) :: OpMutable1 DI s
(>+<=) = (RAROR.>+<=)

-- | Outward rounded in-place addition
addOutInPlace :: OpMutable2 DI s
addOutInPlace = RAROR.addOutInPlace

-- | Outward rounded addition assignment
(<+>=) :: OpMutable1 DI s
(<+>=) = (RAROR.<+>=)

-- | Inward rounded in-place subtraction
subtrInInPlace :: OpMutable2 DI s
subtrInInPlace = RAROR.subtrInInPlace

-- | Inward rounded subtraction assignment
(>-<=) :: OpMutable1 DI s
(>-<=) = (RAROR.>-<=)

-- | Outward rounded in-place subtraction
subtrOutInPlace :: OpMutable2 DI s
subtrOutInPlace = RAROR.subtrOutInPlace

-- | Outward rounded subtraction assignment
(<->=) :: OpMutable1 DI s
(<->=) = (RAROR.<->=)

-- | Inward rounded in-place absolute value
absInInPlace :: OpMutable1 DI s
absInInPlace = RAROR.absInInPlace 

-- | Outward rounded in-place absolute value
absOutInPlace :: OpMutable1 DI s
absOutInPlace = RAROR.absOutInPlace 

-- | Inward rounded in-place multiplication
multInInPlace :: OpMutable2 DI s
multInInPlace = RAROR.multInInPlace

-- | Inward rounded multiplication assignment
(>*<=) :: OpMutable1 DI s
(>*<=) = (RAROR.>*<=)

-- | Outward rounded in-place multiplication
multOutInPlace :: OpMutable2 DI s
multOutInPlace = RAROR.multOutInPlace

-- | Outward rounded multiplication assignment
(<*>=) :: OpMutable1 DI s
(<*>=) = (RAROR.<*>=)

-- | Inward rounded in-place power
powerToNonnegIntInInPlace :: OpMutableNonmut DI Int s
powerToNonnegIntInInPlace = RAROR.powerToNonnegIntInInPlace

-- | Inward rounded in-place power assignment
(>^<=) :: OpNonmut DI Int s
(>^<=) = (RAROR.>^<=)

-- | Outward rounded in-place power
powerToNonnegIntOutInPlace :: OpMutableNonmut DI Int s
powerToNonnegIntOutInPlace = RAROR.powerToNonnegIntOutInPlace

-- | Inward rounded in-place power assignment
(<^>=) :: OpNonmut DI Int s
(<^>=) = (RAROR.<^>=)

-- | Inward rounded in-place division
divInInPlace :: OpMutable2 DI s
divInInPlace = RAROR.divInInPlace

-- | Inward rounded division assignment
(>/<=) :: OpMutable1 DI s
(>/<=) = (RAROR.>/<=)

-- | Outward rounded in-place division
divOutInPlace :: OpMutable2 DI s
divOutInPlace = RAROR.divOutInPlace

-- | Outward rounded division assignment
(</>=) :: OpMutable1 DI s
(</>=) = (RAROR.</>=)

-- | Inward rounded in-place mixed addition
mixedAddInInPlace :: (RAROR.RoundedMixedAddInPlace DI tn) => 
    OpMutableNonmut DI tn s
mixedAddInInPlace = RAROR.mixedAddInInPlace

-- | Inward rounded additive scalar action assignment
(>+<|=) :: (RAROR.RoundedMixedAddInPlace DI tn) => OpNonmut DI tn s
(>+<|=) = (RAROR.>+<|=)

-- | Outward rounded in-place mixed addition
mixedAddOutInPlace :: (RAROR.RoundedMixedAddInPlace DI tn) =>
    OpMutableNonmut DI tn s
mixedAddOutInPlace = RAROR.mixedAddOutInPlace

-- | Outward rounded additive scalar action assignment
(<+>|=) :: (RAROR.RoundedMixedAddInPlace DI tn) => OpNonmut DI tn s
(<+>|=) = (RAROR.<+>|=)

-- | Inward rounded in-place mixed multiplication
mixedMultInInPlace :: (RAROR.RoundedMixedMultiplyInPlace DI tn) => 
    OpMutableNonmut DI tn s
mixedMultInInPlace = RAROR.mixedMultInInPlace

-- | Inward rounded multiplicative scalar action assignment
(>*<|=) :: (RAROR.RoundedMixedMultiplyInPlace DI tn) => OpNonmut DI tn s
(>*<|=) = (RAROR.>*<|=)

-- | Outward rounded in-place mixed multiplication
mixedMultOutInPlace :: (RAROR.RoundedMixedMultiplyInPlace DI tn) => 
    OpMutableNonmut DI tn s
mixedMultOutInPlace = RAROR.mixedMultOutInPlace

-- | Outward rounded multiplicative scalar action assignment
(<*>|=) :: (RAROR.RoundedMixedMultiplyInPlace DI tn) => OpNonmut DI tn s
(<*>|=) = (RAROR.<*>|=)

-- | Inward rounded in-place mixed reciprocal action
mixedDivInInPlace :: (RAROR.RoundedMixedDivideInPlace DI tn) => 
    OpMutableNonmut DI tn s
mixedDivInInPlace = RAROR.mixedDivInInPlace

-- | Inward rounded multiplicative scalar reciprocal action assignment
(>/<|=) :: (RAROR.RoundedMixedDivideInPlace DI tn) => OpNonmut DI tn s
(>/<|=) = (RAROR.>/<|=)

-- | Outward rounded in-place mixed reciprocal action
mixedDivOutInPlace :: (RAROR.RoundedMixedDivideInPlace DI tn) => 
    OpMutableNonmut DI tn s
mixedDivOutInPlace = RAROR.mixedDivOutInPlace

-- | Outward rounded multiplicative scalar reciprocal action assignment
(</>|=) :: (RAROR.RoundedMixedDivideInPlace DI tn) => OpNonmut DI tn s
(</>|=) = (RAROR.</>|=)

-- | Inward rounded in-place exponential
expInInPlace :: OpMutable1 DI s
expInInPlace = RAROR.expInInPlace 

-- | Outward rounded in-place exponential
expOutInPlace :: OpMutable1 DI s
expOutInPlace = RAROR.expOutInPlace 

-- | Inward rounded in-place square root
sqrtInInPlace :: OpMutable1 DI s
sqrtInInPlace = RAROR.sqrtInInPlace 

-- | Outward rounded in-place square root
sqrtOutInPlace :: OpMutable1 DI s
sqrtOutInPlace = RAROR.sqrtOutInPlace 

