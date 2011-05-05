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
  (CanBeMutable(..),OpMutable2)

import qualified Numeric.AERN.Basics.NumericOrder.InPlace.OpsDefaultEffort as BNOIPODE
  (minOutInPlace,maxOutInPlace,
   minInInPlace,maxInInPlace)

import qualified Numeric.AERN.Basics.RefinementOrder.InPlace.OpsDefaultEffort as BROIPODE
  (meetOutInPlace,(</\>=),(<⊓>=),
   joinOutInPlace,(<\/>=),(<⊔>=),
   partialJoinOutInPlace,
   meetInInPlace,(>/\<=),(>⊓<=),
   joinInInPlace,(>\/<=),(>⊔<=),
   partialJoinInInPlace)

import Numeric.AERN.RealArithmetic.Basis.Double()
import Numeric.AERN.RealArithmetic.Interval.Mutable()
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as NumOrd
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

import Numeric.AERN.RealArithmetic.Interval.Mutable.ElementaryFromFieldOps
    (expOutInPlaceIters, expInInPlaceIters, sqrtOutInPlaceIters, sqrtInInPlaceIters)

import Numeric.AERN.DoubleBasis.Interval
import Control.Monad.ST (runST)

-- | 
-- Mutable intervals with Double endpoints. Created and handled using
-- the methods of 'CanBeMutable' as in e.g.
-- 
-- > identity :: DI -> DI 
-- > identity x =
-- >   runST $
-- >     do
-- >     xM <- makeMutable x
-- >     result <- readMutable xM
-- >     return result
type MDI = Mutable DI

-- | Outward rounded in-place minimum
minOutInPlace :: OpMutable2 DI s
minOutInPlace = BNOIPODE.minOutInPlace

-- | Outward rounded in-place maximum
maxOutInPlace :: OpMutable2 DI s
maxOutInPlace = BNOIPODE.maxOutInPlace

-- | Inward rounded in-place minimum
minInInPlace :: OpMutable2 DI s
minInInPlace = BNOIPODE.minInInPlace

-- | Inward rounded in-place maximum
maxInInPlace :: OpMutable2 DI s
maxInInPlace = BNOIPODE.maxInInPlace

infixr 3 </\>=, >/\<=, <⊓>=, >⊓<= 
infixr 2 <\/>=, >\/<=, <⊔>=, >⊔<= 

-- | Outward rounded in-place meet
meetOutInPlace :: OpMutable2 DI s
meetOutInPlace = BROIPODE.meetOutInPlace

-- | Outward rounded meet assignment
(</\>=) :: OpMutable1 DI s
(</\>=) = (BROIPODE.</\>=)

-- | Inward rounded in-place meet
meetInInPlace :: OpMutable2 DI s
meetInInPlace = BROIPODE.meetInInPlace

-- | Inward rounded meet assignment
(>/\<=) :: OpMutable1 DI s
(>/\<=) = (BROIPODE.>/\<=)

-- | Outward rounded in-place join
joinOutInPlace :: OpMutable2 DI s
joinOutInPlace = BROIPODE.joinOutInPlace

-- | Outward rounded join assignment
(<\/>=) :: OpMutable1 DI s
(<\/>=) = (BROIPODE.<\/>=)

-- | Inward rounded in-place join
joinInInPlace :: OpMutable2 DI s
joinInInPlace = BROIPODE.joinInInPlace 

-- | Inward rounded join assignment
(>\/<=) :: OpMutable1 DI s
(>\/<=) = (BROIPODE.>\/<=)

-- | Partial outward rounded in-place join
partialJoinOutInPlace :: OpPartialMutable2 DI s
partialJoinOutInPlace = BROIPODE.partialJoinOutInPlace

-- | Partial inward rounded in-place join
partialJoinInInPlace :: OpPartialMutable2 DI s
partialJoinInInPlace = BROIPODE.partialJoinInInPlace

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

infixl 6 <+>=, >+<=, <->=, >-<=
infixl 7 <*>=, >*<=
infixl 8 <^>=, >^<=
infixl 7 </>=, >/<=

infixl 6 <+>|=, >+<|=
infixl 7 <*>|=, >*<|=
infixl 7 </>|=, >/<|=

-- | Inward rounded in-place addition
addInInPlace :: OpMutable2 DI s
addInInPlace = RARORIPODE.addInInPlace

-- | Inward rounded addition assignment
(>+<=) :: OpMutable1 DI s
(>+<=) = (RARORIPODE.>+<=)

-- | Outward rounded in-place addition
addOutInPlace OpMutable2 DI s
addOutInPlace = RARORIPODE.addOutInPlace

-- | Outward rounded addition assignment
(<+>=) :: OpMutable1 DI s
(<+>=) = (RARORIPODE.<+>=)

-- | Inward rounded in-place subtraction
subtrInInPlace :: OpMutable2 DI s
subtrInInPlace = RARORIPODE.subtrInInPlace

-- | Inward rounded subtraction assignment
(>-<=) :: OpMutable1 DI s
(>-<=) = (RARORIPODE.>-<=)

-- | Outward rounded in-place subtraction
subtrOutInPlace :: OpMutable2 DI s
subtrOutInPlace = RARORIPODE.subtrOutInPlace

-- | Outward rounded subtraction assignment
(<->=) :: OpMutable1 DI s
(<->=) = (RARORIPODE.<->=)

-- | Inward rounded in-place absolute value
absInInPlace :: OpMutable1 DI s
absInInPlace = RARORIPODE.absInInPlace 

-- | Outward rounded in-place absolute value
absOutInPlace :: OpMutable1 DI s
absOutInPlace = RARORIPODE.absOutInPlace 

-- | Inward rounded in-place multiplication
multInInPlace :: OpMutable2 DI s
multInInPlace = RARORIPODE.multInInPlace

-- | Inward rounded multiplication assignment
(>*<=) :: OpMutable1 DI s
(>*<=) = (RARORIPODE.>*<=)

-- | Outward rounded in-place multiplication
multOutInPlace :: OpMutable2 DI s
multOutInPlace = RARORIPODE.multOutInPlace

-- | Outward rounded multiplication assignment
(<*>=) :: OpMutable1 DI s
(<*>=) = (RARORIPODE.<*>=)

-- | Inward rounded in-place power
powerToNonnegIntInInPlace :: OpMutableNonmut DI Int s
powerToNonnegIntInInPlace = RARORIPODE.powerToNonnegIntInInPlace

-- | Inward rounded in-place power assignment
(>^<=) :: OpNonmut DI Int s
(>^<=) = (RARORIPODE.>^<=)

-- | Outward rounded in-place power
powerToNonnegIntOutInPlace :: OpMutableNonmut DI Int s
powerToNonnegIntOutInPlace = RARORIPODE.powerToNonnegIntOutInPlace

-- | Inward rounded in-place power assignment
(<^>=) :: OpNonmut DI Int s
(<^>=) = (RARORIPODE.<^>=)

-- | Inward rounded in-place division
divInInPlace :: OpMutable2 DI s
divInInPlace = RARORIPODE.divInInPlace

-- | Inward rounded division assignment
(>/<=) :: OpMutable1 DI s
(>/<=) = (RARORIPODE.>/<=)

-- | Outward rounded in-place division
divOutInPlace :: OpMutable2 DI s
divOutInPlace = RARORIPODE.divOutInPlace

-- | Outward rounded division assignment
(</>=) :: OpMutable1 DI s
(</>=) = (RARORIPODE.</>=)

-- | Inward rounded in-place mixed addition
mixedAddInInPlace :: (RoundedMixedAddInPlace DI tn) => 
    OpMutableNonmut DI tn s
mixedAddInInPlace = RARORIPODE.mixedAddInInPlace

-- | Inward rounded additive scalar action assignment
(>+<|=) :: (RoundedMixedAddInPlace DI tn) => OpNonmut DI tn s
(>+<|=) = (RARORIPODE.>+<|=)

-- | Outward rounded in-place mixed addition
mixedAddOutInPlace :: (RoundedMixedAddInPlace DI tn) =>
    OpMutableNonmut DI tn s
mixedAddOutInPlace = RARORIPODE.mixedAddOutInPlace

-- | Outward rounded additive scalar action assignment
(<+>|=) :: (RoundedMixedAddInPlace DI tn) => OpNonmut DI tn s
(<+>|=) = (RARORIPODE.<+>|=)

-- | Inward rounded in-place mixed multiplication
mixedMultInInPlace :: (RoundedMixedMultiplyInPlace DI tn) => 
    OpMutableNonmut DI tn s
mixedMultInInPlace = RARORIPODE.mixedMultInInPlace

-- | Inward rounded multiplicative scalar action assignment
(>*<|=) :: (RoundedMixedMultiplyInPlace DI tn) => OpNonmut DI tn s
(>*<|=) = (RARORIPODE.>*<|=)

-- | Outward rounded in-place mixed multiplication
mixedMultOutInPlace :: (RoundedMixedMultiplyInPlace DI tn) => 
    OpMutableNonmut DI tn s
mixedMultOutInPlace = RARORIPODE.mixedMultOutInPlace

-- | Outward rounded multiplicative scalar action assignment
(<*>|=) :: (RoundedMixedMultiplyInPlace DI tn) => OpNonmut DI tn s
(<*>|=) = (RARORIPODE.<*>|=)

-- | Inward rounded in-place mixed reciprocal action
mixedDivInInPlace :: (RoundedMixedDivideInPlace DI tn) => 
    OpMutableNonmut DI tn s
mixedDivInInPlace = RARORIPODE.mixedDivInInPlace

-- | Inward rounded multiplicative scalar reciprocal action assignment
(>/<|=) :: (RoundedMixedDivideInPlace DI tn) => OpNonmut DI tn s
(>/<|=) = (RARORIPODE.>/<|=)

-- | Outward rounded in-place mixed reciprocal action
mixedDivOutInPlace :: (RoundedMixedDivideInPlace DI tn) => 
    OpMutableNonmut DI tn s
mixedDivOutInPlace = RARORIPODE.mixedDivOutInPlace

-- | Outward rounded multiplicative scalar reciprocal action assignment
(</>|=) :: (RoundedMixedDivideInPlace DI tn) => OpNonmut DI tn s
(</>|=) = (RARORIPODE.</>|=)

-- | Inward rounded in-place exponential
expInInPlace :: OpMutable1 DI s
expInInPlace = RARORIPODE.expInInPlace 

-- | Outward rounded in-place exponential
expOutInPlace :: OpMutable1 DI s
expOutInPlace = RARORIPODE.expOutInPlace 

-- | Inward rounded in-place square root
sqrtInInPlace :: OpMutable1 DI s
sqrtInInPlace = RARORIPODE.sqrtInInPlace 

-- | Outward rounded in-place square root
sqrtOutInPlace :: OpMutable1 DI s
sqrtOutInPlace = RARORIPODE.sqrtOutInPlace 
