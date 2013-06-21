{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.DoubleBasis.RealApprox
    Description :  Double intervals for approximating real numbers
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Intervals with Double endpoints as an
    abstract data type for approximating real numbers.
    Each interval represents a single real number.
    Only operations that respect this view are available
    via this module.
-}
module Numeric.AERN.DoubleBasis.RealApprox
(
    -- |
    -- A convenience module re-exporting various interval operations 
    -- with default effort indicators.

    -- * Main type
    RealApprox,
    
    -- * Order relations
    -- | 
    -- There are two types of order relations to consider: 
    -- 
    --   * the /numerical/ order, generalising the order relation
    --     on Doubles and
    -- 
    --   * the /refinement/ order, generalising the reverse-inclusion 
    --     relation on consistent intervals.  
    --
    -- The intervals in 'RealApprox' form a /meet/-semilattice
    -- corresponding to the refiniement order under the operation /\\ 
    -- returning the subset-least interval containing the /union/ of 
    -- its argument intervals.
    -- 
    -- A safe approximation of the exact operation /\\ 
    -- is given by '</\>'.
    --
    -- The dual operation to /\\ is partial 
    -- since the /intersection/ of disjoint sets is empty. 
    -- 
    -- A safe approximation of the exact operation \\/\?
    -- is given by '<\/>?'.

    -- ** Numerical order
    -- | 
    -- Interval extensions of the corresponding tests and relations on 
    -- Double.
    
    -- *** Extrema
    -- |
    -- The values retured by 'least' and 'greatest' complete the 
    -- numerical partial order on 'RealApprox'.
    least,greatest,
    
    -- *** Comparability tests
    (==?),(<==>?),(</=>?),
    
    -- *** Order relations
    (<?),(>?),(<=?),(>=?),
    
    -- ** Refinement order
    -- | 
    -- Tests and relations in the interval poset.
    
    -- *** Extrema
    -- |
    -- The values retured by 'bottom' and 'top' complete the 
    -- refinement partial order on 'RealApprox'.

    -- **** ASCII versions
    bottom,top,
    
    -- **** Unicode versions
    (⊥),(⊤),
    
    -- *** Comparability tests
    (|==?),(|<==>?),(|</=>?),

    -- *** Order relations
    
    -- **** ASCII versions
    (|<?),(|>?),(|<=?),(|>=?),
    
    -- **** Unicode versions
    (⊏?),(⊑?),(⊒?),(⊐?),
    
    -- * Outward rounded operations
    -- | 
    -- Interval extensions of common functions. The 'Num', 'Fractional' 
    -- and 'Floating' instances for 'RealApprox' use such versions as instance 
    -- methods.
    
    -- ** Order operations
    
    -- *** Numerical order
    -- | 
    -- Outward rounded interval extensions of the corresponding 
    -- operations on Double.
    minOut,maxOut,

    -- *** Refinement order
    -- | 
    -- Outward rounded lattice operations in the interval poset.
    
    -- **** ASCII versions
    (</\>),(<\/>?),

    -- **** Unicode versions
    (<⊓>),(<⊔>?),

    -- ** Field operations

    -- *** Interval operations
    (<+>),(<->),(<*>),(</>),

    -- *** Mixed type operations
    (|<+>),(<+>|),(|<*>),(<*>|),(</>|),(<^>),

    -- ** Special constants 
    piOut,eOut,
    
    -- ** Elementary functions
    absOut,expOut,sqrtOut,
    
    -- *** Elementary functions with iteration effort control
    -- |
    -- To be used eg as follows:
    -- 
    -- > expOutIters 10 x
    --
    -- which means that at most 10 iterations should be used while computing exp
    expOutIters,sqrtOutIters
)
where

import Numeric.AERN.Basics.Interval
  (Interval(..))

import qualified Numeric.AERN.NumericOrder as BNO
  (least,greatest,
   (==?),(<==>?),(</=>?),
   (<?),(>?),(<=?),(>=?),
   minOut,maxOut,minIn,maxIn)

import qualified Numeric.AERN.RefinementOrder as BRO
  (bottom,top,(⊥),(⊤),
   (|==?),(|<==>?),(|</=>?),
   (|<?),(|>?),(|<=?),(|>=?),(⊏?),(⊑?),(⊒?),(⊐?),
   (</\>),(<\/>?),(<⊓>),(<⊔>?))

import Numeric.AERN.RealArithmetic.Interval()

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as RAROR
  (RoundedMixedAdd(..),RoundedMixedMultiply(..),RoundedMixedDivide(..))

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort as RARORODE
 ((<+>),(<->),(<*>),(</>),(|<+>),(<+>|),(|<*>),(<*>|),(</>|),(<^>),
  piOut,eOut,absOut,expOut,sqrtOut)
 
import qualified Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps as RAIEFFO
  (expOutIters, sqrtOutIters)

import Numeric.AERN.RealArithmetic.Basis.Double()

import qualified Numeric.AERN.NumericOrder as NumOrd

import Test.QuickCheck

-- | 
-- Intervals with Double endpoints, presented as an abstract
-- data type for approximating real numbers.  One interval
-- represents a single real number contained in it.  Only
-- operations supporting this view are provided by this module. 
type RealApprox = Interval Double

sampleRealApprox :: RealApprox
sampleRealApprox = Interval 0 0

least :: RealApprox
least = BNO.least sampleRealApprox

greatest :: RealApprox
greatest = BNO.greatest sampleRealApprox

infix 4 ==?, <==>?, </=>?, <?, <=?, >=?, >?

-- | Partial equality
(==?) :: RealApprox -> RealApprox -> Maybe Bool
(==?) = (BNO.==?) 

-- | Partial `is comparable to`
(<==>?) :: RealApprox -> RealApprox -> Maybe Bool
(<==>?) = (BNO.<==>?)

-- | Partial `is not comparable to`
(</=>?) :: RealApprox -> RealApprox -> Maybe Bool
(</=>?) = (BNO.</=>?)

-- | Partial `strictly less than`
(<?) :: RealApprox -> RealApprox -> Maybe Bool
(<?) = (BNO.<?)

-- | Partial `strictly greater than`
(>?) :: RealApprox -> RealApprox -> Maybe Bool
(>?) = (BNO.>?)

-- | Partial `less than or equal to`
(<=?) :: RealApprox -> RealApprox -> Maybe Bool
(<=?) = (BNO.<=?)

-- | Partial `greater than or equal to`
(>=?) :: RealApprox -> RealApprox -> Maybe Bool
(>=?) = (BNO.>=?)
 
-- | Outward rounded minimum
minOut :: RealApprox -> RealApprox -> RealApprox
minOut = BNO.minOut

-- | Outward rounded maximum
maxOut :: RealApprox -> RealApprox -> RealApprox
maxOut = BNO.maxOut

-- | Inward rounded minimum
minIn :: RealApprox -> RealApprox -> RealApprox
minIn = BNO.minIn

-- | Inward rounded maximum
maxIn :: RealApprox -> RealApprox -> RealApprox
maxIn = BNO.maxIn

bottom :: RealApprox
bottom = BRO.bottom sampleRealApprox

top :: RealApprox
top = BRO.top sampleRealApprox

-- | Convenience Unicode notation for 'bottom'
(⊥) :: RealApprox
(⊥) = (BRO.⊥) sampleRealApprox

-- | Convenience Unicode notation for 'top'
(⊤) :: RealApprox
(⊤) = (BRO.⊤) sampleRealApprox

infix 4 |==?, |<==>?, |</=>?, |<?, |<=?, |>=?, |>?, ⊏?, ⊑?, ⊒?, ⊐?
infixr 3 </\>, <⊓> 
infixr 2 <\/>?, <⊔>?

-- | Partial equality
(|==?) :: RealApprox -> RealApprox -> Maybe Bool
(|==?) = (BRO.|==?)

-- | Partial `is comparable to`
(|<==>?) :: RealApprox -> RealApprox -> Maybe Bool
(|<==>?) = (BRO.|<==>?)

-- | Partial `is not comparable to`
(|</=>?) :: RealApprox -> RealApprox -> Maybe Bool
(|</=>?) = (BRO.|</=>?)

-- | Partial `strictly below`
(|<?) :: RealApprox -> RealApprox -> Maybe Bool
(|<?) = (BRO.|<?)

-- | Partial `strictly above`
(|>?) :: RealApprox -> RealApprox -> Maybe Bool
(|>?) = (BRO.|>?)

-- | Partial `below or equal to`
(|<=?) :: RealApprox -> RealApprox -> Maybe Bool
(|<=?) = (BRO.|<=?)

-- | Partial `above or equal to`
(|>=?) :: RealApprox -> RealApprox -> Maybe Bool
(|>=?) = (BRO.|>=?)

{-| Convenience Unicode notation for '|<?' -}
(⊏?) :: RealApprox -> RealApprox -> Maybe Bool
(⊏?) = (BRO.⊏?)

{-| Convenience Unicode notation for '|<=?' -}
(⊑?) :: RealApprox -> RealApprox -> Maybe Bool
(⊑?) = (BRO.⊑?)

{-| Convenience Unicode notation for '|>=?' -}
(⊒?) :: RealApprox -> RealApprox -> Maybe Bool
(⊒?) = (BRO.⊒?)

{-| Convenience Unicode notation for '|>?' -}
(⊐?) :: RealApprox -> RealApprox -> Maybe Bool 
(⊐?) = (BRO.⊐?)

-- | Outward rounded meet
(</\>) :: RealApprox -> RealApprox -> RealApprox
(</\>) = (BRO.</\>)

{-| Convenience Unicode notation for '</\>' -}
(<⊓>) :: RealApprox -> RealApprox -> RealApprox
(<⊓>) = (BRO.<⊓>)

-- | Partial outward rounded join
(<\/>?) :: RealApprox -> RealApprox -> Maybe RealApprox
(<\/>?) = (BRO.<\/>?)

{-| Convenience Unicode notation for '<\/>?' -}
(<⊔>?) :: RealApprox -> RealApprox -> Maybe RealApprox 
(<⊔>?) = (BRO.<⊔>?)

infixl 6 <+>, <->
infixl 7 <*>
infixl 8 <^>
infixl 7 </>

infixr 6 |<+>
infixl 6 <+>|
infixr 7 |<*>
infixl 7 <*>|
infixl 7 </>|

-- | Outward rounded addition
(<+>) :: RealApprox -> RealApprox -> RealApprox
(<+>) = (RARORODE.<+>)

-- | Outward rounded subtraction
(<->) :: RealApprox -> RealApprox -> RealApprox
(<->) = (RARORODE.<->)

-- | Outward rounded multiplication
(<*>) :: RealApprox -> RealApprox -> RealApprox
(<*>) = (RARORODE.<*>)

-- | Outward rounded division
(</>) :: RealApprox -> RealApprox -> RealApprox
(</>) = (RARORODE.</>)

-- | Outward rounded additive scalar left action
(|<+>) :: RAROR.RoundedMixedAdd RealApprox tn => tn -> RealApprox -> RealApprox
(|<+>) = (RARORODE.|<+>)

-- | Outward rounded additive scalar right action
(<+>|) :: RAROR.RoundedMixedAdd RealApprox tn => RealApprox -> tn -> RealApprox
(<+>|) = (RARORODE.<+>|)

-- | Outward rounded multiplicative scalar left action
(|<*>) :: RAROR.RoundedMixedMultiply RealApprox tn => tn -> RealApprox -> RealApprox
(|<*>) = (RARORODE.|<*>)

-- | Outward rounded multiplicative scalar right action
(<*>|) :: RAROR.RoundedMixedMultiply RealApprox tn => RealApprox -> tn -> RealApprox
(<*>|) = (RARORODE.<*>|)

-- | Outward rounded multiplicative scalar reciprocal right action
(</>|) :: RAROR.RoundedMixedDivide RealApprox tn => RealApprox -> tn -> RealApprox
(</>|) = (RARORODE.</>|)

-- | Outward rounded power
(<^>) :: RealApprox -> Int -> RealApprox
(<^>) = (RARORODE.<^>)

-- | Outward rounded pi
piOut :: RealApprox
piOut = RARORODE.piOut 

-- | Outward rounded e
eOut :: RealApprox
eOut = RARORODE.eOut 

-- | Outward rounded absolute value
absOut :: RealApprox -> RealApprox
absOut = RARORODE.absOut

-- | Outward rounded exponential
expOut :: RealApprox -> RealApprox
expOut = RARORODE.expOut

-- | Outward rounded square root
sqrtOut :: RealApprox -> RealApprox
sqrtOut = RARORODE.sqrtOut

expOutIters :: Int -> RealApprox -> RealApprox
expOutIters = RAIEFFO.expOutIters

sqrtOutIters :: Int -> RealApprox -> RealApprox
sqrtOutIters = RAIEFFO.sqrtOutIters

newtype PositiveRealApprox = 
    PositiveRealApprox { unPositiveRealApprox :: RealApprox }

instance Show PositiveRealApprox where
    show (PositiveRealApprox i) = show i

instance Arbitrary PositiveRealApprox
    where
    arbitrary =
        do
        NumOrd.UniformlyOrderedPair (l,h) <- arbitrary
        return $ PositiveRealApprox (Interval (pos l) (pos h))
        where
        pos e 
            | e > 0 =  e
            | e == 0 =  1
            | otherwise = (-e) 
