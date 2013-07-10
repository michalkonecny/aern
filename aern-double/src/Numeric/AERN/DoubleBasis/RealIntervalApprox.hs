{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.DoubleBasis.RealIntervalApprox
    Description :  Double intervals for approximating real intervals
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Intervals with Double endpoints as an
    abstract data type for approximating real number intervals.
    Each interval is either an outer or inner approximation
    of another interval. 
    Only operations that respect this view are available
    via this module.
-}
module Numeric.AERN.DoubleBasis.RealIntervalApprox
(
    -- |
    -- A convenience module re-exporting various interval operations 
    -- with default effort indicators.

    -- * Main type
    RealIntervalApprox,
    
    -- ** associated operations
    width, bisect,

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
    -- The consistent intervals in 'RealIntevalApprox' form a /meet/-semilattice
    -- corresponding to the refiniement order under the operation /\\ 
    -- returning the subset-least interval containing the /union/ of 
    -- its argument intervals. The operation is extended to all of 'RealIntevalApprox'
    -- by returning the highest interval below both of its argument 
    -- intervals.  
    -- 
    -- The structure ({ 'di' | 'di' is consistent \}, /\\, 
    -- 'bottom') is a complete meet-semilattice.
    -- 
    -- Lower and upper approximations of the exact operation /\\ 
    -- are given by '</\>' and '>/\<' respectively.
    --
    -- The dual operation to /\\ is partial on consistent intervals, 
    -- since the /intersection/ of disjoint sets is empty. Therefore,
    -- the /join/-semilattice structure on 'RealIntevalApprox' comes in two flavours:
    --
    --   * the partial consistent interval-valued join \\/\? which 
    --     returns 'Nothing' for disjoint and anticonsistent arguments
    --     and
    --
    --   * the total join \\/ which returns the lowest interval in
    --     'RealIntevalApprox' above both of its argument intervals. 
    -- 
    -- The structure ('RealIntevalApprox', \/\\, \\\/, 'bottom', 'top') is a complete 
    -- lattice.
    --
    -- Lower and upper approximations of the exact operations \\/\?
    -- and \\\/ are given by '<\/>?', '<\/>' and '>\/<' respectively.

    -- ** Numerical order
    -- | 
    -- Interval extensions of the corresponding tests and relations on 
    -- Double.
    
    -- *** Extrema
    -- |
    -- The values retured by 'least' and 'greatest' complete the 
    -- numerical partial order on 'RealIntevalApprox'.
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
    -- refinement partial order on 'RealIntevalApprox'.

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
    -- and 'Floating' instances for 'RealIntevalApprox' use such versions as instance 
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
    (</\>),(<\/>),(<\/>?),

    -- **** Unicode versions
    (<⊓>),(<⊔>),(<⊔>?),

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
    expOutIters,sqrtOutIters,

    -- * Inward rounded operations 

    -- ** Order operations

    -- *** Numerical order
    -- | 
    -- Inward rounded interval extensions of the corresponding 
    -- operations on Double.
    minIn,maxIn,

    -- *** Refinement order

    -- **** ASCII versions
    (>/\<),(>\/<),

    -- **** Unicode versions
    (>⊓<),(>⊔<),

    -- ** Field operations
    
    -- *** Interval operations
    (>+<),(>-<),(>*<),(>/<),
    
    -- *** Mixed type operations
    (|>+<),(>+<|),(|>*<),(>*<|),(>/<|),(>^<),

    -- ** Special constants 
    piIn,eIn,
    
    -- ** Elementary functions
    absIn,expIn,sqrtIn,

    -- *** Elementary functions with iteration effort control
    -- |
    -- To be used eg as follows:
    -- 
    -- > expInIters 10 x
    --
    -- which means that at most 10 iterations should be used while computing exp
    expInIters,sqrtInIters
)
where

import Numeric.AERN.Basics.Interval
  (Interval(..))

import qualified Numeric.AERN.Basics.Interval as BI
  (getEndpoints,fromEndpoints)

import qualified Numeric.AERN.NumericOrder as BNO
  (least,greatest,
   (==?),(<==>?),(</=>?),
   (<?),(>?),(<=?),(>=?),
   minOut,maxOut,minIn,maxIn)

import qualified Numeric.AERN.RefinementOrder as BRO
  (bottom,top,(⊥),(⊤),
   (|==?),(|<==>?),(|</=>?),
   (|<?),(|>?),(|<=?),(|>=?),(⊏?),(⊑?),(⊒?),(⊐?),
   (</\>),(<\/>),(<\/>?),(<⊓>),(<⊔>),(<⊔>?),
   (>/\<),(>\/<),(>\/<?),(>⊓<),(>⊔<),(>⊔<?))

import Numeric.AERN.RealArithmetic.Interval()

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as RAROR
  (RoundedMixedAdd(..),RoundedMixedMultiply(..),RoundedMixedDivide(..),
   (<+>),(<->),(<*>),(</>),(|<+>),(<+>|),(|<*>),(<*>|),(</>|),(<^>),
   piOut,eOut,absOut,expOut,sqrtOut,
   (>+<),(>-<),(>*<),(>/<),(|>+<),(>+<|),(|>*<),(>*<|),(>/<|),(>^<),
   piIn,eIn,absIn,expIn,sqrtIn
  )
 
import qualified Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps as RAIEFFO
    (intervalExpOutIters, intervalExpInIters, intervalSqrtOutIters, intervalSqrtInIters)

import Numeric.AERN.RealArithmetic.Basis.Double()

import qualified Numeric.AERN.RealArithmetic.Interval.Double as RAID
 (width, bisect)

import qualified Numeric.AERN.NumericOrder as NumOrd

import Test.QuickCheck

-- | 
-- Intervals with Double endpoints, presented as an abstract
-- data type for approximating real number intervals from outside
-- or from inside. 
-- 
-- Note that ('l','r') = 'getEndpoints' ('di' :: 'RealIntervalApprox') does not 
-- fix an ordering of 'l' and 'r'. 
-- 
--   * 'di' is called /consistent/ when 'l' '<=' 'r'
--
--   * 'di' is called /anticonsistent/ when 'r' '<=' 'l' 
--
-- A consistent interval 'di' may be identified with the set defined by
-- \{ 'x' | 'l' '<=' 'x' and 'x' '<=' 'r' \}.
type RealIntervalApprox = Interval Double

-- | Given an argument interval 'i' 'getEndpoints' returns the endpoint pair 
--   ('leftEndpoint' 'i','rightEndpoint' 'i').
getEndpoints :: RealIntervalApprox -> (Double, Double)
getEndpoints = BI.getEndpoints

-- | Constructs an interval from an endpoint pair.
fromEndpoints :: (Double, Double) -> RealIntervalApprox
fromEndpoints = BI.fromEndpoints

sampleRealIntervalApprox :: RealIntervalApprox
sampleRealIntervalApprox = Interval 0 0

{-|
    Calculate the width of an interval.
    The result may not be thin since the width calculation uses outwards rounding.
-}
width :: RealIntervalApprox -> RealIntervalApprox
width = RAID.width

{-|
    Split an interval into two subintervals that share only one point 
    and whose union is the original interval.
-}
bisect ::
    Maybe Double {-^ optional parameter, indicating where to split the interval -} -> 
    RealIntervalApprox {-^ the interval to split -} -> 
    (RealIntervalApprox, RealIntervalApprox)
bisect = RAID.bisect

least :: RealIntervalApprox
least = BNO.least sampleRealIntervalApprox

greatest :: RealIntervalApprox
greatest = BNO.greatest sampleRealIntervalApprox

infix 4 ==?, <==>?, </=>?, <?, <=?, >=?, >?

-- | Partial equality
(==?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(==?) = (BNO.==?) 

-- | Partial `is comparable to`
(<==>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(<==>?) = (BNO.<==>?)

-- | Partial `is not comparable to`
(</=>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(</=>?) = (BNO.</=>?)

-- | Partial `strictly less than`
(<?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(<?) = (BNO.<?)

-- | Partial `strictly greater than`
(>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(>?) = (BNO.>?)

-- | Partial `less than or equal to`
(<=?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(<=?) = (BNO.<=?)

-- | Partial `greater than or equal to`
(>=?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(>=?) = (BNO.>=?)
 
-- | Outward rounded minimum
minOut :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
minOut = BNO.minOut

-- | Outward rounded maximum
maxOut :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
maxOut = BNO.maxOut

-- | Inward rounded minimum
minIn :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
minIn = BNO.minIn

-- | Inward rounded maximum
maxIn :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
maxIn = BNO.maxIn

bottom :: RealIntervalApprox
bottom = BRO.bottom sampleRealIntervalApprox

top :: RealIntervalApprox
top = BRO.top sampleRealIntervalApprox

-- | Convenience Unicode notation for 'bottom'
(⊥) :: RealIntervalApprox
(⊥) = (BRO.⊥) sampleRealIntervalApprox

-- | Convenience Unicode notation for 'top'
(⊤) :: RealIntervalApprox
(⊤) = (BRO.⊤) sampleRealIntervalApprox

infix 4 |==?, |<==>?, |</=>?, |<?, |<=?, |>=?, |>?, ⊏?, ⊑?, ⊒?, ⊐?
infixr 3 </\>, >/\<, <⊓>, >⊓< 
infixr 2 <\/>?, <\/>, >\/<, <⊔>?, <⊔>, >⊔< 

-- | Partial equality
(|==?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|==?) = (BRO.|==?)

-- | Partial `is comparable to`
(|<==>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|<==>?) = (BRO.|<==>?)

-- | Partial `is not comparable to`
(|</=>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|</=>?) = (BRO.|</=>?)

-- | Partial `strictly below`
(|<?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|<?) = (BRO.|<?)

-- | Partial `strictly above`
(|>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|>?) = (BRO.|>?)

-- | Partial `below or equal to`
(|<=?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|<=?) = (BRO.|<=?)

-- | Partial `above or equal to`
(|>=?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|>=?) = (BRO.|>=?)

{-| Convenience Unicode notation for '|<?' -}
(⊏?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(⊏?) = (BRO.⊏?)

{-| Convenience Unicode notation for '|<=?' -}
(⊑?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(⊑?) = (BRO.⊑?)

{-| Convenience Unicode notation for '|>=?' -}
(⊒?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(⊒?) = (BRO.⊒?)

{-| Convenience Unicode notation for '|>?' -}
(⊐?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool 
(⊐?) = (BRO.⊐?)

-- | Outward rounded meet
(</\>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(</\>) = (BRO.</\>)

-- | Outward rounded join
(<\/>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<\/>) = (BRO.<\/>)

-- | Inward rounded meet
(>/\<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>/\<) = (BRO.>/\<)

-- | Inward rounded join
(>\/<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>\/<) = (BRO.>\/<)

{-| Convenience Unicode notation for '</\>' -}
(<⊓>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<⊓>) = (BRO.<⊓>)

{-| Convenience Unicode notation for '<\/>' -}
(<⊔>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<⊔>) = (BRO.<⊔>)

{-| Convenience Unicode notation for '>/\<' -}
(>⊓<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>⊓<) = (BRO.>⊓<)

{-| Convenience Unicode notation for '>\/<' -}
(>⊔<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>⊔<) = (BRO.>⊔<)
 
-- | Partial outward rounded join
(<\/>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe RealIntervalApprox
(<\/>?) = (BRO.<\/>?)

{-| Convenience Unicode notation for '<\/>?' -}
(<⊔>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe RealIntervalApprox 
(<⊔>?) = (BRO.<⊔>?)

-- | Partial outward rounded join
(>\/<?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe RealIntervalApprox
(>\/<?) = (BRO.>\/<?)

{-| Convenience Unicode notation for '>\/<?' -}
(>⊔<?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe RealIntervalApprox
(>⊔<?) = (BRO.>⊔<?)


infixl 6 <+>, >+<, <->, >-<
infixl 7 <*>, >*<
infixl 8 <^>, >^<
infixl 7 </>, >/<

infixr 6 |<+>, |>+<
infixl 6 <+>|, >+<|
infixr 7 |<*>, |>*<
infixl 7 <*>|, >*<|
infixl 7 </>|, >/<|

-- | Outward rounded addition
(<+>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<+>) = (RAROR.<+>)

-- | Outward rounded subtraction
(<->) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<->) = (RAROR.<->)

-- | Outward rounded multiplication
(<*>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<*>) = (RAROR.<*>)

-- | Outward rounded division
(</>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(</>) = (RAROR.</>)

-- | Inward rounded addition
(>+<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>+<) = (RAROR.>+<)

-- | Inward rounded subtraction
(>-<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>-<) = (RAROR.>-<)

-- | Inward rounded multiplication
(>*<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>*<) = (RAROR.>*<)

-- | Inward rounded division
(>/<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>/<) = (RAROR.>/<)

-- | Outward rounded additive scalar left action
(|<+>) :: RAROR.RoundedMixedAdd RealIntervalApprox tn => tn -> RealIntervalApprox -> RealIntervalApprox
(|<+>) = (RAROR.|<+>)

-- | Inward rounded additive scalar left action
(|>+<) :: RAROR.RoundedMixedAdd RealIntervalApprox tn => tn -> RealIntervalApprox -> RealIntervalApprox
(|>+<) = (RAROR.|>+<)

-- | Outward rounded additive scalar right action
(<+>|) :: RAROR.RoundedMixedAdd RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox
(<+>|) = (RAROR.<+>|)

-- | Inward rounded additive scalar right action
(>+<|) :: RAROR.RoundedMixedAdd RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox
(>+<|) = (RAROR.>+<|)

-- | Outward rounded multiplicative scalar left action
(|<*>) :: RAROR.RoundedMixedMultiply RealIntervalApprox tn => tn -> RealIntervalApprox -> RealIntervalApprox
(|<*>) = (RAROR.|<*>)

-- | Inward rounded multiplicative scalar left action
(|>*<) :: RAROR.RoundedMixedMultiply RealIntervalApprox tn => tn -> RealIntervalApprox -> RealIntervalApprox
(|>*<) = (RAROR.|>*<)

-- | Outward rounded multiplicative scalar right action
(<*>|) :: RAROR.RoundedMixedMultiply RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox
(<*>|) = (RAROR.<*>|)

-- | Inward rounded multiplicative scalar right action
(>*<|) :: RAROR.RoundedMixedMultiply RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox
(>*<|) = (RAROR.>*<|)

-- | Outward rounded multiplicative scalar reciprocal right action
(</>|) :: RAROR.RoundedMixedDivide RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox
(</>|) = (RAROR.</>|)

-- | Inward rounded multiplicative scalar reciprocal right action
(>/<|) :: RAROR.RoundedMixedDivide RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox  
(>/<|) = (RAROR.>/<|)

-- | Outward rounded power
(<^>) :: RealIntervalApprox -> Int -> RealIntervalApprox
(<^>) = (RAROR.<^>)

-- | Inward rounded power
(>^<) :: RealIntervalApprox -> Int -> RealIntervalApprox
(>^<) = (RAROR.>^<)

-- | Outward rounded pi
piOut :: RealIntervalApprox
piOut = RAROR.piOut 0

-- | Outward rounded e
eOut :: RealIntervalApprox
eOut = RAROR.eOut 0

-- | Inward rounded pi
piIn :: RealIntervalApprox
piIn = RAROR.piIn 0

-- | Inward rounded e
eIn :: RealIntervalApprox
eIn = RAROR.eIn 0

-- | Outward rounded absolute value
absOut :: RealIntervalApprox -> RealIntervalApprox
absOut = RAROR.absOut

-- | Outward rounded exponential
expOut :: RealIntervalApprox -> RealIntervalApprox
expOut = RAROR.expOut

-- | Outward rounded square root
sqrtOut :: RealIntervalApprox -> RealIntervalApprox
sqrtOut = RAROR.sqrtOut

-- | Inward rounded absolute value
absIn :: RealIntervalApprox -> RealIntervalApprox
absIn = RAROR.absIn

-- | Inward rounded exponential
expIn :: RealIntervalApprox -> RealIntervalApprox
expIn = RAROR.expIn

-- | Inward rounded square root
sqrtIn :: RealIntervalApprox -> RealIntervalApprox
sqrtIn = RAROR.sqrtIn

expOutIters :: Int -> RealIntervalApprox -> RealIntervalApprox
expOutIters = RAIEFFO.intervalExpOutIters

sqrtOutIters :: Int -> RealIntervalApprox -> RealIntervalApprox
sqrtOutIters = RAIEFFO.intervalSqrtOutIters

expInIters :: Int -> RealIntervalApprox -> RealIntervalApprox
expInIters = RAIEFFO.intervalExpInIters

sqrtInIters :: Int -> RealIntervalApprox -> RealIntervalApprox
sqrtInIters = RAIEFFO.intervalSqrtInIters

newtype PositiveRealIntervalApprox = 
    PositiveRealIntervalApprox 
    { unPositiveRealIntervalApprox :: RealIntervalApprox }

instance Show PositiveRealIntervalApprox where
    show (PositiveRealIntervalApprox i) = show i

instance Arbitrary PositiveRealIntervalApprox
    where
    arbitrary =
        do
        NumOrd.UniformlyOrderedPair (l,h) <- arbitrary
        return $ PositiveRealIntervalApprox (Interval (pos l) (pos h))
        where
        pos e 
            | e > 0 =  e
            | e == 0 =  1
            | otherwise = (-e) 
