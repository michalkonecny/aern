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
    (|<+>),(<+>|),(|<*>),(<*>|),(</>|),

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
    (|>+<),(>+<|),(|>*<),(>*<|),(>/<|),

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

import qualified Numeric.AERN.Basics.NumericOrder as BNO
  (least,greatest)

import qualified Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort as BNOODE
  ((==?),(<==>?),(</=>?),
   (<?),(>?),(<=?),(>=?),
   minOut,maxOut,minIn,maxIn)

import qualified Numeric.AERN.Basics.RefinementOrder as BRO
  (bottom,top,(⊥),(⊤))

import qualified Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort as BROODE
  ((|==?),(|<==>?),(|</=>?),
   (|<?),(|>?),(|<=?),(|>=?),(⊏?),(⊑?),(⊒?),(⊐?),
   (</\>),(<\/>),(<\/>?),(<⊓>),(<⊔>),(<⊔>?),
   (>/\<),(>\/<),(>⊓<),(>⊔<))

import Numeric.AERN.RealArithmetic.Interval()

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as RAROR
  (RoundedMixedAdd(..),RoundedMixedMultiply(..),RoundedMixedDivide(..))

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort as RARORODE
 ((<+>),(<->),(<*>),(</>),(|<+>),(<+>|),(|<*>),(<*>|),(</>|),
  piOut,eOut,absOut,expOut,sqrtOut,
  (>+<),(>-<),(>*<),(>/<),(|>+<),(>+<|),(|>*<),(>*<|),(>/<|),
  piIn,eIn,absIn,expIn,sqrtIn)
 
import qualified Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps as RAIEFFO
    (expOutIters, expInIters, sqrtOutIters, sqrtInIters)

import Numeric.AERN.RealArithmetic.Basis.Double()

import qualified Numeric.AERN.RealArithmetic.Interval.Double as RAID
 (width, bisect)

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

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
least = BNO.least

greatest :: RealIntervalApprox
greatest = BNO.greatest

-- | Partial equality
(==?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(==?) = (BNOODE.==?) 

-- | Partial `is comparable to`
(<==>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(<==>?) = (BNOODE.<==>?)

-- | Partial `is not comparable to`
(</=>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(</=>?) = (BNOODE.</=>?)

-- | Partial `strictly less than`
(<?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(<?) = (BNOODE.<?)

-- | Partial `strictly greater than`
(>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(>?) = (BNOODE.>?)

-- | Partial `less than or equal to`
(<=?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(<=?) = (BNOODE.<=?)

-- | Partial `greater than or equal to`
(>=?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(>=?) = (BNOODE.>=?)
 
-- | Outward rounded minimum
minOut :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
minOut = BNOODE.minOut

-- | Outward rounded maximum
maxOut :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
maxOut = BNOODE.maxOut

-- | Inward rounded minimum
minIn :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
minIn = BNOODE.minIn

-- | Inward rounded maximum
maxIn :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
maxIn = BNOODE.maxIn

bottom :: RealIntervalApprox
bottom = BRO.bottom

top :: RealIntervalApprox
top = BRO.top

-- | Convenience Unicode notation for 'bottom'
(⊥) :: RealIntervalApprox
(⊥) = (BRO.⊥)

-- | Convenience Unicode notation for 'top'
(⊤) :: RealIntervalApprox
(⊤) = (BRO.⊤)

-- | Partial equality
(|==?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|==?) = (BROODE.|==?)

-- | Partial `is comparable to`
(|<==>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|<==>?) = (BROODE.|<==>?)

-- | Partial `is not comparable to`
(|</=>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|</=>?) = (BROODE.|</=>?)

-- | Partial `strictly below`
(|<?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|<?) = (BROODE.|<?)

-- | Partial `strictly above`
(|>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|>?) = (BROODE.|>?)

-- | Partial `below or equal to`
(|<=?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|<=?) = (BROODE.|<=?)

-- | Partial `above or equal to`
(|>=?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(|>=?) = (BROODE.|>=?)

{-| Convenience Unicode notation for '|<?' -}
(⊏?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(⊏?) = (BROODE.⊏?)

{-| Convenience Unicode notation for '|<=?' -}
(⊑?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(⊑?) = (BROODE.⊑?)

{-| Convenience Unicode notation for '|>=?' -}
(⊒?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool
(⊒?) = (BROODE.⊒?)

{-| Convenience Unicode notation for '|>?' -}
(⊐?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe Bool 
(⊐?) = (BROODE.⊐?)

-- | Outward rounded meet
(</\>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(</\>) = (BROODE.</\>)

-- | Outward rounded join
(<\/>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<\/>) = (BROODE.<\/>)

-- | Inward rounded meet
(>/\<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>/\<) = (BROODE.>/\<)

-- | Inward rounded join
(>\/<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>\/<) = (BROODE.>\/<)

{-| Convenience Unicode notation for '</\>' -}
(<⊓>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<⊓>) = (BROODE.<⊓>)

{-| Convenience Unicode notation for '<\/>' -}
(<⊔>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<⊔>) = (BROODE.<⊔>)

{-| Convenience Unicode notation for '>/\<' -}
(>⊓<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>⊓<) = (BROODE.>⊓<)

{-| Convenience Unicode notation for '>\/<' -}
(>⊔<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>⊔<) = (BROODE.>⊔<)
 
-- | Partial outward rounded join
(<\/>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe RealIntervalApprox
(<\/>?) = (BROODE.<\/>?)

{-| Convenience Unicode notation for '<\/>?' -}
(<⊔>?) :: RealIntervalApprox -> RealIntervalApprox -> Maybe RealIntervalApprox 
(<⊔>?) = (BROODE.<⊔>?)

-- | Outward rounded addition
(<+>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<+>) = (RARORODE.<+>)

-- | Outward rounded subtraction
(<->) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<->) = (RARORODE.<->)

-- | Outward rounded multiplication
(<*>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(<*>) = (RARORODE.<*>)

-- | Outward rounded division
(</>) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(</>) = (RARORODE.</>)

-- | Inward rounded addition
(>+<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>+<) = (RARORODE.>+<)

-- | Inward rounded subtraction
(>-<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>-<) = (RARORODE.>-<)

-- | Inward rounded multiplication
(>*<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>*<) = (RARORODE.>*<)

-- | Inward rounded division
(>/<) :: RealIntervalApprox -> RealIntervalApprox -> RealIntervalApprox
(>/<) = (RARORODE.>/<)

-- | Outward rounded additive scalar left action
(|<+>) :: RAROR.RoundedMixedAdd RealIntervalApprox tn => tn -> RealIntervalApprox -> RealIntervalApprox
(|<+>) = (RARORODE.|<+>)

-- | Inward rounded additive scalar left action
(|>+<) :: RAROR.RoundedMixedAdd RealIntervalApprox tn => tn -> RealIntervalApprox -> RealIntervalApprox
(|>+<) = (RARORODE.|>+<)

-- | Outward rounded additive scalar right action
(<+>|) :: RAROR.RoundedMixedAdd RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox
(<+>|) = (RARORODE.<+>|)

-- | Inward rounded additive scalar right action
(>+<|) :: RAROR.RoundedMixedAdd RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox
(>+<|) = (RARORODE.>+<|)

-- | Outward rounded multiplicative scalar left action
(|<*>) :: RAROR.RoundedMixedMultiply RealIntervalApprox tn => tn -> RealIntervalApprox -> RealIntervalApprox
(|<*>) = (RARORODE.|<*>)

-- | Inward rounded multiplicative scalar left action
(|>*<) :: RAROR.RoundedMixedMultiply RealIntervalApprox tn => tn -> RealIntervalApprox -> RealIntervalApprox
(|>*<) = (RARORODE.|>*<)

-- | Outward rounded multiplicative scalar right action
(<*>|) :: RAROR.RoundedMixedMultiply RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox
(<*>|) = (RARORODE.<*>|)

-- | Inward rounded multiplicative scalar right action
(>*<|) :: RAROR.RoundedMixedMultiply RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox
(>*<|) = (RARORODE.>*<|)

-- | Outward rounded multiplicative scalar reciprocal right action
(</>|) :: RAROR.RoundedMixedDivide RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox
(</>|) = (RARORODE.</>|)

-- | Inward rounded multiplicative scalar reciprocal right action
(>/<|) :: RAROR.RoundedMixedDivide RealIntervalApprox tn => RealIntervalApprox -> tn -> RealIntervalApprox  
(>/<|) = (RARORODE.>/<|)

-- | Outward rounded pi
piOut :: RealIntervalApprox
piOut = RARORODE.piOut 

-- | Outward rounded e
eOut :: RealIntervalApprox
eOut = RARORODE.eOut 

-- | Inward rounded pi
piIn :: RealIntervalApprox
piIn = RARORODE.piIn 

-- | Inward rounded e
eIn :: RealIntervalApprox
eIn = RARORODE.eIn 

-- | Outward rounded absolute value
absOut :: RealIntervalApprox -> RealIntervalApprox
absOut = RARORODE.absOut

-- | Outward rounded exponential
expOut :: RealIntervalApprox -> RealIntervalApprox
expOut = RARORODE.expOut

-- | Outward rounded square root
sqrtOut :: RealIntervalApprox -> RealIntervalApprox
sqrtOut = RARORODE.sqrtOut

-- | Inward rounded absolute value
absIn :: RealIntervalApprox -> RealIntervalApprox
absIn = RARORODE.absIn

-- | Inward rounded exponential
expIn :: RealIntervalApprox -> RealIntervalApprox
expIn = RARORODE.expIn

-- | Inward rounded square root
sqrtIn :: RealIntervalApprox -> RealIntervalApprox
sqrtIn = RARORODE.sqrtIn

expOutIters :: Int -> RealIntervalApprox -> RealIntervalApprox
expOutIters = RAIEFFO.expOutIters

sqrtOutIters :: Int -> RealIntervalApprox -> RealIntervalApprox
sqrtOutIters = RAIEFFO.sqrtOutIters

expInIters :: Int -> RealIntervalApprox -> RealIntervalApprox
expInIters = RAIEFFO.expInIters

sqrtInIters :: Int -> RealIntervalApprox -> RealIntervalApprox
sqrtInIters = RAIEFFO.sqrtInIters

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
