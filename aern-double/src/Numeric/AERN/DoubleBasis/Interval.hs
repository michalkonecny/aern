{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.DoubleBasis.Interval
    Description :  Interval Double type and operations  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Intervals with Double endpoints.
-}
module Numeric.AERN.DoubleBasis.Interval
(
    -- |
    -- A convenience module re-exporting various interval operations 
    -- with default effort indicators.

    -- * Main type
    DI,
    
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
    -- The consistent intervals in 'DI' form a /meet/-semilattice
    -- corresponding to the refiniement order under the operation /\\ 
    -- returning the subset-least interval containing the /union/ of 
    -- its argument intervals. The operation is extended to all of 'DI'
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
    -- the /join/-semilattice structure on 'DI' comes in two flavours:
    --
    --   * the partial consistent interval-valued join \\/\? which 
    --     returns 'Nothing' for disjoint and anticonsistent arguments
    --     and
    --
    --   * the total join \\/ which returns the lowest interval in
    --     'DI' above both of its argument intervals. 
    -- 
    -- The structure ('DI', \/\\, \\\/, 'bottom', 'top') is a complete 
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
    -- numerical partial order on 'DI'.
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
    -- refinement partial order on 'DI'.

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
    -- and 'Floating' instances for 'DI' use such versions as instance 
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
    
    -- * Low level facilities
    
    -- ** Access functions
    getEndpoints,fromEndpoints,

    -- ** Base type
    Interval(..),
)
where

import Numeric.AERN.Basics.Interval
  (Interval(..))

import qualified Numeric.AERN.Basics.Interval as BI
  (getEndpoints,fromEndpoints)

import qualified Numeric.AERN.NumericOrder as BNO
  (least,greatest,minOut,maxOut,minIn,maxIn,
   (==?),(<==>?),(</=>?),
   (<?),(>?),(<=?),(>=?))

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
   piIn,eIn,absIn,expIn,sqrtIn)
 
import Numeric.AERN.RealArithmetic.Basis.Double()

import Numeric.AERN.RealArithmetic.Interval.Double(width, bisect)

import qualified Numeric.AERN.NumericOrder as NumOrd

import Test.QuickCheck

infix 4 ==?, <==>?, </=>?, <?, <=?, >=?, >?

infix 4 |==?, |<==>?, |</=>?, |<?, |<=?, |>=?, |>?, ⊏?, ⊑?, ⊒?, ⊐?
infixr 3 </\>, >/\<, <⊓>, >⊓< 
infixr 2 <\/>?, <\/>, >\/<, <⊔>?, <⊔>, >⊔< 

infixl 6 <+>, >+<, <->, >-<
infixl 7 <*>, >*<
infixl 8 <^>, >^<
infixl 7 </>, >/<

infixr 6 |<+>, |>+<
infixl 6 <+>|, >+<|
infixr 7 |<*>, |>*<
infixl 7 <*>|, >*<|
infixl 7 </>|, >/<|

-- | 
-- Intervals with Double endpoints. 
-- 
-- Note that ('l','r') = 'getEndpoints' ('di' :: 'DI') does not 
-- fix an ordering of 'l' and 'r'. 
-- 
--   * 'di' is called /consistent/ when 'l' '<=' 'r'
--
--   * 'di' is called /anticonsistent/ when 'r' '<=' 'l' 
--
-- A consistent interval 'di' may be identified with the set defined by
-- \{ 'x' | 'l' '<=' 'x' and 'x' '<=' 'r' \}.
type DI = Interval Double

-- | Given an argument interval 'i' 'getEndpoints' returns the endpoint pair 
--   ('leftEndpoint' 'i','rightEndpoint' 'i').
getEndpoints :: DI -> (Double, Double)
getEndpoints = BI.getEndpoints

-- | Constructs an interval from an endpoint pair.
fromEndpoints :: (Double, Double) -> DI
fromEndpoints = BI.fromEndpoints

sampleDI :: DI
sampleDI = Interval 0 0

least :: DI
least = BNO.least sampleDI

greatest :: DI
greatest = BNO.greatest sampleDI

-- | Partial equality
(==?) :: DI -> DI -> Maybe Bool
(==?) = (BNO.==?) 

-- | Partial `is comparable to`
(<==>?) :: DI -> DI -> Maybe Bool
(<==>?) = (BNO.<==>?)

-- | Partial `is not comparable to`
(</=>?) :: DI -> DI -> Maybe Bool
(</=>?) = (BNO.</=>?)

-- | Partial `strictly less than`
(<?) :: DI -> DI -> Maybe Bool
(<?) = (BNO.<?)

-- | Partial `strictly greater than`
(>?) :: DI -> DI -> Maybe Bool
(>?) = (BNO.>?)

-- | Partial `less than or equal to`
(<=?) :: DI -> DI -> Maybe Bool
(<=?) = (BNO.<=?)

-- | Partial `greater than or equal to`
(>=?) :: DI -> DI -> Maybe Bool
(>=?) = (BNO.>=?)
 
-- | Outward rounded minimum
minOut :: DI -> DI -> DI
minOut = BNO.minOut

-- | Outward rounded maximum
maxOut :: DI -> DI -> DI
maxOut = BNO.maxOut

-- | Inward rounded minimum
minIn :: DI -> DI -> DI
minIn = BNO.minIn

-- | Inward rounded maximum
maxIn :: DI -> DI -> DI
maxIn = BNO.maxIn

bottom :: DI
bottom = BRO.bottom sampleDI

top :: DI
top = BRO.top sampleDI

-- | Convenience Unicode notation for 'bottom'
(⊥) :: DI
(⊥) = (BRO.⊥) sampleDI

-- | Convenience Unicode notation for 'top'
(⊤) :: DI
(⊤) = (BRO.⊤) sampleDI

-- | Partial equality
(|==?) :: DI -> DI -> Maybe Bool
(|==?) = (BRO.|==?)

-- | Partial `is comparable to`
(|<==>?) :: DI -> DI -> Maybe Bool
(|<==>?) = (BRO.|<==>?)

-- | Partial `is not comparable to`
(|</=>?) :: DI -> DI -> Maybe Bool
(|</=>?) = (BRO.|</=>?)

-- | Partial `strictly below`
(|<?) :: DI -> DI -> Maybe Bool
(|<?) = (BRO.|<?)

-- | Partial `strictly above`
(|>?) :: DI -> DI -> Maybe Bool
(|>?) = (BRO.|>?)

-- | Partial `below or equal to`
(|<=?) :: DI -> DI -> Maybe Bool
(|<=?) = (BRO.|<=?)

-- | Partial `above or equal to`
(|>=?) :: DI -> DI -> Maybe Bool
(|>=?) = (BRO.|>=?)

{-| Convenience Unicode notation for '|<?' -}
(⊏?) :: DI -> DI -> Maybe Bool
(⊏?) = (BRO.⊏?)

{-| Convenience Unicode notation for '|<=?' -}
(⊑?) :: DI -> DI -> Maybe Bool
(⊑?) = (BRO.⊑?)

{-| Convenience Unicode notation for '|>=?' -}
(⊒?) :: DI -> DI -> Maybe Bool
(⊒?) = (BRO.⊒?)

{-| Convenience Unicode notation for '|>?' -}
(⊐?) :: DI -> DI -> Maybe Bool 
(⊐?) = (BRO.⊐?)

-- | Outward rounded meet
(</\>) :: DI -> DI -> DI
(</\>) = (BRO.</\>)

-- | Outward rounded join
(<\/>) :: DI -> DI -> DI
(<\/>) = (BRO.<\/>)

-- | Inward rounded meet
(>/\<) :: DI -> DI -> DI
(>/\<) = (BRO.>/\<)

-- | Inward rounded join
(>\/<) :: DI -> DI -> DI
(>\/<) = (BRO.>\/<)

{-| Convenience Unicode notation for '</\>' -}
(<⊓>) :: DI -> DI -> DI
(<⊓>) = (BRO.<⊓>)

{-| Convenience Unicode notation for '<\/>' -}
(<⊔>) :: DI -> DI -> DI
(<⊔>) = (BRO.<⊔>)

{-| Convenience Unicode notation for '>/\<' -}
(>⊓<) :: DI -> DI -> DI
(>⊓<) = (BRO.>⊓<)

{-| Convenience Unicode notation for '>\/<' -}
(>⊔<) :: DI -> DI -> DI
(>⊔<) = (BRO.>⊔<)
 
-- | Partial outward rounded join
(<\/>?) :: DI -> DI -> Maybe DI
(<\/>?) = (BRO.<\/>?)

-- | Partial outward rounded join
(>\/<?) :: DI -> DI -> Maybe DI
(>\/<?) = (BRO.>\/<?)

{-| Convenience Unicode notation for '<\/>?' -}
(<⊔>?) :: DI -> DI -> Maybe DI 
(<⊔>?) = (BRO.<⊔>?)

{-| Convenience Unicode notation for '>\/<?' -}
(>⊔<?) :: DI -> DI -> Maybe DI 
(>⊔<?) = (BRO.>⊔<?)

-- | Outward rounded addition
(<+>) :: DI -> DI -> DI
(<+>) = (RAROR.<+>)

-- | Outward rounded subtraction
(<->) :: DI -> DI -> DI
(<->) = (RAROR.<->)

-- | Outward rounded multiplication
(<*>) :: DI -> DI -> DI
(<*>) = (RAROR.<*>)

-- | Outward rounded division
(</>) :: DI -> DI -> DI
(</>) = (RAROR.</>)

-- | Inward rounded addition
(>+<) :: DI -> DI -> DI
(>+<) = (RAROR.>+<)

-- | Inward rounded subtraction
(>-<) :: DI -> DI -> DI
(>-<) = (RAROR.>-<)

-- | Inward rounded multiplication
(>*<) :: DI -> DI -> DI
(>*<) = (RAROR.>*<)

-- | Inward rounded division
(>/<) :: DI -> DI -> DI
(>/<) = (RAROR.>/<)

-- | Outward rounded additive scalar left action
(|<+>) :: RAROR.RoundedMixedAdd DI tn => tn -> DI -> DI
(|<+>) = (RAROR.|<+>)

-- | Inward rounded additive scalar left action
(|>+<) :: RAROR.RoundedMixedAdd DI tn => tn -> DI -> DI
(|>+<) = (RAROR.|>+<)

-- | Outward rounded additive scalar right action
(<+>|) :: RAROR.RoundedMixedAdd DI tn => DI -> tn -> DI
(<+>|) = (RAROR.<+>|)

-- | Inward rounded additive scalar right action
(>+<|) :: RAROR.RoundedMixedAdd DI tn => DI -> tn -> DI
(>+<|) = (RAROR.>+<|)

-- | Outward rounded multiplicative scalar left action
(|<*>) :: RAROR.RoundedMixedMultiply DI tn => tn -> DI -> DI
(|<*>) = (RAROR.|<*>)

-- | Inward rounded multiplicative scalar left action
(|>*<) :: RAROR.RoundedMixedMultiply DI tn => tn -> DI -> DI
(|>*<) = (RAROR.|>*<)

-- | Outward rounded multiplicative scalar right action
(<*>|) :: RAROR.RoundedMixedMultiply DI tn => DI -> tn -> DI
(<*>|) = (RAROR.<*>|)

-- | Inward rounded multiplicative scalar right action
(>*<|) :: RAROR.RoundedMixedMultiply DI tn => DI -> tn -> DI
(>*<|) = (RAROR.>*<|)

-- | Outward rounded multiplicative scalar reciprocal right action
(</>|) :: RAROR.RoundedMixedDivide DI tn => DI -> tn -> DI
(</>|) = (RAROR.</>|)

-- | Inward rounded multiplicative scalar reciprocal right action
(>/<|) :: RAROR.RoundedMixedDivide DI tn => DI -> tn -> DI  
(>/<|) = (RAROR.>/<|)

-- | Outward rounded power
(<^>) :: DI -> Int -> DI
(<^>) = (RAROR.<^>)

-- | Inward rounded power
(>^<) :: DI -> Int -> DI
(>^<) = (RAROR.>^<)

-- | Outward rounded pi
piOut :: DI
piOut = RAROR.piOut 0

-- | Outward rounded e
eOut :: DI
eOut = RAROR.eOut 0

-- | Inward rounded pi
piIn :: DI
piIn = RAROR.piIn 0

-- | Inward rounded e
eIn :: DI
eIn = RAROR.eIn 0

-- | Outward rounded absolute value
absOut :: DI -> DI
absOut = RAROR.absOut

-- | Outward rounded exponential
expOut :: DI -> DI
expOut = RAROR.expOut

-- | Outward rounded square root
sqrtOut :: DI -> DI
sqrtOut = RAROR.sqrtOut

-- | Inward rounded absolute value
absIn :: DI -> DI
absIn = RAROR.absIn

-- | Inward rounded exponential
expIn :: DI -> DI
expIn = RAROR.expIn

-- | Inward rounded square root
sqrtIn :: DI -> DI
sqrtIn = RAROR.sqrtIn

newtype PositiveDI = PositiveDI { unPositiveDI :: DI }

instance Show PositiveDI where
    show (PositiveDI i) = show i

instance Arbitrary PositiveDI
    where
    arbitrary =
        do
        NumOrd.UniformlyOrderedPair (l,h) <- arbitrary
        return $ PositiveDI (Interval (pos l) (pos h))
        where
        pos e 
            | e > 0 =  e
            | e == 0 =  1
            | otherwise = (-e) 
