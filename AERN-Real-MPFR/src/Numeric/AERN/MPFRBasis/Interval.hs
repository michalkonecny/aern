{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.MPFRBasis.Interval
    Description :  Interval MPFR type and operations  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Intervals with MPFR endpoints.
-}
module Numeric.AERN.MPFRBasis.Interval
(
    -- |
    -- A convenience module re-exporting various interval operations 
    -- with default effort indicators.

    -- * Main type
    MI,
    
    -- ** associated operations
    width, bisect,

    -- * Order relations
    -- | 
    -- There are two types of order relations to consider: 
    -- 
    --   * the /numerical/ order, generalising the order relation
    --     on MPFRs and
    -- 
    --   * the /refinement/ order, generalising the reverse-inclusion 
    --     relation on consistent intervals.  
    --
    -- The consistent intervals in 'MI' form a /meet/-semilattice
    -- corresponding to the refiniement order under the operation /\\ 
    -- returning the subset-least interval containing the /union/ of 
    -- its argument intervals. The operation is extended to all of 'MI'
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
    -- the /join/-semilattice structure on 'MI' comes in two flavours:
    --
    --   * the partial consistent interval-valued join \\/\? which 
    --     returns 'Nothing' for disjoint and anticonsistent arguments
    --     and
    --
    --   * the total join \\/ which returns the lowest interval in
    --     'MI' above both of its argument intervals. 
    -- 
    -- The structure ('MI', \/\\, \\\/, 'bottom', 'top') is a complete 
    -- lattice.
    --
    -- Lower and upper approximations of the exact operations \\/\?
    -- and \\\/ are given by '<\/>?', '<\/>' and '>\/<' respectively.

    -- ** Numerical order
    -- | 
    -- Interval extensions of the corresponding tests and relations on 
    -- MPFR.
    
    -- *** Extrema
    -- |
    -- The values retured by 'least' and 'greatest' complete the 
    -- numerical partial order on 'MI'.
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
    -- refinement partial order on 'MI'.

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
    -- and 'Floating' instances for 'MI' use such versions as instance 
    -- methods.
    
    -- ** Order operations
    
    -- *** Numerical order
    -- | 
    -- Outward rounded interval extensions of the corresponding 
    -- operations on MPFR.
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
    absOut,
    RARORODE.expOut,RARORODE.sqrtOut,

    -- * Inward rounded operations 

    -- ** Order operations

    -- *** Numerical order
    -- | 
    -- Inward rounded interval extensions of the corresponding 
    -- operations on MPFR.
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
    absIn,
    RARORODE.expIn,RARORODE.sqrtIn,

    -- * Low level facilities

    -- ** Access functions
    getEndpoints,fromEndpoints,

    -- ** Base type
    Interval(..),
    MPFR,
    Precision
)
where

import Numeric.AERN.Basics.Interval
  (Interval(..))

import qualified Numeric.AERN.Basics.Interval as BI
  (getEndpoints,fromEndpoints)

import qualified Numeric.AERN.NumericOrder as BNO
  (least,greatest)

import qualified Numeric.AERN.NumericOrder.OpsDefaultEffort as BNOODE
  ((==?),(<==>?),(</=>?),
   (<?),(>?),(<=?),(>=?),
   minOut,maxOut,minIn,maxIn)

import qualified Numeric.AERN.RefinementOrder as BRO
  (bottom,top,(⊥),(⊤))

import qualified Numeric.AERN.RefinementOrder.OpsDefaultEffort as BROODE
  ((|==?),(|<==>?),(|</=>?),
   (|<?),(|>?),(|<=?),(|>=?),(⊏?),(⊑?),(⊒?),(⊐?),
   (</\>),(<\/>),(<\/>?),(<⊓>),(<⊔>),(<⊔>?),
   (>/\<),(>\/<),(>⊓<),(>⊔<))

import Numeric.AERN.RealArithmetic.Interval()

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as RAROR 
  (RoundedMixedAdd(..),RoundedMixedMultiply(..),RoundedMixedDivide(..))

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort as RARORODE
 ((<+>),(<->),(<*>),(</>),(|<+>),(<+>|),(|<*>),(<*>|),(</>|),(<^>),
  piOut,eOut,absOut,expOut,sqrtOut,
  (>+<),(>-<),(>*<),(>/<),(|>+<),(>+<|),(|>*<),(>*<|),(>/<|),(>^<),
  piIn,eIn,absIn,expIn,sqrtIn)
 
import qualified Numeric.AERN.RealArithmetic.Interval.ElementaryFromBasis()

import Numeric.AERN.RealArithmetic.Basis.MPFR(MPFR)

import Data.Number.MPFR (Precision)

import Numeric.AERN.RealArithmetic.Interval.MPFR(width, bisect)

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
-- Intervals with MPFR endpoints. 
-- 
-- Note that ('l','r') = 'getEndpoints' ('mi' :: 'MI') does not 
-- fix an ordering of 'l' and 'r'. 
-- 
--   * 'mi' is called /consistent/ when 'l' '<=' 'r'
--
--   * 'mi' is called /anticonsistent/ when 'r' '<=' 'l' 
--
-- A consistent interval 'di' may be identified with the set defined by
-- \{ 'x' | 'l' '<=' 'x' and 'x' '<=' 'r' \}.
type MI = Interval MPFR

-- | Given an argument interval 'i' 'getEndpoints' returns the endpoint pair 
--   ('leftEndpoint' 'i','rightEndpoint' 'i').
getEndpoints :: MI -> (MPFR, MPFR)
getEndpoints = BI.getEndpoints

-- | Constructs an interval from an endpoint pair.
fromEndpoints :: (MPFR, MPFR) -> MI
fromEndpoints = BI.fromEndpoints

sampleMI :: MI
sampleMI = Interval 0 0

least :: MI
least = BNO.least

greatest :: MI
greatest = BNO.greatest

-- | Partial equality
(==?) :: MI -> MI -> Maybe Bool
(==?) = (BNOODE.==?) 

-- | Partial `is comparable to`
(<==>?) :: MI -> MI -> Maybe Bool
(<==>?) = (BNOODE.<==>?)

-- | Partial `is not comparable to`
(</=>?) :: MI -> MI -> Maybe Bool
(</=>?) = (BNOODE.</=>?)

-- | Partial `strictly less than`
(<?) :: MI -> MI -> Maybe Bool
(<?) = (BNOODE.<?)

-- | Partial `strictly greater than`
(>?) :: MI -> MI -> Maybe Bool
(>?) = (BNOODE.>?)

-- | Partial `less than or equal to`
(<=?) :: MI -> MI -> Maybe Bool
(<=?) = (BNOODE.<=?)

-- | Partial `greater than or equal to`
(>=?) :: MI -> MI -> Maybe Bool
(>=?) = (BNOODE.>=?)
 
-- | Outward rounded minimum
minOut :: MI -> MI -> MI
minOut = BNOODE.minOut

-- | Outward rounded maximum
maxOut :: MI -> MI -> MI
maxOut = BNOODE.maxOut

-- | Inward rounded minimum
minIn :: MI -> MI -> MI
minIn = BNOODE.minIn

-- | Inward rounded maximum
maxIn :: MI -> MI -> MI
maxIn = BNOODE.maxIn

bottom :: MI
bottom = BRO.bottom

top :: MI
top = BRO.top

-- | Convenience Unicode notation for 'bottom'
(⊥) :: MI
(⊥) = (BRO.⊥)

-- | Convenience Unicode notation for 'top'
(⊤) :: MI
(⊤) = (BRO.⊤)

-- | Partial equality
(|==?) :: MI -> MI -> Maybe Bool
(|==?) = (BROODE.|==?)

-- | Partial `is comparable to`
(|<==>?) :: MI -> MI -> Maybe Bool
(|<==>?) = (BROODE.|<==>?)

-- | Partial `is not comparable to`
(|</=>?) :: MI -> MI -> Maybe Bool
(|</=>?) = (BROODE.|</=>?)

-- | Partial `strictly below`
(|<?) :: MI -> MI -> Maybe Bool
(|<?) = (BROODE.|<?)

-- | Partial `strictly above`
(|>?) :: MI -> MI -> Maybe Bool
(|>?) = (BROODE.|>?)

-- | Partial `below or equal to`
(|<=?) :: MI -> MI -> Maybe Bool
(|<=?) = (BROODE.|<=?)

-- | Partial `above or equal to`
(|>=?) :: MI -> MI -> Maybe Bool
(|>=?) = (BROODE.|>=?)

{-| Convenience Unicode notation for '|<?' -}
(⊏?) :: MI -> MI -> Maybe Bool
(⊏?) = (BROODE.⊏?)

{-| Convenience Unicode notation for '|<=?' -}
(⊑?) :: MI -> MI -> Maybe Bool
(⊑?) = (BROODE.⊑?)

{-| Convenience Unicode notation for '|>=?' -}
(⊒?) :: MI -> MI -> Maybe Bool
(⊒?) = (BROODE.⊒?)

{-| Convenience Unicode notation for '|>?' -}
(⊐?) :: MI -> MI -> Maybe Bool 
(⊐?) = (BROODE.⊐?)

-- | Outward rounded meet
(</\>) :: MI -> MI -> MI
(</\>) = (BROODE.</\>)

-- | Outward rounded join
(<\/>) :: MI -> MI -> MI
(<\/>) = (BROODE.<\/>)

-- | Inward rounded meet
(>/\<) :: MI -> MI -> MI
(>/\<) = (BROODE.>/\<)

-- | Inward rounded join
(>\/<) :: MI -> MI -> MI
(>\/<) = (BROODE.>\/<)

{-| Convenience Unicode notation for '</\>' -}
(<⊓>) :: MI -> MI -> MI
(<⊓>) = (BROODE.<⊓>)

{-| Convenience Unicode notation for '<\/>' -}
(<⊔>) :: MI -> MI -> MI
(<⊔>) = (BROODE.<⊔>)

{-| Convenience Unicode notation for '>/\<' -}
(>⊓<) :: MI -> MI -> MI
(>⊓<) = (BROODE.>⊓<)

{-| Convenience Unicode notation for '>\/<' -}
(>⊔<) :: MI -> MI -> MI
(>⊔<) = (BROODE.>⊔<)
 
-- | Partial outward rounded join
(<\/>?) :: MI -> MI -> Maybe MI
(<\/>?) = (BROODE.<\/>?)

{-| Convenience Unicode notation for '<\/>?' -}
(<⊔>?) :: MI -> MI -> Maybe MI 
(<⊔>?) = (BROODE.<⊔>?)

-- | Outward rounded addition
(<+>) :: MI -> MI -> MI
(<+>) = (RARORODE.<+>)

-- | Outward rounded subtraction
(<->) :: MI -> MI -> MI
(<->) = (RARORODE.<->)

-- | Outward rounded multiplication
(<*>) :: MI -> MI -> MI
(<*>) = (RARORODE.<*>)

-- | Outward rounded division
(</>) :: MI -> MI -> MI
(</>) = (RARORODE.</>)

-- | Inward rounded addition
(>+<) :: MI -> MI -> MI
(>+<) = (RARORODE.>+<)

-- | Inward rounded subtraction
(>-<) :: MI -> MI -> MI
(>-<) = (RARORODE.>-<)

-- | Inward rounded multiplication
(>*<) :: MI -> MI -> MI
(>*<) = (RARORODE.>*<)

-- | Inward rounded division
(>/<) :: MI -> MI -> MI
(>/<) = (RARORODE.>/<)

-- | Outward rounded additive scalar left action
(|<+>) :: RAROR.RoundedMixedAdd MI tn => tn -> MI -> MI
(|<+>) = (RARORODE.|<+>)

-- | Inward rounded additive scalar left action
(|>+<) :: RAROR.RoundedMixedAdd MI tn => tn -> MI -> MI
(|>+<) = (RARORODE.|>+<)

-- | Outward rounded additive scalar right action
(<+>|) :: RAROR.RoundedMixedAdd MI tn => MI -> tn -> MI
(<+>|) = (RARORODE.<+>|)

-- | Inward rounded additive scalar right action
(>+<|) :: RAROR.RoundedMixedAdd MI tn => MI -> tn -> MI
(>+<|) = (RARORODE.>+<|)

-- | Outward rounded multiplicative scalar left action
(|<*>) :: RAROR.RoundedMixedMultiply MI tn => tn -> MI -> MI
(|<*>) = (RARORODE.|<*>)

-- | Inward rounded multiplicative scalar left action
(|>*<) :: RAROR.RoundedMixedMultiply MI tn => tn -> MI -> MI
(|>*<) = (RARORODE.|>*<)

-- | Outward rounded multiplicative scalar right action
(<*>|) :: RAROR.RoundedMixedMultiply MI tn => MI -> tn -> MI
(<*>|) = (RARORODE.<*>|)

-- | Inward rounded multiplicative scalar right action
(>*<|) :: RAROR.RoundedMixedMultiply MI tn => MI -> tn -> MI
(>*<|) = (RARORODE.>*<|)

-- | Outward rounded multiplicative scalar reciprocal right action
(</>|) :: RAROR.RoundedMixedDivide MI tn => MI -> tn -> MI
(</>|) = (RARORODE.</>|)

-- | Inward rounded multiplicative scalar reciprocal right action
(>/<|) :: RAROR.RoundedMixedDivide MI tn => MI -> tn -> MI  
(>/<|) = (RARORODE.>/<|)

-- | Outward rounded power
(<^>) :: MI -> Int -> MI
(<^>) = (RARORODE.<^>)

-- | Inward rounded power
(>^<) :: MI -> Int -> MI
(>^<) = (RARORODE.>^<)

-- | Outward rounded pi
piOut :: MI
piOut = RARORODE.piOut 

-- | Outward rounded e
eOut :: MI
eOut = RARORODE.eOut 

-- | Inward rounded pi
piIn :: MI
piIn = RARORODE.piIn 

-- | Inward rounded e
eIn :: MI
eIn = RARORODE.eIn 

-- | Outward rounded absolute value
absOut :: MI -> MI
absOut = RARORODE.absOut

---- | Outward rounded exponential
--expOut :: MI -> MI
--expOut = RARORODE.expOut
--
---- | Outward rounded square root
--sqrtOut :: MI -> MI
--sqrtOut = RARORODE.sqrtOut

-- | Inward rounded absolute value
absIn :: MI -> MI
absIn = RARORODE.absIn

---- | Inward rounded exponential
--expIn :: MI -> MI
--expIn = RARORODE.expIn
--
---- | Inward rounded square root
--sqrtIn :: MI -> MI
--sqrtIn = RARORODE.sqrtIn

newtype PositiveMI = PositiveMI { unPositiveMI :: MI }

instance Show PositiveMI where
    show (PositiveMI i) = show i

instance Arbitrary PositiveMI
    where
    arbitrary =
        do
        NumOrd.UniformlyOrderedPair (l,h) <- arbitrary
        return $ PositiveMI (Interval (pos l) (pos h))
        where
        pos e 
            | e > 0 =  e
            | e == 0 =  1
            | otherwise = (-e) 
