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
    (|<+>),(<+>|),(|<*>),(<*>|),(</>|),

    -- ** Special constants 
    piOut,eOut,
    
    -- ** Elementary functions
    absOut,expOut,sqrtOut
)
where

import Numeric.AERN.Basics.Interval
  (Interval(..))

import Numeric.AERN.Basics.NumericOrder
  (least,greatest)

import Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort
  ((==?),(<==>?),(</=>?),
   (<?),(>?),(<=?),(>=?),
   minOut,maxOut,minIn,maxIn)

import Numeric.AERN.Basics.RefinementOrder
  (bottom,top,(⊥),(⊤))

import Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort
  ((|==?),(|<==>?),(|</=>?),
   (|<?),(|>?),(|<=?),(|>=?),(⊏?),(⊑?),(⊒?),(⊐?),
   (</\>),(<\/>?),(<⊓>),(<⊔>?))

--import Numeric.AERN.RealArithmetic.Interval
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
 ((<+>),(<->),(<*>),(</>),(|<+>),(<+>|),(|<*>),(<*>|),(</>|),
  piOut,eOut,absOut,expOut,sqrtOut)
 
import Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps()

import Numeric.AERN.RealArithmetic.Basis.Double

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck

-- | 
-- Intervals with Double endpoints, presented as an abstract
-- data type for approximating real numbers.  One interval
-- represents a single real number contained in it.  Only
-- operations supporting this view are provided by this module. 
type RealApprox = Interval Double

sampleRealApprox :: RealApprox
sampleRealApprox = Interval 0 0

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
