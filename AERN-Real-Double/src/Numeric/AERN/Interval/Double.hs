{-|
    Module      :  Numeric.AERN.Interval.Double
    Description :  Interval Double type and operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Intervals with Double endpoints.
-}
module Numeric.AERN.Interval.Double 
(
    -- |
    -- A convenience module re-exporting various interval operations 
    -- with default effort indicators.

    -- * Main type
    DI,
    
    -- * Order relations
    -- | 
    -- There are two types of order relations to consider: 
    -- 
    --   * the /numerical/ order, generalising the order relation
    --     on Doubles and
    -- 
    --   * the /refinement/ order, generalising the reverse-inclusion 
    --     relation on intervals.  
    
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
    bottom,top,
    
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
    (|<+>),(<+>|),(|<*>),(<*>|),(</>|),

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
    (|>+<),(>+<|),(|>*<),(>*<|),(>/<|),
    
    
    -- * Low level facilities
    
    -- ** Access functions
    getEndpoints,fromEndpoints,

    -- ** Base type
    Interval(..),
)
where

import Numeric.AERN.Basics.Interval
  (Interval(..),getEndpoints,fromEndpoints)

import Numeric.AERN.Basics.NumericOrder
  (least,greatest)

import Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort
  ((==?),(<==>?),(</=>?),
   (<?),(>?),(<=?),(>=?),
   minOut,maxOut,minIn,maxIn)

import Numeric.AERN.Basics.RefinementOrder
  (bottom,top)

import Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort
  ((|==?),(|<==>?),(|</=>?),
   (|<?),(|>?),(|<=?),(|>=?),(⊏?),(⊑?),(⊒?),(⊐?),
   (</\>),(<\/>),(<\/>?),(<⊓>),(<⊔>),(<⊔>?),
   (>/\<),(>\/<),(>⊓<),(>⊔<))

--import Numeric.AERN.RealArithmetic.Interval
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort
 ((<+>),(<->),(<*>),(</>),(|<+>),(<+>|),(|<*>),(<*>|),(</>|),
  piOut,eOut,absOut,expOut,sqrtOut,
  (>+<),(>-<),(>*<),(>/<),(|>+<),(>+<|),(|>*<),(>*<|),(>/<|),
  piIn,eIn,absIn,expIn,sqrtIn)
 
import Numeric.AERN.RealArithmetic.Interval.ElementaryDirect()

import Numeric.AERN.RealArithmetic.Basis.Double

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck

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

sampleDI :: DI
sampleDI = Interval 0 0

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
