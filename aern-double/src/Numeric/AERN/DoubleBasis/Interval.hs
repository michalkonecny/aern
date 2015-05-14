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
  (Interval(..),getEndpoints,fromEndpoints)

import Numeric.AERN.NumericOrder 
    hiding (least,greatest) 
import qualified Numeric.AERN.NumericOrder as BNO
    (least,greatest)

import Numeric.AERN.RefinementOrder
    hiding (bottom,top,(⊥),(⊤))
import qualified Numeric.AERN.RefinementOrder as BRO
    (bottom,top,(⊥),(⊤))

import Numeric.AERN.RealArithmetic.Interval()

import Numeric.AERN.RealArithmetic.RefinementOrderRounding 
  hiding (piOut,piIn,eOut,eIn)
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as RAROR 
  (piOut,piIn,eOut,eIn)
 
import Numeric.AERN.RealArithmetic.Basis.Double()

import Numeric.AERN.RealArithmetic.Interval.Double (width, bisect)

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
sampleDI = 0

least :: DI
least = BNO.least sampleDI

greatest :: DI
greatest = BNO.greatest sampleDI

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

-- | Outward rounded pi
piOut :: DI
piOut = RAROR.piOut sampleDI

-- | Outward rounded e
eOut :: DI
eOut = RAROR.eOut sampleDI

-- | Inward rounded pi
piIn :: DI
piIn = RAROR.piIn sampleDI

-- | Inward rounded e
eIn :: DI
eIn = RAROR.eIn sampleDI
