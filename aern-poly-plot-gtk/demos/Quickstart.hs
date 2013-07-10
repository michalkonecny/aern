{-|
    Code accompanying the quick start tutorial on
    using AERN to produce tight enclosures for real functions.
    
    WARNING: This module is under construction.
    It is intended that this module will demonstrate
    how AERN has been refactored to be easier to use.
    This module draft serves as source of requirements
    for the changes. 
-}
module Quickstart where

{----- AERN imports -----}

-- Doubles as interval endpoints:
import Numeric.AERN.RealArithmetic.Basis.Double ()

-- intervals with Double endpoints:
import Numeric.AERN.RealArithmetic.Interval.Double (DI, Interval)

-- interval-coefficient polynomials:
import Numeric.AERN.Poly.IntPoly (IntPoly, IntPolySizeLimits(..))

-- abstract approximate order operations:
--import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.Operators
--import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.Operators ((</\>))

-- abstract approximate real arithmetic operations:
--import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators
--import Numeric.AERN.RealArithmetic.ExactOps

-- abstract function processing operations:
import Numeric.AERN.RmToRn (newConstFn, newProjection)

{----- non-AERN imports -----}

--import qualified Data.Map as Map

{----- type definitions -----}

type PI = Interval Poly
    -- polynomial intervals

type Poly = IntPoly V DI 
    -- polynomials with V variables and Double interval coefficients

type V = String
    -- variables

{----- the first functions -----}

{-| The identity @\x:[0,1] -> x@.  This is a simple example of a projection.  -}
x :: PI
x = newProjection sizeLimits varDoms "x"

{-| The constant function @\x:[0,1] -> 1@. -}
c1 :: PI
c1 = newConstFn sizeLimits varDoms 1

{-| encoding of the domain box -}
varDoms :: [(V, DI)]
varDoms = zip vars doms

vars :: [V]
vars = ["x"]

doms :: [DI]
doms = [0 </\> 1]

{-| example size limits for polynomials -}
sizeLimits :: IntPolySizeLimits DI
sizeLimits =
    IntPolySizeLimits 
        {
            ipolylimits_cf_limits = (),
            ipolylimits_maxdeg = 3,
            ipolylimits_maxsize = 40
        }

    