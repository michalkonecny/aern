{-|
    Code accompanying the quick start tutorial on
    using AERN to produce tight enclosures for real functions.
    
    WARNING: This module is under construction.
    It is intended that this module will demonstrate
    how AERN has been refactored to be easier to use.
    This module draft serves as source of requirements
    for the changes. 
-}
module Main where

{----- AERN imports -----}

-- Doubles as interval endpoints:
import Numeric.AERN.RealArithmetic.Basis.Double () 

-- intervals with Double endpoints:
import Numeric.AERN.RealArithmetic.Interval.Double (DI, Interval)

-- interval-coefficient polynomials:
import Numeric.AERN.Poly.IntPoly (IntPoly, IntPolySizeLimits(..), defaultIntPolySizeLimits)

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

{----- definitions -----}

type PI = Interval Poly
    -- polynomial intervals

type Poly = IntPoly V DI 
    -- polynomials with V variables and Double interval coefficients

type V = String
    -- variables

main :: IO ()
main =
    do
    putStrLn $ "x + 1 = " ++ show (x + c1)

x :: PI
x = newProjection sizeLimits varDoms "x"

c1 :: PI
c1 = newConstFn sizeLimits varDoms 1

varDoms :: [(V, DI)]
varDoms = zip vars doms

vars :: [V]
vars = ["x"]

doms :: [DI]
doms = [0 </\> 1]

sizeLimits :: IntPolySizeLimits DI
sizeLimits =
    (defaultIntPolySizeLimits ()) 
        {
            ipolylimits_maxdeg = 3,
            ipolylimits_maxsize = 40
        }

    