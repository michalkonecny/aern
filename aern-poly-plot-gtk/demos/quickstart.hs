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

-- intervals with Double endpoints:
import Numeric.AERN.RealArithmetic.Interval.Double (DI, Interval)

-- interval-coefficient polynomials:
import Numeric.AERN.Poly.IntPoly (IntPoly)

-- abstract approximate order operations:
import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.Operators
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.Operators

-- abstract approximate real arithmetic operations:
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators

-- abstract function processing operations:
import Numeric.AERN.RmToRn

{----- non-AERN imports -----}

import qualified Data.Map as Map

{----- definitions -----}

type Poly = IntPoly String DI 
    -- polynomials with string variables and Double interval coefficients
type PI = Interval Poly
    -- polynomial intervals

main =
    do
    return () -- TODO
--    putStrLn $ "x = " ++ show x

--x = newProjection sizeLimits dombox "x" :: PI
--c0 = newConstFn sizeLimits dombox 0 :: PI
--c1 = newConstFn sizeLimits dombox 1 :: PI
--
--dombox = Map.fromList $ zip vars doms
--
--vars = ["x"]
--
--doms :: [(DI, DI)]
--doms = [(0, 1)]
--
--sizeLimits =
--    IntPolySizeLimits
--        {
--            ipolylimits_maxdeg = 10,
--            ipolylimits_maxsize = 20
--        }

    