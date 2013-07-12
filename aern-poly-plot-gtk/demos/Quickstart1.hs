{-|
    Code accompanying the quick start tutorial on
    using AERN to produce tight enclosures for real functions.
-}
module Quickstart1 where

-- Doubles as interval endpoints:
import Numeric.AERN.RealArithmetic.Basis.Double ()

-- intervals generic in the type of its endpoints:
import Numeric.AERN.Basics.Interval

-- interval-coefficient polynomials:
import Numeric.AERN.Poly.IntPoly 
    (IntPoly, IntPolySizeLimits(..), defaultIntPolySizeLimits)
import Numeric.AERN.Poly.IntPoly.Plot ()

-- abstract approximate order operations:
import Numeric.AERN.RefinementOrder.Operators 
    ((</\>)) -- hull of interval union

-- abstract approximate real arithmetic operations:
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

-- abstract function processing operations:
import Numeric.AERN.RmToRn (newConstFn, newProjection)
import Numeric.AERN.RmToRn.Plot.Simple (plotFns)

{----- type definitions -----}
type DI = Interval Double
type V = String -- names of variables
type Poly = IntPoly V DI 
type PI = Interval Poly

{----- constructing basic functions -----}

{-| encoding of the domain box -}
varDoms :: [(V, DI)]
varDoms = [("x", (-1) </\> 1)]

{-| example size limits for polynomials -}
sizeLimits :: IntPolySizeLimits DI
sizeLimits = defaultIntPolySizeLimits () 

{-| The identity @\x:[0,1] -> x@.  This is a simple example of a projection.  -}
x :: PI
x = newProjection sizeLimits varDoms "x"

{-| The constant function @\x:[0,1] -> 1@. -}
c1 :: PI
c1 = newConstFn sizeLimits varDoms 1

{-| The function @\x:[0,1] -> sqrt(x+1)@. -}
sqrtXplus1 :: PI
sqrtXplus1 = ArithInOut.sqrtOut $ x + c1 + c1

{-
    To make the following plotting code work, the file FnView.glade
    has to be available in the current directory.  The master copy of this file
    is in the root folder of the aern-realfn-plot-gtk package.
    (https://aern.googlecode.com/hg/aern-realfn-plot-gtk/FnView.glade)
-}
plotSqrt :: IO ()
plotSqrt = 
    plotFns 
        [("sqrt example", 
            [("(\\x:[0,1].x)", x),
             ("(\\x:[0,1].1)", c1),
             ("(\\x:[0,1].sqrt(x+1))", sqrtXplus1)])]

