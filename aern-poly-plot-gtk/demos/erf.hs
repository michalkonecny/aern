{-|
    Code accompanying the quick start tutorial on
    using AERN to produce tight enclosures for real functions.
-}
module Erf where

-- Doubles as interval endpoints:
import Numeric.AERN.RealArithmetic.Basis.Double ()

-- intervals generic in the type of its endpoints:
import Numeric.AERN.Basics.Interval 
    (Interval(..))

-- interval-coefficient polynomials:
import Numeric.AERN.Poly.IntPoly 
    (IntPoly, IntPolySizeLimits(..), IntPolyEffort(..), defaultIntPolySizeLimits)
import Numeric.AERN.Poly.IntPoly.Interval () 
import Numeric.AERN.Poly.IntPoly.Plot () 

-- abstract approximate real arithmetic operations:
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
    (piOut, sqrtOut, expOutEff, expDefaultEffort)
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators
    ((|<*>))

-- ability to control the effort for elementary operations: 
import Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps
    (ExpThinEffortIndicator(..))

-- abstract function processing operations:
import Numeric.AERN.RmToRn 
    (newProjection, primitiveFunctionOut)
import qualified 
       Numeric.AERN.RmToRn.Plot.FnView 
    as FV

{----- type definitions -----}
type DI = Interval Double
type V = String -- names of variables
type Poly = IntPoly V DI 
type PI = Interval Poly

{----- the error function -----}

{-| An enclosure of the error function over the interval [0,2] -}
erf :: Int -> Int -> PI
erf maxdeg taylorDeg =
    scale $
    primitiveFunctionOut
        (expOutEff effExp (- x * x))
        integrationVariable
    where
    integrationVariable = "x"
    x = newProjection sizeLimits [("x", Interval 0 2)] "x"
        -- ie the identity function \x:[0,2] -> x

    scale f = c |<*> f 
              -- mixed multiplication, here: number |<*> function 
    c = 2 / (sqrtOut (piOut sampleCoeff)) -- = 2/\sqrt{\pi}

    -- indicators controlling the approximation effort:
    effExp =
        (expDefaultEffort sampleFn)
        {
            expeff_taylorDeg = taylorDeg -- Taylor approximation degree
        }
    sizeLimits = -- restrictions on the size of the polynomials
        (defaultIntPolySizeLimits sampleCoeff () 1)
        {
            ipolylimits_maxdeg = maxdeg, -- maximum degree
            ipolylimits_maxsize = 100 -- maximum number of terms
        }
    sampleCoeff = 0
    sampleFn = x

{-| Show a plot of erf(x) over [-1,1] -}
plotERF :: IO ()
plotERF = 
    FV.plotFns 
        [("erf example", 
            [
             (("(\\x:[-1,1].integral(exp(-x^2)))", FV.blue, True), erf 10 5),
             (("(\\x:[-1,1].integral(exp(-x^2)))", FV.blue, True), erf 15 5),
             (("(\\x:[-1,1].integral(exp(-x^2)))", FV.blue, True), erf 20 5),
             (("(\\x:[-1,1].integral(exp(-x^2)))", FV.black, True), erf 25 5),
             (("(\\x:[-1,1].integral(exp(-x^2)))", FV.black, True), erf 30 5)
            ]
         )
        ]

