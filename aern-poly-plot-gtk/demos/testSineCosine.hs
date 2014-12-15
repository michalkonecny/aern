{-|
    An encoding of the error function and plotting example enclosures.
-}
module TestSineCosine where

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
    (piOut, sqrtOut, expOutEff, expDefaultEffort, sincosDefaultEffort, sinOutEff, cosOutEff)
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators
--    ((|*))

import Numeric.AERN.RealArithmetic.ExactOps (one)


-- ability to control the effort for elementary operations: 
import Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps
    (SineCosineThinEffortIndicator(..))



-- abstract function processing operations:
import Numeric.AERN.RmToRn 
    (newProjection, primitiveFunctionOut)

import Numeric.AERN.RefinementOrder ((/\))

import qualified 
       Numeric.AERN.RmToRn.Plot.FnView 
    as FV

{----- type definitions -----}
type DI = Interval Double
type V = String -- names of variables
type Poly = IntPoly V DI 
type PI = Interval Poly

{----- using interval polynomials -----}

plotSineCosine :: IO ()
plotSineCosine = 
    FV.plotFns 
        [
--         ("sin^2(x) + cos^2(x) = 1", 
--            [
--             (("sin(x)", FV.blue, False), sineX),
--             (("cos(x)", FV.blue, False), cosineX)
--             ,
--             (("sin^2(x)", FV.blue, False), sineX * sineX),
--             (("cos^2(x)", FV.blue, False), cosineX * cosineX),
--             (("sin^2(x) + cos^2(x)", FV.blue, False), sineX * sineX + cosineX * cosineX)
--            ]
--         )
--         ,
         ("over thick argument",
            [
             (("x", FV.black, False), x),
             (("x+1", FV.black, False), xp1),
             (("[x,x+1]", FV.black, False), xThick),
             (("sin(x)", FV.black, False), sineX),
             (("sin(x+1)", FV.black, False), sinOutEff effSinCos xp1),
             (("sin([x,x+1])", FV.red, False), sinOutEff effSinCos xThick)
             ,
             (("cos(x)", FV.black, False), cosineX),
             (("cos(x+1)", FV.black, False), cosOutEff effSinCos xp1),
             (("cos([x,x+1])", FV.red, False), cosOutEff effSinCos xThick)
            ]
         )
--         ,
--         ("sin(x^2)",
--            [
--             (("x*x", FV.black, False), x * x),
--             (("sin(x*x)", FV.red, False), sineXSqr)
--            ]
--         )
        ]
    where
    maxdeg = 20
    taylorDeg = 10
    
    sineX =
        sinOutEff effSinCos x
    cosineX =
        cosOutEff effSinCos x
    sineXSqr =
        sinOutEff effSinCos (x * x)

    xp1 = x + (one x)
    xThick = x + y

    x :: Poly
    x = newProjection sizeLimits doms "x"
    y = newProjection sizeLimits doms "y"
    doms = 
        [
            ("x", Interval (0) 2)
            ,
            ("y", Interval (0) 1)
        ]

    -- indicators controlling the approximation effort:
    effSinCos =
        (sincosDefaultEffort sampleFn)
        {
            sincoseff_taylorDeg = taylorDeg -- Taylor approximation degree
        }

    sizeLimits = -- restrictions on the size of the polynomials
        defaultSizeLimits
        {
            ipolylimits_maxdeg = maxdeg, -- maximum degree
            ipolylimits_maxsize = 1000, -- maximum number of terms
            ipolylimits_effort =
                (ipolylimits_effort defaultSizeLimits)
                {
                    ipolyeff_evalMaxSplitSize = 2000
                } 
        }
        where
        defaultSizeLimits = (defaultIntPolySizeLimits sampleCoeff () 1)
    sampleCoeff = 0
    sampleFn = x



{----- using polynomial intervals -----}

plotSineCosinePI :: IO ()
plotSineCosinePI = 
    FV.plotFns 
        [
         ("sin^2(x) + cos^2(x) = 1", 
            [
             (("sin(x)", FV.blue, False), sineX),
             (("cos(x)", FV.blue, False), cosineX),
             (("sin^2(x)", FV.blue, False), sineX * sineX),
             (("cos^2(x)", FV.blue, False), cosineX * cosineX),
             (("sin^2(x) + cos^2(x)", FV.blue, False), sineX * sineX + cosineX * cosineX)
            ]
         )
         ,
         ("over thick argument",
            [
             (("x", FV.black, False), x),
             (("x+1", FV.black, False), x + 1),
             (("[x,x+1]", FV.black, False), xThick),
             (("sin([x,x+1])", FV.red, False), sinOutEff effSinCos xThick)
             ,
             (("cos([x,x+1])", FV.red, False), cosOutEff effSinCos xThick)
            ]
         )
         ,
         ("sin(x^2)",
            [
             (("x*x", FV.black, False), x * x),
             (("sin(x*x)", FV.red, False), sineXSqr)
            ]
         )
        ]
    where
    maxdeg = 100
    taylorDeg = 5
    
    sineX =
        sinOutEff effSinCos x
    cosineX =
        cosOutEff effSinCos x
    sineXSqr =
        sinOutEff effSinCos (x * x)
    xThick = x /\ (x + 1)
    x :: PI
    x = 
        -- ie the identity function \x:[0,1] -> x
        newProjection sizeLimits [("x", Interval (0) 1)] "x"

    -- indicators controlling the approximation effort:
    effSinCos =
        (sincosDefaultEffort sampleFn)
        {
            sincoseff_taylorDeg = taylorDeg -- Taylor approximation degree
        }

    sizeLimits = -- restrictions on the size of the polynomials
        defaultSizeLimits
        {
            ipolylimits_maxdeg = maxdeg, -- maximum degree
            ipolylimits_maxsize = 100, -- maximum number of terms
            ipolylimits_effort =
                (ipolylimits_effort defaultSizeLimits)
                {
                    ipolyeff_evalMaxSplitSize = 10000
                } 
        }
        where
        defaultSizeLimits = (defaultIntPolySizeLimits sampleCoeff () 1)
    sampleCoeff = 0
    sampleFn = x

