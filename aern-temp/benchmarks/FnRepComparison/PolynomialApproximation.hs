module PolynomialApproximation where

import FunctionAbstraction

import Numeric.AERN.RmToRn

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.MPFRBasis.Interval

type Poly = IntPoly String MI

{-| An effective real function of one variable.  
    The Int parameter @n@ gives the following bound on the width 
    of the resulting polynomial enclosure: @ <= 2^(-n)@ 
-}
newtype PolyReal = PolyReal (Int -> Poly)

instance RF PolyReal where
    build = buildPolyReal
    evalMI = evalMIPolyReal
    
buildPolyReal :: (Rational, Rational) -> Expression -> PolyReal
buildPolyReal (domL, domR) (ExprLit r) =
    undefined
--    PolyReal $ \n -> newConst sizeLimits dbox (fromRationalWithAccuracy n r)

evalMIPolyReal :: PolyReal -> MI -> MI
evalMIPolyReal =
    undefined    
    