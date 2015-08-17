module PolynomialApproximation where

import FunctionAbstraction

import Numeric.AERN.RmToRn

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.MPFRBasis.Interval

import qualified Numeric.AERN.RefinementOrder as RefOrd (fromEndpointsOut)
import Numeric.AERN.RealArithmetic.Measures (imprecisionOf)
import Numeric.AERN.Basics.Effort

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
buildPolyReal (lRat, rRat) (ExprLit aRat) =
    PolyReal polySeq
    where
    aSeq = fromRationalWithIncreasingAccuracy aRat
    lSeq = fromRationalWithIncreasingAccuracy lRat
    rSeq = fromRationalWithIncreasingAccuracy rRat
    polySeq n = newConstFn limits varDoms aMI
        where
        aMI = aSeq !! n
        lMI = lSeq !! n
        rMI = rSeq !! n
        limits :: IntPolySizeLimits MI
        limits =
            (defaultIntPolySizeLimits 0 coeffPrecision 1)
            {
                ipolylimits_maxdeg = 10,
                ipolylimits_maxsize = 11
            } 
            where
            coeffPrecision = getPrec aMI
        varDoms = zip vars doms
        vars = ["x"]
        doms = [RefOrd.fromEndpointsOut (lMI, rMI)]
    
    
evalMIPolyReal :: PolyReal -> MI -> MI
evalMIPolyReal (PolyReal polySeq) x =
    -- evaluate on successive polynomials until:
    --   - the precision of coefficient in the polynomials is higher than the precision of x
    --   - the accuracy of the result has not improved several times in a row (how many times?)   
    undefined
    
fromRationalWithIncreasingAccuracy :: Rational -> [MI]
fromRationalWithIncreasingAccuracy a =
    adjustRate (0 :: Int) $ 
        map withPrec precisionSequence
    where
    withPrec p = fromRationalWithPrec p a
    precisionSequence = effortIncrementSequence initPrec
        where
        initPrec = 50
    adjustRate i l@(h : t) 
        | hAccurateEnough = h : (adjustRate (i+1) l)
        | otherwise = adjustRate i t
        where
        hAccurateEnough 
            | i == 0 =
                (imprecisionOf h <=? one) == Just True
                -- this case is required to avoid the following trap: 
                --   two^^(0) = 1 with MPFR precision 100
            | otherwise =
                (imprecisionOf h <=? two^^(-i)) == Just True
            where
            one = fromRationalWithPrec (getPrec h) 1
            two = fromRationalWithPrec (getPrec h) 2
    adjustRate _ _ = error "internal error in fromRationalWithIncreasingAccuracy" 
        
    