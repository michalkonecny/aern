module Main where

import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsDefaultEffort
import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.MPFRBasis.Interval
import Numeric.AERN.Basics.ShowInternals

import qualified Data.List as List

main =
    do
    putStrLn $ unlines $ map show firstStep
    
firstStep = makeStep 30 1 1 0
    
makeStep dg h y0 y0Der =
    markShrinking $
    map (evalPoly h) $ 
    map fst $    
    take 20 $
    iterate (reduceBoth . picard (y0, y0Der)) $ 
    ([y0 + (plusMinus h)], [y0Der]) 
    where
    reduceBoth (p1,p2) = (reducePoly h dg p1, reducePoly h dg p2)
    markShrinking (p1 : p2 : rest) 
        = (p1, shrinking p1 p2) : (markShrinking (p2 : rest))
    markShrinking [p1] = [(p1, Nothing)]
    shrinking i1 i2 = i1 |<=? i2
    
plusMinus h = (-h) </\> h
    
picard ::
--    MI {-^ starting time -} -> -- autonomous system, always 0 
    (MI, MI) {-^ initial values for y,y' -} -> 
    (Poly, Poly) {-^ approximates of y and y' -} -> 
    (Poly, Poly) {-^ imporved approximates of y and y' -}
picard (y0, y0Der) (yPrev, yPrevDer) =
    (yNext, yNextDer)
    where
    yNextDer =
        integratePoly y0Der $
            (negPoly yPrev)
    yNext =
        integratePoly y0 yNextDer
    
--stepOpt (Interval yPrevL yPrevR) (Interval yPrevDerL yPrevDerR) (Interval h _) =
--    (Interval yNextL yNextR, Interval yNextDerL yNextDerR)
--    where
--    yNextDerL
--        = yPrevDerL -. ((h *^ h +^ h) *^ yPrevR)
--    yNextDerR
--        = yPrevDerR -^ ((h *. h +. h) *. yPrevL)
    
    
{--- poor man's polynomials  ---}
type Poly = [MI] -- uni-variate with constant term last

showPoly :: Poly -> String
showPoly [] = "0"
showPoly poly =
    List.intercalate " + " $ zipWith showTerm poly [degree,degree-1..0]
    where
    degree = length poly - 1
    showTerm coeff 0 = showCoeff coeff
    showTerm coeff n = showCoeff coeff ++ "*" ++ (showVar n)
    showCoeff = showInternals (10,False)
    showVar 1 = "x"
    showVar n = "x^" ++ show n 

evalPoly :: MI -> Poly -> MI
evalPoly x poly@(coeffHighestPower : rest) =
    aux rest x coeffHighestPower
    where
    aux [] _ acc = acc
    aux (coeffHighestPower : rest) x acc =
        aux rest x newAcc
        where
        newAcc = coeffHighestPower <+> (x <*> acc)

negPoly :: Poly -> Poly
negPoly poly = map negate poly

integratePoly :: MI -> Poly -> Poly
integratePoly const poly =
    polyFractions ++ [const]
    where
    polyFractions = zipWith (</>|) poly (reverse [1..degreePlusOne])
    degreePlusOne = length poly

reducePoly :: 
    MI {-^ @h@ right domain boundary, @h<1@ (left boundary is 0) -} -> 
    Int -> 
    Poly -> Poly
reducePoly h n poly@(highestDegreeCoeff : rest) 
    | n >= lengthPoly - 1 = poly
    | otherwise =
        addPolyConst polyLowDegreeCoeffs $
            (foldl1 (<+>) $ map range polyHighDegreeCoeffs)
    where
    lengthPoly = length poly
    (polyHighDegreeCoeffs, polyLowDegreeCoeffs) 
        = splitAt (lengthPoly - 1 - n) poly
    range coeff = 0 </\> (h <*> coeff)
        
--addPolys :: Poly -> Poly -> Poly
--addPolys poly1 poly2 
--    | lengthPoly1 == lengthPoly2 =
--        zipWith (<+>) poly1 poly2
--    | lengthPoly1 > lengthPoly2 =
--        zipWith (<+>) poly1  paddedPoly2
--    | otherwise =
--        zipWith (<+>) paddedPoly1  poly2
--    where
--    lengthPoly1 = length poly1
--    lengthPoly2 = length poly2
--    paddedPoly2 = (replicate (lengthPoly1 - lengthPoly2) 0) ++ poly2
--    paddedPoly1 = (replicate (lengthPoly2 - lengthPoly1) 0) ++ poly1
--    
addPolyConst :: Poly -> MI -> Poly
addPolyConst [] const = [const]
addPolyConst poly const =
    aux poly
    where
    aux [c] = [c <+> const]
    aux (t : ts) = t : (aux ts)  
    

    