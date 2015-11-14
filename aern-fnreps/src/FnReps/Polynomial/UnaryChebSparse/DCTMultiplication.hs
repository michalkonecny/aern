{-|
    This module implements the multiplication algorithm presented in:
    [BT97] http://dx.doi.org/10.1016/0024-3795(95)00696-6

-}
module FnReps.Polynomial.UnaryChebSparse.DCTMultiplication 
--(multiplyDCT_terms)
where

--import Numeric.AERN.MPFRBasis.Interval
import Numeric.AERN.DoubleBasis.Interval
import Numeric.AERN.RealArithmetic.RefinementOrderRounding ((/|), (|*))

import qualified Data.HashMap.Strict as HM

--import Debug.Trace (trace)

--type RA = MI
type RA = DI

multiplyDCT_terms :: HM.HashMap Int RA -> HM.HashMap Int RA -> HM.HashMap Int RA
multiplyDCT_terms =
    undefined

{-|
    Simplified DCT-III computed directly from its definition in
    [BT97, page 20, display (6.3)].
    
    This is quite inefficient.  It is to be used only as a reference in tests.
-}
tSDCT_III_reference :: 
    [RA] {-^ h a vector of validated real numbers -} -> 
    [RA] {-^ h~ a vector of validated real numbers -}
tSDCT_III_reference h =
--    trace (
--        "rPi = " ++ show rPi
--        ++ "\ncN = " ++ show cN
--        ++ "\ncN1 = " ++ show cN1
--        ++ "\n( (((4*3+1)*3 :: Int) |* rPi) /| cN) = " ++ show (( (((4*3+1)*3 :: Int) |* rPi) /| cN))
--        ++ "\ncos ( (((4*3+1)*3 :: Int) |* rPi) /| cN) = " ++ show (cos ( (((4*3+1)*3 :: Int) |* rPi) /| cN))
--    ) $
    [sum [ (h !! ell) * cos ( (((4*j+1)*ell) |* rPi) /| cN)
            | ell <- [0..(cN1-1)] 
         ] 
        | j <- [0..(cN1-1)]
    ]
    where
    cN = cN1 * 2
    cN1 = length h
    rPi = piOut -- rSample
    (rSample : _) = h

{-|
    Simplified DCT-III computed as described in 
    [BT97, page 21, Algorithm 1].
    
    Changed many occurrences of N1 with N1-1 because the indices were out of range.
    This is part of a trial and error process.
    
    Precondition: length h is a power of 2
-}
tSDCT_III_nlogn :: 
    [RA] {-^ h a vector of validated real numbers -} -> 
    [RA] {-^ h~ a vector of validated real numbers -}
tSDCT_III_nlogn h =
    map (\ (_,[a],_) -> a) $ splitUntilSingletons $ [(0, h, 1)]
    where
    splitUntilSingletons :: [(Int, [RA], Int)] -> [(Int, [RA], Int)]
    splitUntilSingletons groups
        | allSingletons = groups
        | otherwise =
            splitUntilSingletons $
                concat $ map splitGroup groups
        where
        allSingletons = and $ map isSingleton groups
        isSingleton (_, [_], _) = True
        isSingleton _ = False
    splitGroup :: (Int, [RA], Int) -> [(Int, [RA], Int)]
    splitGroup (c_Itau_minus_1, hItau_minus_1, two_pow_tau_minus_1) =
        [subgroup 0, subgroup 1]
        where
        subgroup bit_iTauMinus1 =
            (c_Itau_minus_1 + bit_iTauMinus1 * two_pow_tau_minus_1, 
             map hItau [0..c_Ntau_plus_1-1], 
             2 * two_pow_tau_minus_1)
            where
            hItau 0 = 
                (hItau_minus_1 !! 0) 
                + 
                (minusOnePow bit_iTauMinus1) * (hItau_minus_1 !! (c_Ntau_plus_1)) * gamma
            hItau n = 
                (hItau_minus_1 !! n)
                -
                (hItau_minus_1 !! (c_Ntau - n))
                + 
                (((2 :: Int) |* (minusOnePow bit_iTauMinus1)) * (hItau_minus_1 !! (c_Ntau_plus_1+n)) * gamma)
            gamma =
                cos $ (((4 * c_Itau_minus_1) + 1) |* rPi) /| (4*two_pow_tau_minus_1)
        c_Ntau = length hItau_minus_1
        c_Ntau_plus_1 
            | even c_Ntau = c_Ntau `div` 2
            | otherwise = error "tSDCT_III_nlogn: precondition violated: (length h) has to be a power of 2"
        
    minusOnePow :: Int -> RA
    minusOnePow 0 = 1
    minusOnePow 1 = -1 
    minusOnePow _ = error "tSDCT_III_nlogn: minusOnePow called with a value other than 0,1"

    rPi = piOut -- rSample
    (rSample : _) = h

allBits :: Int -> [[Int]]
allBits n 
    | n > 0 =
        (map (0:) allBits_n_minus_1) ++ (map (1:) allBits_n_minus_1)
    | n == 0 = [[]]
    | otherwise = error "allBits: precondition violated: n >= 0"
    where
    allBits_n_minus_1 = allBits (n - 1)
        
    
{-| 
    The formula at the bottom of page 20, fixed by Eike. 
    
    This should be equivalent to tSDCT_III_reference.
-}
aux1 h =
    [htld j 
        | j <- [0..(cN1-1)]
    ]
    where
    htld j =
        (h !! 0) 
        +
        (sum 
            [
                ((h !! n) - (h !! (cN1 - n)))
                *
                (cos ((((4*j + 1)*n) |* rPi)/| cN))
                | n <- [1..(cN2 - 1)]
            ]
        ) 
        +
        2 * cos(((4*j + 1) |* rPi)/ 4) *
        (
            0.5 * (h !! cN2)
            +
            (sum
                [
                    (h !! (cN2 + n))
                    *
                    (cos ((((4*j + 1)*(n)) |* rPi)/| cN))
                    | n <- [1..(cN2 - 1)]
                ]
            )
        )
    cN = cN1 * 2
    cN1 = length h
    cN2 = cN1 `div` 2
    rPi = piOut -- rSample
    (rSample : _) = h
    
    