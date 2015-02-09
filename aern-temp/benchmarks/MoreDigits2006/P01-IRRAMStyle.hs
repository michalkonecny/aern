{-|
    More Digits 2006 problem P01
    (http://rnc7.loria.fr/competition.html)
-}
module Main where

-- the MPFR interval type:
import qualified Numeric.AERN.MPFRBasis.Interval as MI

-- real arithmetic operators and imprecision measure:
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators ((|*))
import Numeric.AERN.RealArithmetic.RefinementOrderRounding (RoundedReal)
import Numeric.AERN.RealArithmetic.Measures (imprecisionOf, iterateUntilAccurate)

-- ability to change precision of numbers:
import Numeric.AERN.Basics.SizeLimits (changeSizeLimitsOut)

-- generic tools for controlling formatting of variable precision approximations:
import Numeric.AERN.Basics.ShowInternals (showInternals)


import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Environment (getArgs)

type RealApprox = MI.MI -- MPFR interval
type Precision = MI.Precision

main = 
    do
    print $ benchmark params_hard 

{-|
    Compute n digits of the expression c-th root of a/b.
-}
data Params =
    Params { p_n :: Int, p_a :: Int, p_b :: Int, p_c :: Int }

params_easy :: Params
params_easy = Params { p_n = 25000, p_a = 345678, p_b = 1000, p_c = 1008 }

params_hard :: Params
params_hard = Params { p_n = 209953, p_a = 1234567, p_b = 20000, p_c = 543219876 }

params_super_easy :: Params
params_super_easy = Params { p_n = 1000, p_a = 2, p_b = 1, p_c = 2 }

iterationsFromDigits :: Int -> Int
iterationsFromDigits n = ceiling ((logBase 2 (logBase 2 10)) + (logBase 2 (fromIntegral n)))

benchmark params = 
   if(a == 0) then (0, 0, 50 :: Precision) --since the benchmark takes only rational inputs, we don't have to use different algorithms
                     --depending on whether or not the argument is small.
   else  (result, MI.width result, finalPrecision)
   where  
   (Params n a b c) = params 
   
   x0 = forgetErrors (fromJust (bisectUntilClose (aLowPrec/bLowPrec) c))
   aLowPrec :: RealApprox
   aLowPrec = a |* (changeSizeLimitsOut (50 :: Precision) 1)
   bLowPrec :: RealApprox
   bLowPrec = b |* (changeSizeLimitsOut (50 :: Precision) 1)
   fromJust (Just something) = something
   forgetErrors :: RealApprox -> RealApprox
   forgetErrors y =
      yExact
      where
      ympfr = snd (MI.getEndpoints y)
      yExact = MI.fromEndpoints (ympfr,ympfr)
   (result, finalPrecision) = tryPrecision a b c n 50
   try prec = tryPrecision a b c n prec
   tryPrecision :: Int -> Int -> Int -> Int -> Precision -> (RealApprox, Precision)
   tryPrecision a b c n prec =
      if goodEnough result then result
      else tryPrecision a b c n (improve prec)
      where
      improve p = p * 2 -- I'd like to put 1.5 or 1.25, but I don't know how to implement this...
      result = (nthRootNewton (aReal/bReal) (changeSizeLimitsOut prec x0) c n, prec)
      aReal :: RealApprox
      aReal =  a |* (changeSizeLimitsOut prec 1)
      bReal :: RealApprox
      bReal = b |* (changeSizeLimitsOut prec 1)
      goodEnough (r,_) = goodResult (maybeGoodEnough r) && fromJust (maybeGoodEnough r)
      maybeGoodEnough x = (MI.width x) MI.<? 1/((changeSizeLimitsOut prec 10)^(n))
      goodResult (Just x) = True
      goodResult Nothing = False
      fromJust (Just x) = x
   
newtonRootFinder :: RealApprox -> Int -> Int -> RealApprox
newtonRootFinder x n decimalDigits = 
   tryPrecision x n decimalDigits 50
   where  
   x0 = forgetErrors (fromJust (bisectUntilClose xLowPrec n))
   xLowPrec :: RealApprox
   xLowPrec = (changeSizeLimitsOut (50 :: Precision) x)
   fromJust (Just something) = something
   forgetErrors :: RealApprox -> RealApprox
   forgetErrors y =
      yExact
      where
      ympfr = snd (MI.getEndpoints y)
      yExact = MI.fromEndpoints (ympfr,ympfr)
   tryPrecision :: RealApprox -> Int -> Int -> Precision -> RealApprox
   tryPrecision x n decimalDigits prec =
      if goodEnough result then result
      else tryPrecision x n decimalDigits (improve prec)
      where
      improve p = p * 2 -- I'd like to put 1.5 or 1.25, but I don't know how to implement this...
      result = nthRootNewton x (changeSizeLimitsOut prec x0) n decimalDigits
      goodEnough r = goodResult (maybeGoodEnough r) && fromJust (maybeGoodEnough r)
      maybeGoodEnough x = (MI.width x) MI.<? 1/((changeSizeLimitsOut prec 10)^(decimalDigits))
      goodResult (Just x) = True
      goodResult Nothing = False
      fromJust (Just x) = x

nthRootNewton :: RealApprox -> RealApprox -> Int -> Int -> RealApprox
nthRootNewton x x0 n decimalDigits =
  (lowerBound bounds) MI.</\> (upperBound bounds)
  where
  bounds = nthRootNewtonIt x n (iterationsFromDigits decimalDigits) x0 (makedelta x0)
  fromJust (Just num) = num
  nReal :: RealApprox
  nReal = n |* (changeSizeLimitsOut (MI.getPrec x) 1)
  makedelta y = 1/nReal * (x/(y^(n - 1)) - y)
  lowerBound (it,delta) = it + delta
  upperBound (it,delta) = it
  forgetErrors :: RealApprox -> RealApprox
  forgetErrors y =
    changeSizeLimitsOut (MI.getPrec x) yExact
    where
    ympfr = snd (MI.getEndpoints y)
    yExact = MI.fromEndpoints (ympfr,ympfr)
  nthRootNewtonIt :: RealApprox -> Int -> Int -> RealApprox -> RealApprox -> (RealApprox, RealApprox)
  nthRootNewtonIt x n k it delta =
    if k == 0 then (it,delta)
    else nthRootNewtonIt x n (k - 1) nextIt (makedelta nextIt)
    where
    nextIt = forgetErrors (it + delta)

-- TODO currently assumes that x >> 0.
-- The problem comes from an analytic error estimate in Newton's iteration, which assumes
-- that the function is twice differentiable near the root, which is not the case in zero.
-- Also, we have to do a lot of (slow) bisections if x is close to zero, since the error estimate
-- goes (linearly) to infinity as x approaches 0.
bisectUntilClose :: RealApprox -> Int -> Maybe RealApprox
bisectUntilClose x n =
   keepTrying x n (initialUpper MI.</\> initialLower) (MI.getPrec x)
   where
   goodResult (Just x) = True
   goodResult Nothing = False
   fromJust (Just x) = x
   initialUpper = x + 1 -- we add 1 to get a guaranteed upper bound in case that x < 1
   initialLower = 0
   nontrivialLowerBound :: RealApprox -> Maybe RealApprox -- find lower bound on x (not on the root!) greater than 0.
   nontrivialLowerBound x = 
      if goodResult greaterZero then
         if fromJust greaterZero then
            Just lowerBound
         else 
            Nothing
      else
         Nothing
      where
      greaterZero = x MI.>? 0
      (ampfr, bmpfr) = MI.getEndpoints x
      (a, b) = (MI.fromEndpoints (ampfr,ampfr), MI.fromEndpoints (bmpfr,bmpfr))
      lowerBound = MI.minOut a b
   keepTrying :: RealApprox -> Int -> RealApprox -> Precision -> Maybe RealApprox
   keepTrying x n enclosure prec =
      if surelyClose then Just enclosure
      else
        if goodResult isGuessSmaller then -- comparison yielded well-defined result?
           if fromJust isGuessSmaller then keepTrying x n upperEnclosure prec
           else keepTrying x n lowerEnclosure prec
        else                              -- if not, try with higher precision!
           keepTrying (changeSizeLimitsOut (improve prec) x) n enclosure (improve prec)
      where
      improve p = p*2
      epsilon = ((changeSizeLimitsOut prec lower) MI.</>| n) --TODO here we assume that x >> 0
      lower = MI.minOut (fromJust (nontrivialLowerBound x)) 1 -- lower bound on the nth root. If x >= 1, then we use 1. Otherwise we use a lower bound on x, using that in this case this is also a lower bound on the root.
      maybeClose = (MI.width enclosure) MI.<? epsilon
      surelyClose = goodResult maybeClose && fromJust maybeClose
      isGuessSmaller = guess MI.<? x
      (endpointA, endpointB) = MI.getEndpoints enclosure
      mpfrMidpoint = (endpointA + endpointB)/2
      midpoint = MI.fromEndpoints (mpfrMidpoint, mpfrMidpoint)
      guess = midpoint^n
      upperEnclosure = MI.fromEndpoints (mpfrMidpoint, endpointB)
      lowerEnclosure = MI.fromEndpoints (endpointA, mpfrMidpoint)