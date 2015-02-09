{-|
    More Digits 2006 problem P01
    (http://rnc7.loria.fr/competition.html)
-}
module Main(main) where

-- the MPFR interval type:
import qualified Numeric.AERN.MPFRBasis.Interval as MI

import Data.Maybe (fromJust, isJust)

type RealApprox = MI.MI -- MPFR interval
type Precision = MI.Precision

lowPrec :: Precision
lowPrec = 50

improvePrec :: Double -> Precision -> Precision
improvePrec ratio p = ceiling $ (fromIntegral p) * ratio

main :: IO ()
main =
    do
    print $ benchmark params_hard

{-|
    Compute n digits of the expression c-th root of a/b.
-}
data Params =
    Params { p_n :: Int, p_a :: Int, p_b :: Int, p_c :: Int }

params_hard :: Params
params_hard = Params { p_n = 209953, p_a = 1234567, p_b = 20000, p_c = 543219876 }

--params_easy :: Params
--params_easy = Params { p_n = 25000, p_a = 345678, p_b = 1000, p_c = 1008 }
--
--params_super_easy :: Params
--params_super_easy = Params { p_n = 1000, p_a = 2, p_b = 1, p_c = 2 }

benchmark ::
    Params -> (RealApprox, RealApprox, Precision)
benchmark params
   | a == 0 = (0, 0, 50 :: Precision)
        --since the benchmark takes only rational inputs, we don't have to use different algorithms
        --depending on whether or not the argument is small.
   | otherwise = (result, MI.width result, finalPrecision)
   where
   (Params n a b c) = params

   x0 = forgetErrors (fromJust (bisectUntilClose (aLowPrec/bLowPrec) c))
   aLowPrec :: RealApprox
   aLowPrec = a MI.|<*> (MI.setPrecOut lowPrec 1)
   bLowPrec :: RealApprox
   bLowPrec = b MI.|<*> (MI.setPrecOut lowPrec 1)
   (result, finalPrecision) = tryPrecision lowPrec
   tryPrecision :: Precision -> (RealApprox, Precision)
   tryPrecision prec
      | goodEnough result2 = result2
      | otherwise = tryPrecision (improvePrec 2 prec)
      where
      result2 = (nthRootNewton (aReal/bReal) (MI.setPrecOut prec x0) c n, prec)
      aReal :: RealApprox
      aReal =  a MI.|<*> (MI.setPrecOut prec 1)
      bReal :: RealApprox
      bReal = b MI.|<*> (MI.setPrecOut prec 1)
      goodEnough (r,_) = isJust (maybeGoodEnough r) && fromJust (maybeGoodEnough r)
      maybeGoodEnough x = (MI.width x) MI.<? 1/((MI.setPrecOut prec 10)^(n))

nthRootNewton :: RealApprox -> RealApprox -> Int -> Int -> RealApprox
nthRootNewton x x0 n decimalDigits =
  (lowerBound bounds) MI.</\> (upperBound bounds)
  where
  lowerBound (it,delta) = it + delta -- delta is always negative
  upperBound (it,_delta) = it
  bounds =
     nthRootNewtonIt (iterationsFromDigits decimalDigits) x0
     where
     -- TODO: try the following:
     --   stop when the improvement with the last iteration was not at least by 50% width
     iterationsFromDigits :: Int -> Int
     iterationsFromDigits n2 =
        ceiling ((logBase 2 (logBase 2 10)) + (logBase 2 (fromIntegral n2 :: Double)))
  nthRootNewtonIt :: Int -> RealApprox -> (RealApprox, RealApprox)
  nthRootNewtonIt k it
     | k == 0 = (it,delta)
     | otherwise = nthRootNewtonIt (k - 1) nextIt
     where
     nextIt = forgetErrors (it + delta)
     delta = 1/nReal * (x/(it^(n - 1)) - it)
  nReal :: RealApprox
  nReal = n MI.|<*> (MI.setPrecOut (MI.getPrec x) 1)


-- TODO currently assumes that x >> 0.
-- The problem comes from an analytic error estimate in Newton's iteration, which assumes
-- that the function is twice differentiable near the root, which is not the case in zero.
-- Also, we have to do a lot of (slow) bisections if x is close to zero, since the error estimate
-- goes (linearly) to infinity as x approaches 0.
bisectUntilClose :: RealApprox -> Int -> Maybe RealApprox
bisectUntilClose xOrig n =
   keepTrying xOrig (initialUpper MI.</\> initialLower) (MI.getPrec xOrig)
   where
   initialUpper = xOrig + 1 -- we add 1 to get a guaranteed upper bound in case that x < 1
   initialLower = 0
   keepTrying :: RealApprox -> RealApprox -> Precision -> Maybe RealApprox
   keepTrying x enclosure prec
      | surelyClose = Just enclosure
      | isMidpointSmaller == Just True =
            keepTrying x upperEnclosure prec
      | isMidpointSmaller == Just False =
            keepTrying x lowerEnclosure prec
      | otherwise = -- comparison has not yielded a well-defined result
            keepTrying (MI.setPrecOut (improvePrec 1.5 prec) x) enclosure (improvePrec 1.5 prec)
      where
      surelyClose = isJust maybeClose && fromJust maybeClose
      maybeClose = (MI.width enclosure) MI.<? epsilon
         where
         epsilon = ((MI.setPrecOut prec lower) MI.</>| n) --TODO here we assume that x >> 0
         lower = MI.minOut lowerBoundOfX 1
         lowerBoundOfX = MI.fromEndpoints (lowerBoundMPFR, lowerBoundMPFR)
         (lowerBoundMPFR, _) = MI.getEndpoints x -- assuming x is consistent

            -- lower bound on the nth root. If x >= 1, then we use 1.
            -- Otherwise we use a lower bound on x, using that
            -- in this case this is also a lower bound on the root.
      isMidpointSmaller = (midpoint^n) MI.<? x
         where
         midpoint = MI.fromEndpoints (mpfrMidpoint, mpfrMidpoint)
      upperEnclosure = MI.fromEndpoints (mpfrMidpoint, endpointB)
      lowerEnclosure = MI.fromEndpoints (endpointA, mpfrMidpoint)
      mpfrMidpoint = (endpointA + endpointB)/2
      (endpointA, endpointB) = MI.getEndpoints enclosure

forgetErrors :: RealApprox -> RealApprox
forgetErrors y =
  yExact
  where
  ympfr = snd (MI.getEndpoints y)
  yExact = MI.fromEndpoints (ympfr,ympfr)

