module Main where

import Numeric.AERN.Poly.IntPoly

import Numeric.AERN.RmToRn

import Numeric.AERN.RealArithmetic.Basis.Double ()
--import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.Basics.Interval

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators

--import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Measures 

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.Operators

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort

import Numeric.AERN.Basics.SizeLimits
--import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.ShowInternals

--import System.IO

--import qualified Data.Map as Map
--import qualified Data.List as List

--type CF = Interval MPFR
type CF = Interval Double
type Poly = IntPoly String CF


main :: IO ()
main =
    do
    return ()

{------

(maxdeg, maxsize, recipdeg, evalsplitsize) -> range, time 

(10, 100, 8, 100) -> [_-972793.072247958,7683443.423662806^], 5.4s
(10, 100, 8, 1000) -> [_-972793.072247958,7683408.438461192^], 47s
(10, 200, 8, 100) -> [_-87430.5804374732,6873411.247332599^], 11.8s
(10, 300, 8, 100) -> [_-18708.75565867797,6600028.754532665^], 16.7s
(10, 400, 8, 100) ->  [_24754.284036349527,6281330.040812409^], 20.88s
(10, 500, 8, 100) ->  [_38138.95909293841,6103350.005896601^], 22.5s
(10, 700, 8, 100) -> [_71173.31500771112,5897926.955546107^], 31.3s
* (10, 800, 8, 100) -> [_73288.21702714099,5827688.490139663^], 40.8s 
(10, 1000, 8, 100) -> [_73288.21702714099,5738954.230274559^], 47s
(10, 1000, 8, 1000) -> [_73288.21702714099,5698260.713513501^], 358s
(10, 1200, 8, 100) -> [_73288.21702714099,5703914.65761335^], 59.1s
(10, 1000, 10, 100) -> [_73288.21702714099,5738954.230274559^], 51.5s
(12, 1000, 8, 100) -> [_73288.21702714099,5738954.230274559^], 48.8s 


(20, 1000, 8, 1000) -> [_73288.21702714099,5698260.713513501^], 359s 
(20, 2000, 10, 1000) -> [_73288.21702714099,5648992.123038283^], 436s

sqrt(73288.21702714099) > 270.7179658374 

------}

(maxdeg, maxsize, recipdeg, evalsplitsize) = (10, 800, 8, 100) 

result =
    show (maxdeg, maxsize, recipdeg, evalsplitsize) 
    ++ " -> " ++ show range_flyspeckSqr

--f_flyspeck =
--    f_delta4DeltaX / (ArithInOut.sqrtOutEff effSqrt $ 4 * x1 * f_DeltaX) 

range_flyspeckSqr =
    (evalAtPointOut (getDomainBox x1) f_flyspeckSqr)

f_flyspeckSqr =
    (f_delta4DeltaX * f_delta4DeltaX) / 4 * x1 * f_DeltaX

f_delta4DeltaX = 
    x1 * (-x1 + x2 + x3 - x4 + x5 + x6) + x2 * x5 + x3 * x6 - x2 * x3 - x5 * x6

f_DeltaX = 
    x1*x4*(-x1 + x2 + x3 - x4 + x5 + x6) + 
    x2*x5*(x1 - x2 + x3 + x4 - x5 + x6) +
    x3*x6*(x1 + x2 - x3 + x4 + x5 - x6)
    - x2*(x3*x4+x1*x6)
    - x5*(x1*x3+x4*x6) 


x1,x2,x3,x4,x5,x6 :: Poly
x1 = newProjection limits varDoms "x1"
x2 = newProjection limits varDoms "x2"
x3 = newProjection limits varDoms "x3"
x4 = newProjection limits varDoms "x4"
x5 = newProjection limits varDoms "x5"
x6 = newProjection limits varDoms "x6"

--effSqrt =
    

effIP :: IntPolyEffort CF
effIP =
    IntPolyEffort
    {
        ipolyeff_sampleCf = 0,
        ipolyeff_cfRoundedRealEffort = effCf,
        ipolyeff_cfGetEndpointsEffort = RefOrd.getEndpointsDefaultEffort sampleCf,
        ipolyeff_cfFromEndpointsEffort = RefOrd.fromEndpointsDefaultEffort sampleCf,
        ipolyeff_evalMaxSplitSize = evalsplitsize,
        ipolyeff_minmaxBernsteinDegree = bernsteinDegree,
        ipolyeff_recipTauDegree = tauDegree,
        ipolyeff_counterExampleSearchSampleCount = 4 * arity
    }
    where
    bernsteinDegree = 4
    tauDegree = recipdeg
    arity = 2
    sampleCf = 0 :: CF
    
effCf :: ArithInOut.RoundedRealEffortIndicator CF
--effCf = (100, (100,())) -- MPFR with explicit precision
effCf = ArithInOut.roundedRealDefaultEffort (0:: CF)
    
effIPsplitSize :: Int -> IntPolyEffort CF
effIPsplitSize maxsplitSize =
    effIP { ipolyeff_evalMaxSplitSize = maxsplitSize }

minmaxUpDnEff :: IntPolyEffort CF
minmaxUpDnEff = effIP { ipolyeff_minmaxBernsteinDegree = 10 }

minmaxInOutEff :: IntPolyEffort CF
minmaxInOutEff = effIP { ipolyeff_minmaxBernsteinDegree = 10 }

evalOpsOutCf :: PolyEvalOps String CF CF
evalOpsOutCf = evalOpsEff effIP x1 (0::CF)

limits :: IntPolySizeLimits CF
limits =
    IntPolySizeLimits
    {
        ipolylimits_cf_limits = (),
        ipolylimits_maxdeg = maxdeg,
        ipolylimits_maxsize = maxsize,
        ipolylimits_effort = effIP
    } 

varDoms :: [(Var Poly, CF)]
varDoms = 
    take 6 $ zip vars $ repeat dom 


vars :: [Var Poly]
vars = map (\n -> "x" ++ show n) [1..]

dom = 4 </\> (6.3504)

