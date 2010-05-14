{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsDefaultEffort

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.Basis.Double
import Numeric.AERN.RealArithmetic.Interval.Double

import Numeric.AERN.Basics.CInterval

main =
    do
    putStrLn $ "big = " ++ show big ++ " [" ++ show (decodeFloat big) ++ "]"
    putStrLn $ "t1 = " ++ show t1 ++ " [" ++ show (decodeFloat t1) ++ "]"
    putStrLn $ "t2 = " ++ show t2 ++ " [" ++ show (decodeFloat t2L) ++ " , " ++ show (decodeFloat t2H) ++ "]"
    return ()

big = 10E200

t1 :: Double
t1 = 1 +^ big

(t2L, t2H) = getEndpoints t2

t2 :: DI
t2 = 
   (ArithInOut.fromDoubleOutEff () 1) 
   <+> 
   (ArithInOut.fromDoubleOutEff () big)
