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
import Numeric.AERN.Basics.ShowInternals

import Control.Concurrent

main =
    do
--    setMachineRoundingModeUp 
    -- not sure why the above is sometimes needed...
    -- the unsafePerformIO embedded in rounded ops sometimes 
    -- does not work when run standalone, 
    -- but they seem to work when run from ghci 
    putStrLn $ "big = " ++ showInternals shouldShowInternals big
    putStrLn $ "t1 = " ++ showInternals shouldShowInternals t1
    putStrLn $ "t2 = " ++ showInternals shouldShowInternals t2
    return ()

shouldShowInternals = False

big = 10E200

t1 :: Double
t1 = 1 +^ big

(t2L, t2H) = getEndpoints t2

t2 :: DI
t2 = 
   (ArithInOut.fromDoubleOutEff () 1) 
   <+> 
   (ArithInOut.fromDoubleOutEff () big)
