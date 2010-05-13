{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
--import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsDefaultEffort

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.Basis.Double
import Numeric.AERN.RealArithmetic.Interval.Double

main =
    do
--    putStrLn $ "t1 = " ++ show t1
--    putStrLn $ "t2 = " ++ show t2
    return ()

--t1 :: Double
--t1 = 1 +^ 10E100
--
--t2 :: DI
--t2 = 1 <+> 10E100
