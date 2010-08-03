{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Numeric.AERN.Basics.Effort

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsDefaultEffort

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.RealArithmetic.Interval.MPFR
import Numeric.AERN.RealArithmetic.Interval.ElementaryDirect

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
--    putStrLn $ "big = " ++ showInternals shouldShowInternals big
--    putStrLn $ "t1 = " ++ showInternals shouldShowInternals t1
--    putStrLn $ "t2 = " ++ showInternals shouldShowInternals t2
    putStrLn $ "exp 1 = \n" ++ (unlines $ map (showInternals shouldShowInternals) exp1efforts)
    putStrLn $ "exp 3 = \n" ++ (unlines $ map (showInternals shouldShowInternals) exp3efforts)
    putStrLn $ "exp 10 = \n" ++ (unlines $ map (showInternals shouldShowInternals) exp10efforts)
    putStrLn $ "exp 100 = \n" ++ (unlines $ map (showInternals shouldShowInternals) exp100efforts)
    putStrLn $ "exp 1000 = \n" ++ (unlines $ map (showInternals shouldShowInternals) exp1000efforts)
    return ()

shouldShowInternals = (30,False)

exp1efforts :: [MI]
exp1efforts =
    map (\n -> ArithInOut.expOutEff (expEffort n) one) [1..20]
    
exp3efforts :: [MI]
exp3efforts =
    map (\n -> ArithInOut.expOutEff (expEffort n) three) [1..20]

exp10efforts :: [MI]
exp10efforts =
    map (\n -> ArithInOut.expOutEff (expEffort n) ten) [1..20]

exp100efforts :: [MI]
exp100efforts =
    map (\n -> ArithInOut.expOutEff (expEffort n) hundred) [1..20]

exp1000efforts :: [MI]
exp1000efforts =
    map (\n -> ArithInOut.expOutEff (expEffort n) thousand) [1..20]

three, ten, hundred, thousand :: MI

[three, ten, hundred, thousand] = 
    map (ArithInOut.convertOutEff 100) ([3,10,100,1000] :: [Int])

    
expEffort n =
    (a, Int1To10 n, c)
    where
    (a, _, c) = expEffortDefault

expEffortDefault = 
   ArithInOut.expDefaultEffortIndicator three


   