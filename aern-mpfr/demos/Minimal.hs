{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Numeric.AERN.Basics.Effort

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.Operators

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.Operators

import Numeric.AERN.RealArithmetic.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsDefaultEffort

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.Basis.MPFR
import Numeric.AERN.RealArithmetic.Interval.MPFR
--import Numeric.AERN.RealArithmetic.Interval.ElementaryFromBasis
import Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.ShowInternals

import Control.Concurrent

main =
    do
    putStrLn $ "oo - oo = " ++ showInternals shouldShowInternals inftyMinusInftyOut
    putStrLn $ "0</>0 = " ++ showInternals shouldShowInternals zeroOverZeroOut
    putStrLn $ "0>/<0 = " ++ showInternals shouldShowInternals zeroOverZeroIn
    putStrLn $ "exp 1 = \n" ++ (unlines $ map (showInternals shouldShowInternals) exp1efforts)
    putStrLn $ "exp 3 = \n" ++ (unlines $ map (showInternals shouldShowInternals) exp3efforts)
    putStrLn $ "exp 10 = \n" ++ (unlines $ map (showInternals shouldShowInternals) exp10efforts)
    putStrLn $ "exp 100 = \n" ++ (unlines $ map (showInternals shouldShowInternals) exp100efforts)
    putStrLn $ "exp 1000 = \n" ++ (unlines $ map (showInternals shouldShowInternals) exp1000efforts)
    return ()

shouldShowInternals = (30,False)

inftyMinusInftyOut :: MI
inftyMinusInftyOut =
    let ?addDefaultEffort = 100 in
    plusInfinity sampleMI <-> plusInfinity sampleMI

zeroOverZeroOut :: MI
zeroOverZeroOut =
    let ?divDefaultEffort = 100 in
    zero sampleMI </> zero sampleMI

zeroOverZeroIn :: MI
zeroOverZeroIn =
    let ?divDefaultEffort = 100 in
    zero sampleMI >/< zero sampleMI

oneOverZeroOut :: MI
oneOverZeroOut =
    let ?divDefaultEffort = 100 in
    one sampleMI </> zero sampleMI

oneOverZeroIn :: MI
oneOverZeroIn =
    let ?divDefaultEffort = 100 in
    one sampleMI >/< zero sampleMI

exp1efforts :: [MI]
exp1efforts =
    map (\n -> ArithInOut.expOutEff (expEffort n) (one sampleMI)) [1..20]
    
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
    map (ArithInOut.convertOutEff 100 sampleMI) ([3,10,100,1000] :: [Int])

    
expEffort n =
    (a, Int1To10 n)
    where
    (a, _) = expEffortDefault

expEffortDefault = 
   ArithInOut.expDefaultEffort three


   