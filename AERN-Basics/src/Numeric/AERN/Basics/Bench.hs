{-|
    Module      :  Numeric.AERN.Basics.Bench
    Description :  miscellaneous utilities for benchmarking  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Miscellaneous utilities for benchmarking using Criterion.
-}
module Numeric.AERN.Basics.Bench where

import Numeric.AERN.Basics.Effort 

import Criterion
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random

mkBenchSequence1 mkComment fnEff initEffort sample =
    map mkBench $ zip3 [1..20] efforts inputs 
    where
    mkBench (n, effort, input) =
        bench name (nf (\(e,i) -> fnEff e i) (effort, input))
        where
        name =
           "" ++ showPad 2 n ++ ":" ++ mkComment effort input ++ ""
    showPad l n = 
        replicate (max 0 (l - (length sn))) '0' ++ sn 
        where sn = show n
    _ = sample : inputs
    inputs = 
        map (\(g, size) -> unGen arbitrary g size) $ zip gs sizes
        -- get a sample sequence (always the same!)  
    gs = 
        iterate (snd . next) $ mkStdGen 1587346765
        -- sequence of random generators
    sizes = 
        concat $ map (replicate 3) $ scanl1 (*) [2,2..] 
        -- ie [2,2,2,4,4,4,8,8,8..]
    efforts = 
        concat $ map (replicate 10) $ effortIncrementSequence initEffort

mkBenchSequence2 mkComment fnEff initEffort sample1 sample2 =
    map mkBench $ zip3 [1..20] efforts inputs
    where
    mkBench (n, effort, (input1, input2)) =
        bench name (nf (\(e,i1,i2) -> fnEff e i1 i2) (effort, input1, input2))
        where
        name =
           "(" ++ showPad 2 n ++ ": " ++ mkComment effort input1 input2 ++ ")"
    showPad l n = 
        replicate (max 0 (l - (length sn))) '0' ++ sn 
        where sn = show n
    inputs = zip inputs1 inputs2
    _ = sample1 : inputs1
    inputs1 = 
        map (\(g, size) -> unGen arbitrary g size) $ zip gs1 sizes
    gs1 = 
        iterate (snd . next) $ mkStdGen 1587346765
    _ = sample2 : inputs2
    inputs2 = 
        map (\(g, size) -> unGen arbitrary g size) $ zip gs2 sizes
    gs2 = 
        iterate (snd . next) $ mkStdGen 658246234
    sizes = 
        concat $ map (replicate 3) $ scanl1 (*) [2,2..] 
    efforts = 
        concat $ map (replicate 10) $ effortIncrementSequence initEffort 

    