{-|
    Module      :  Numeric.AERN.RealArithmetic.Bench
    Description :  benchmarking utilities  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Benchmarking utilities.
-}

module Numeric.AERN.RealArithmetic.Bench where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.RealArithmetic.Measures

mkCommentImprecision op effort a =
    "x" ++ show (exponent aD) ++ "ri" ++ show (exponent imprecisionD)
    where
    imprecisionD, aD :: Double
    Just aD =
        ArithUpDn.convertUpEff (ArithUpDn.convertDefaultEffort a sampleD) a
    Just imprecisionD =
        ArithUpDn.convertUpEff (ArithUpDn.convertDefaultEffort imprecision sampleD) imprecision
    imprecision =
        imprecisionOfEff (imprecisionDefaultEffort a) $ op effort a
    sampleD :: Double
    sampleD = 0 
