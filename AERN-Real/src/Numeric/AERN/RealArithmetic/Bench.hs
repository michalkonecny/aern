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

import Numeric.AERN.Misc.Debug

mkCommentImprecision op effort a =
    unsafePrint
    (
        "mkCommentImprecision: " 
        ++ "\n a = " ++ show a
        ++ "\n aD = " ++ show aD
        ++ "\n aExp = " ++ show (exponent aD)
        ++ "\n effort = " ++ show effort
        ++ "\n result = " ++ show result
        ++ "\n imprecision = " ++ show imprecision
        ++ "\n imprecisionD = " ++ show imprecisionD
        ++ "\n imprecisionExp = " ++ show (exponent imprecisionD)
    ) $
    "x" ++ show (exponent aD) ++ "ri" ++ show (exponent imprecisionD)
    where
    imprecisionD, aD :: Double
    Just aD =
        ArithUpDn.convertUpEff (ArithUpDn.convertDefaultEffort a sampleD) a
    Just imprecisionD =
        ArithUpDn.convertUpEff (ArithUpDn.convertDefaultEffort imprecision sampleD) imprecision
    imprecision =
        imprecisionOfEff (imprecisionDefaultEffort a) result 
    result = op effort a
    sampleD :: Double
    sampleD = 0 
