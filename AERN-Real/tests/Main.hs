{-|
    Module      :  Main
    Description :  run all tests defined in the AERN-Real package  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Main where

import Numeric.AERN.RealArithmetic.Basis.Double

import Test.Framework (defaultMain)

main = defaultMain tests

tests = 
    [
       testsDoubleEq, testsDoubleSemidecidableEq,
       testsDoublePoset, testsDoubleSemidecidablePoset
    ]