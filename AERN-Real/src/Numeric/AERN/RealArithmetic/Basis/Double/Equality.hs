{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder
    Description :  equality instances for Double  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Equality instances for Double.
    
    This is a private module reexported publicly via its parent.
-}
module Numeric.AERN.RealArithmetic.Basis.Double.Equality 
(
   testsDoubleEq, testsDoubleSemidecidableEq 
)
where

import Numeric.AERN.Basics.Equality

import Numeric.AERN.Basics.Exception
import Control.Exception

import Data.Maybe
import Data.List (sort)

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)


propDoubleEqReflexive :: Double -> Bool
propDoubleEqReflexive = propEqReflexive 

propDoubleEqSymmetric :: Double -> Double -> Bool
propDoubleEqSymmetric = propEqSymmetric

propDoubleEqTransitive :: Double -> Double -> Double -> Bool
propDoubleEqTransitive = propEqTransitive

testsDoubleEq :: Test
testsDoubleEq =
    testGroup "Double (==)" 
        [
         testProperty "reflexive" propDoubleEqReflexive
        ,
         testProperty "symmetric" propDoubleEqSymmetric
        ,
         testProperty "transitive" propDoubleEqTransitive
        ]

instance SemidecidableEq Double where
    maybeEqualEff _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Just $ a == b  
           _ -> throw (AERNException $ "illegal Double argument: maybeEqual " 
                        ++ show a ++ " " ++ show b)
    maybeEqualDefaultEffort _ = []

propDoubleSemidecidableEqReflexive :: Double -> Bool
propDoubleSemidecidableEqReflexive = propSemidecidableEqReflexive

propDoubleSemidecidableEqSymmetric :: Double -> Double -> Bool
propDoubleSemidecidableEqSymmetric = propSemidecidableEqSymmetric

propDoubleSemidecidableEqTransitive :: Double -> Double -> Double -> Bool
propDoubleSemidecidableEqTransitive = propSemidecidableEqTransitive

testsDoubleSemidecidableEq :: Test
testsDoubleSemidecidableEq =
    testGroup "Double (==?)" 
        [
         testProperty "reflexive" propDoubleSemidecidableEqReflexive
        ,
         testProperty "symmetric" propDoubleSemidecidableEqSymmetric
        ,
         testProperty "transitive" propDoubleSemidecidableEqTransitive
        ]

