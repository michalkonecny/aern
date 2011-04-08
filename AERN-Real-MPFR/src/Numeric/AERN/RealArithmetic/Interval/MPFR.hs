{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.MPFR
    Description :  Interval MPFR utilities and tests  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Interval MPFR utilities and tests.
-}
module Numeric.AERN.RealArithmetic.Interval.MPFR 
where

import Numeric.AERN.Basics.Interval

import Data.Number.MPFR (MPFR)
import Data.Number.MPFR.Instances.Up

import Numeric.AERN.RealArithmetic.Basis.MPFR.ShowInternals
import Numeric.AERN.RealArithmetic.Basis.MPFR.NumericOrder

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck

type MI = Interval MPFR

sampleMI :: MI
sampleMI = Interval 0 0

newtype PositiveMI = PositiveMI { unPositiveMI :: MI }

instance Show PositiveMI where
    show (PositiveMI i) = show i

instance Arbitrary PositiveMI
    where
    arbitrary =
        do
        NumOrd.UniformlyOrderedPair (l,h) <- arbitrary
        return $ PositiveMI (Interval (pos l) (pos h))
        where
        pos e 
            | e > 0 =  e
            | e == 0 =  1
            | otherwise = (-e) 
