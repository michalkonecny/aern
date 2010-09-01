{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder
    Description :  numeric order instances for Double  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Numeric order Comparison and lattice instances for Double.
    
    This is a private module reexported publicly via its parent.
-}
module Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder
where

import Prelude hiding (EQ,LT,GT)

import Numeric.AERN.Basics.Exception
import Control.Exception

import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck
import Numeric.AERN.Misc.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Numeric.AERN.Misc.List
import Numeric.AERN.Misc.Debug

import Data.Maybe

sampleD :: Double
sampleD = 0

nanD :: Double
nanD = 0/0

instance NumOrd.HasLeast Double where
    least = - 1/0

instance NumOrd.HasHighest Double where
    highest = 1/0

instance NumOrd.HasExtrema Double where

instance NumOrd.RoundedLattice Double where
    type NumOrd.MinmaxEffortIndicator Double = ()
    minmaxDefaultEffort _ = ()
    maxUpEff _ a b = Prelude.max a b
    maxDnEff = NumOrd.maxUpEff 
    minUpEff _ a b = Prelude.min a b
    minDnEff = NumOrd.minUpEff 
    



       

