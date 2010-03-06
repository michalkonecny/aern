{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double
    Description :  instances of Double required for serving as interval endpoints  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.RealArithmetic.Basis.Double where

import Prelude hiding (EQ)

import Numeric.AERN.Basics.Granularity
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

instance HasGranularity Double where
    type Granularity Double = Int
    getGranularity _ = 53

instance SemidecidableEq Double where
    maybeEqual _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Just $ a == b  
           (True, True) -> Just True
           _ -> Just False
    maybeEqualDefaultEffort _ = []

instance NumOrd.HasLeast Double where
    least = - 1/0

instance NumOrd.HasHighest Double where
    highest = 1/0

instance NumOrd.SemidecidablePoset Double where
    maybeCompare _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Just $ toPartialOrdering $ Prelude.compare a b  
           (True, True) -> Just EQ
           _ -> Just NC 
    maybeCompareDefaultEffort _ = []

instance NumOrd.Poset Double where
    compare a b =
        case (isNaN a, isNaN b) of
           (False, False) -> toPartialOrdering $ Prelude.compare a b  
           (True, True) -> EQ
           _ -> NC 

instance NumOrd.Lattice Double where
    max a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Prelude.max a b  
           _ -> 0/0 -- ie NaN 
    min a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Prelude.min a b  
           _ -> 0/0 -- ie NaN
    
instance NumOrd.RoundedLattice Double where
    maxUpEff _ = max
    maxDnEff _ = max
    minUpEff _ = min
    minDnEff _ = min
    minmaxDefaultEffort _ = []
    