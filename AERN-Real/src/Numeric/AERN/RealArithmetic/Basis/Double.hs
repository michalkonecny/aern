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

import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

instance SemidecidableEq Double where
    maybeEqual _ a b =
        case (isNaN a, isNaN b) of
           (False, False) -> Just $ a == b  
           (True, True) -> Just True
           _ -> Just False
    maybeEqualDefaultEffort _ = []

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
    
