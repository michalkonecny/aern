{-|
    Module      :  Numeric.AERN.RealArithmetic.ExactOps
    Description :  access to exact zero and one  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Access to exact zero and one.
-}
module Numeric.AERN.RealArithmetic.ExactOps where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Data.Ratio

class HasZero t where
    zero :: t
    
pNonnegNonposEff effort a =
    case NumOrd.pCompareEff effort a zero of
       Just EQ -> Just (True, True) 
       Just LT -> Just (False, True) 
       Just GT -> Just (True, False)
       Just LEE -> Just (False, True) 
       Just GEE -> Just (True, False)
       _ -> Nothing
    
class HasOne t where
    one :: t
    
class HasInfinities t where
    plusInfinity :: t
    minusInfinity :: t
    excludesPlusInfinity :: t -> Bool
    excludesMinusInfinity :: t -> Bool
    excludesInfinity :: t -> Bool
    excludesInfinity a = 
        excludesMinusInfinity a && excludesPlusInfinity a 
    
class Neg t where
    neg :: t -> t

propNegFlip ::
    (Eq t, Neg t) =>
    t -> t -> Bool
propNegFlip _ e =
    neg (neg e) == e 

-- instances for some common types:

instance HasZero Int where zero = 0
instance HasOne Int where one = 1

instance HasZero Integer where zero = 0
instance HasOne Integer where one = 1

instance (HasZero t, HasOne t, Integral t) => 
    HasZero (Ratio t) 
    where zero = zero % one
instance (HasOne t, Integral t) => 
    HasOne (Ratio t) 
    where one = one % one
