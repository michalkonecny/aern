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

import Control.Monad.ST
import Data.STRef

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.Basics.Mutable

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
    
class (Neg t, CanBeMutable t) => NegInPlace t where
    negInPlace :: t -> Mutable t s -> ST s ()
    negInPlace sample var =
        -- default such as this one is very inefficient
        -- but facilitates an API that works even for
        -- types that do not have native in-place updates
        do
        a <- readMutable var
        let _ = [a,sample]
        writeMutable var (neg a)

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
