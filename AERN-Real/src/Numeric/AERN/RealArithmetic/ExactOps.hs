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
    (nonneg, nonpos)
    where
    (_, nonneg, _, nonpos) =
        pPosNonnegNegNonposEff effort a
    
pPosNonnegNegNonposEff effort a =
    case NumOrd.pCompareEff effort a zero of
       Just EQ -> (Just False, Just True, Just False, Just True) 
       Just LT -> (Just False, Just False, Just True, Just True) 
       Just GT -> (Just True, Just True, Just False, Just False)
       Just LEE -> (Just False, Nothing, Nothing, Just True) 
       Just GEE -> (Nothing, Just True, Just False, Nothing)
       _ -> (Nothing, Nothing, Nothing, Nothing)
    
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
    negInPlace :: t -> OpMutable1 t s 
    negInPlace sample =
        pureToMutable1 neg
--        
--        -- default such as this one is very inefficient
--        -- but facilitates an API that works even for
--        -- types that do not have native in-place updates
--        do
--        a <- readMutable aM
--        let _ = [a,sample]
--        writeMutable rM (neg a)

propNegFlip ::
    (Eq t, Neg t) =>
    t -> t -> Bool
propNegFlip _ e =
    neg (neg e) == e 

-- instances for some common types:

instance HasZero Int where zero = 0
instance HasOne Int where one = 1
instance Neg Int where neg = negate

instance HasZero Integer where zero = 0
instance HasOne Integer where one = 1
instance Neg Integer where neg = negate

instance (HasZero t, HasOne t, Integral t) => 
    HasZero (Ratio t) 
    where zero = zero % one
instance (HasOne t, Integral t) => 
    HasOne (Ratio t) 
    where one = one % one
instance (Integral t) => Neg (Ratio t) where neg = negate

instance HasZero Double where zero = 0
instance HasOne Double where one = 1
instance Neg Double where neg = negate

instance HasInfinities Double where
    plusInfinity = 1/0
    minusInfinity = -1/0
    excludesPlusInfinity a = (a /= plusInfinity)
    excludesMinusInfinity a = (a /= minusInfinity)
    

