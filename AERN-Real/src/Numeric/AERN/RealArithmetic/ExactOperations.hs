{-|
    Module      :  Numeric.AERN.RealArithmetic.ExactOperations
    Description :  access to exact zero and one  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Access to exact zero and one.
-}
module Numeric.AERN.RealArithmetic.ExactOperations where

class HasZero t where
    zero :: t
    
class HasOne t where
    one :: t
    
class Neg t where
    neg :: t -> t

propNegFlip ::
    (Eq t, Neg t) =>
    t -> t -> Bool
propNegFlip _ e =
    neg (neg e) == e 

