{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.BasicTypes
    Description :  auxiliary types for exact real number processing 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    auxiliary types and classes for exact real number processing
-}
module Numeric.AERN.BasicTypes where

import Control.Monad.ST (ST)

class CanBeMutable t where
    {-| a mutable version of the type t; 
        the extra parameter is the state of the ST monad run -}
    type Mutable t :: * -> *

    {-| a thaw operation to make a variable with an initial value -}
    makeMutable :: t -> ST s (Mutable t s)
    {-| a read operation, yielding an immutable value -}
    readMutable :: Mutable t s -> ST s t
    {-| a write/update operation -}
    writeMutable :: Mutable t s -> t -> ST s ()


{-|
  The bit size of the floating point numbers (or similar)
  used internally in real number and function approximations.
-}
type Granularity = Int
