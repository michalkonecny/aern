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

import Control.Monad.ST
import Data.STRef

class CanBeMutable t where
    {-| a mutable version of the type t -}
    type Mutable t :: *
    {-| a monad that the mutable version of t can be modified in -}
    type MutableMonad t :: * -> *

    {-| a thaw operation to make a variable with an initial value -}
    makeMutable :: t -> MutableMonad t (Mutable t)
    {-| a read operation, yielding an immutable value -}
    readMutable :: Mutable t -> MutableMonad t t
    {-| a write/update operation -}
    writeMutable :: Mutable t -> t -> MutableMonad t ()
    {-| execute imperative program involving values of b 
        and escape back to the pure world with the results -}
    runMutable :: MutableMonad t [Mutable t] -> [t]  
-- ??    runMutable :: MutableMonad t a -> a  


{-|
  The bit size of the floating point numbers (or similar)
  used internally in real number and function approximations.
-}
type Granularity = Int
