{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.Mutable
    Description :  a type class for ST mutable structures 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    A type class for ST mutable structures.
-}
module Numeric.AERN.Basics.Mutable where

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

--propWriteRead :: 
--propWriteWriteRead ::
--propWriteWriteReadConcurrent ::
