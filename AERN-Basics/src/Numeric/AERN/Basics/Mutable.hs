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

    {-| safely create a new space with the given value -}
    makeMutable :: t -> ST s (Mutable t s)
    {-| create a new space with the given value, making the value volatile -}
    unsafeMakeMutable :: t -> ST s (Mutable t s)
    {-| a safe write/update operation -}
    writeMutable :: Mutable t s -> t -> ST s ()
    {-| an unsafe write/update operation; it makes the second argument volatile -}
    unsafeWriteMutable :: Mutable t s -> t -> ST s ()
    {-| a safe read operation, yielding an immutable value -}
    readMutable :: Mutable t s -> ST s t
    {-| an unsafe read operation, yielding an immutable value that may be volatile -}
    unsafeReadMutable :: Mutable t s -> ST s t

--propWriteRead :: 
--propWriteWriteRead ::
--propWriteWriteReadConcurrent ::
