{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Basics.Interval.Mutable
    Description :  mutable version of Interval
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Mutable version of Interval.
    
    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.Basics.Interval.Mutable where

import Numeric.AERN.Basics.Interval.Basics
import Numeric.AERN.Basics.Mutable

import Numeric.AERN.RealArithmetic.ExactOps

instance (CanBeMutable e) => CanBeMutable (Interval e) where
    data Mutable (Interval e) s = 
        MInterval { mIntervalLeft :: Mutable e s, mIntervalRight :: Mutable e s }
    makeMutable (Interval l r) = 
        do
        lM <- makeMutable l
        rM <- makeMutable r
        return $ MInterval lM rM
    unsafeMakeMutable (Interval l r) = 
        do
        lM <- unsafeMakeMutable l
        rM <- unsafeMakeMutable r
        return $ MInterval lM rM
    writeMutable (MInterval lM rM) (Interval l r) =
        do
        writeMutable lM l
        writeMutable rM r
    unsafeWriteMutable (MInterval lM rM) (Interval l r) =
        do
        unsafeWriteMutable lM l
        unsafeWriteMutable rM r
    readMutable (MInterval lM rM) =
        do
        l <- readMutable lM 
        r <- readMutable rM 
        return $ Interval l r
    unsafeReadMutable (MInterval lM rM) =
        do
        l <- unsafeReadMutable lM 
        r <- unsafeReadMutable rM 
        return $ Interval l r
