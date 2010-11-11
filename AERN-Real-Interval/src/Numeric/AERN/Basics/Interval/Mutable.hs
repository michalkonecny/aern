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

data MInterval e s = 
    MInterval { mIntervalLeft :: e s, mIntervalRight :: e s }

instance (CanBeMutable e) => CanBeMutable (Interval e) where
    type Mutable (Interval e) = MInterval (Mutable e)
    makeMutable (Interval l h) = 
        do
        lM <- makeMutable l
        hM <- makeMutable h
        return $ MInterval lM hM
    unsafeMakeMutable (Interval l h) = 
        do
        lM <- unsafeMakeMutable l
        hM <- unsafeMakeMutable h
        return $ MInterval lM hM
    writeMutable (MInterval lM hM) (Interval l h) =
        do
        writeMutable lM l
        writeMutable hM h
    unsafeWriteMutable (MInterval lM hM) (Interval l h) =
        do
        unsafeWriteMutable lM l
        unsafeWriteMutable hM h
    readMutable (MInterval lM hM) =
        do
        l <- readMutable lM 
        h <- readMutable hM 
        return $ Interval l h
    unsafeReadMutable (MInterval lM hM) =
        do
        l <- unsafeReadMutable lM 
        h <- unsafeReadMutable hM 
        return $ Interval l h

        
