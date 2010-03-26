{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double
    Description :  Instances for Double as interval endpoints.  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable (indirect FFI)
    
    Instances of Double required for serving as interval endpoints,
    namely providing granularity, Comparison, lattice, rounded field and 
    rounded elementary operations.
-}
module Numeric.AERN.RealArithmetic.Basis.Double 
(
   module Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder
)
where

import Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder

import Numeric.AERN.Basics.Granularity

import Numeric.IEEE.RoundMode

instance HasGranularity Double where
    type Granularity Double = Int
    getGranularity _ = 53
    initGranularityRounding _ = initMachineDouble

{-|
    Set machine floating point unit to the upwards-directed rounding
    mode.  
    
    This procedure has to be executed before using 'Double' 
    as a basis for interval and polynomial arithmetic defined in the AERN
    packages.
-}
initMachineDouble :: IO ()
initMachineDouble =
    do
    putStr "initialising Double rounding mode: "
    currentRndMode <- getRound
    case currentRndMode == Upward of
        True ->
            putStrLn "already rounding upwards"
        False ->
            do
            success <- setRound Upward
            case success of
                True -> 
                   putStrLn "switching to upwards rounding"
                False -> 
                   error "Numeric.AERN.RealArithmetic.Basics.Double: failed to switch rounding mode"

