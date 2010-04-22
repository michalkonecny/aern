{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.NumericOrderRounding
    Description :  rounded arithmetic instances for Double
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded arithmetic instances for Double.
    
    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.Double.NumericOrderRounding 
()
where

import Numeric.AERN.RealArithmetic.Basis.Double.ExactOperations

import Numeric.AERN.RealArithmetic.NumericOrderRounding

import Numeric.IEEE.RoundMode
import System.IO.Unsafe

withUpwardsRounding :: a -> a
withUpwardsRounding a =
    unsafePerformIO $ 
        do 
        setMachineRoundingModeUp
        aa <- return $! a
        return aa

setMachineRoundingModeUp :: IO ()
setMachineRoundingModeUp =
    do
    currentRndMode <- getRound
    case currentRndMode == Upward of
        True ->
            return ()
        False ->
            do
            success <- setRound Upward
            case success of
                True -> 
                   return ()
                False -> 
                   error "Numeric.AERN.RealArithmetic.Basics.Double: failed to switch rounding mode"

instance RoundedAdd Double where
    type AddEffortIndicator Double = () 
    addUpEff effort d1 d2 = withUpwardsRounding $ d1 + d2
    addDnEff effort d1 d2 = negate $ withUpwardsRounding $ (negate d1) + (negate d2)
    addDefaultEffort _ = ()

    
instance RoundedSubtr Double

instance RoundedMultiply Double where
    type MultEffortIndicator Double = () 
    multUpEff effort d1 d2 = withUpwardsRounding $ d1 * d2
    multDnEff effort d1 d2 = negate $ withUpwardsRounding $ (negate d1) * d2
    multDefaultEffort _ = ()

--test1 :: [Double]
--test1 =
--    let ?multUpDnEffort = () :: MultEffortIndicator Double in
--    let up = foldl1 (*^) [1/3,1/3,1/3]
--        dn = foldl1 (*.) [1/3,1/3,1/3]
--    in 
--    [up, dn]
    

    