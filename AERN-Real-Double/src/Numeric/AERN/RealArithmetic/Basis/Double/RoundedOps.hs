{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.RoundedOps
    Description :  rounded arithmetic instances for Double
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded arithmetic instances for Double.
    
    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RealArithmetic.Basis.Double.RoundedOps 
()
where

import Numeric.AERN.RealArithmetic.Basis.Double.ExactOperations

import Numeric.AERN.RealArithmetic.NumericOrderRounding

import Numeric.AERN.Basics.Effort

import Numeric.IEEE.RoundMode
import System.IO.Unsafe

import Numeric.AERN.Basics.Exception
import Control.Exception

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

detectNaN :: String -> Double -> Double
detectNaN msg a 
    | isNaN a =
        throw (AERNNaNException $ "NaN in " ++ msg)
    | otherwise = a

instance RoundedAdd Double where
    type AddEffortIndicator Double = () 
    addDefaultEffort _ = ()
    addUpEff effort d1 d2 = 
        detectNaN ("addition " ++ show d1 ++ " +^ " ++ show d2 ) $ 
            withUpwardsRounding $ d1 + d2
    addDnEff effort d1 d2 =
        detectNaN ("addition " ++ show d1 ++ " +. " ++ show d2 ) $ 
            negate $ withUpwardsRounding $ (negate d1) + (negate d2)
    -- the following is an exagerated rounding version meant for testing:
--    type AddEffortIndicator Double = Int1To100 
--    addDefaultEffort _ = Int1To100 10
--    addUpEff effort d1 d2 = 
--        withUpwardsRounding $ 
--            d1 + d2 + (1/effortD)
--        where
--        effortD = fromInteger $ toInteger $ fromInt1To100 $ effort
--    addDnEff effort d1 d2 = 
--        negate $ withUpwardsRounding $ 
--            (negate d1) + (negate d2) + (1/effortD)
--        where
--        effortD = fromInteger $ toInteger $ fromInt1To100 $ effort

    
instance RoundedSubtr Double

instance RoundedAbs Double where
    type AbsEffortIndicator Double = ()
    absDefaultEffort _ = ()
    absDnEff _ = abs
    absUpEff _ = abs

instance RoundedMultiply Double where
    type MultEffortIndicator Double = () 
    multDefaultEffort _ = ()
    multUpEff effort d1 d2 = 
        detectNaN ("multiplication " ++ show d1 ++ " *^ " ++ show d2 ) $ 
            withUpwardsRounding $ d1 * d2
    multDnEff effort d1 d2 = 
        detectNaN ("multiplication " ++ show d1 ++ " *. " ++ show d2 ) $ 
            negate $ withUpwardsRounding $ (negate d1) * d2
    -- the following is an exagerated rounding version meant for testing:
--    type MultEffortIndicator Double = Int1To100 
--    multDefaultEffort _ = Int1To100 10
--    multUpEff effort d1 d2 = 
--        withUpwardsRounding $ 
--            d1 * d2 + (1/effortD)
--        where
--        effortD = fromInteger $ toInteger $ fromInt1To100 $ effort
--    multDnEff effort d1 d2 = 
--        negate $ withUpwardsRounding $ 
--            (negate d1) * d2 + (1/effortD)
--        where
--        effortD = fromInteger $ toInteger $ fromInt1To100 $ effort

instance RoundedDivide Double where
    type DivEffortIndicator Double = () 
    divDefaultEffort _ = ()
    divUpEff effort d1 d2 = 
        detectNaN ("division " ++ show d1 ++ " /^ " ++ show d2 ) $ 
            withUpwardsRounding $ d1 / d2
    divDnEff effort d1 d2 = 
        detectNaN ("division " ++ show d1 ++ " /. " ++ show d2 ) $ 
            negate $ withUpwardsRounding $ (negate d1) / d2

--test1 :: [Double]
--test1 =
--    let ?multUpDnEffort = () :: MultEffortIndicator Double in
--    let up = foldl1 (*^) [1/3,1/3,1/3]
--        dn = foldl1 (*.) [1/3,1/3,1/3]
--    in 
--    [up, dn]
    

    