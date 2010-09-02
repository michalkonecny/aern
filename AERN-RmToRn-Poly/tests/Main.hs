
module Main where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.Basis.Double

import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.FFIhelper
import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Show()

--import Foreign.StablePtr

main :: IO ()
main =
    do
    ops <- mkOpsPureArithUpDn sampleD (ArithUpDn.roundedRealDefaultEffort sampleD)
--    one <- deRefStablePtr $ ops_one $ ops 
--    putStrLn $ show $ one
    opsPtr <- newOps ops 
    p1FP <- newConstPoly (1::Double) (Var 2) (Size 10)
    p2FP <- newProjectionPoly opsPtr (Var 0) (Var 2) (Size 10)
    (maxArity, maxSize) <- peekSizes p2FP
    putStrLn $ show (maxArity, maxSize)
    putStrLn $ "p1 = " ++ show p1FP
    putStrLn $ "p2 = " ++ show p2FP
    