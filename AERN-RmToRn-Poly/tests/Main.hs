
module Main where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.Basis.Double

import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.FFIhelper

main :: IO ()
main =
    do
    ops <- opsRealArithmeticBasis sampleD (ArithUpDn.roundedRealDefaultEffort sampleD) 
    p1FP <- newConstPoly (1::Double) (Var 2) (Size 10)
    p2FP <- newProjectionPoly ops (Var 0) (Var 2) (Size 10)
    (maxArity, maxSize) <- peekSizes p2FP
    putStrLn $ show (maxArity, maxSize)
