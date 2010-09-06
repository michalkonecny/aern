
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
    opsPtr <- newOps ops
    p1 <- newConstPoly (3::Double) (Var 2) (Size 10)
    p2 <- newProjectionPoly sampleD (Var 0) (Var 2) (Size 10)
    p3 <- newProjectionPoly sampleD (Var 1) (Var 2) (Size 10)
    p11 <- polyAddUp sampleD (Size 2) opsPtr p1 p1
    p12 <- polyAddUp sampleD (Size 2) opsPtr p1 p2
    p22 <- polyAddUp sampleD (Size 2) opsPtr p2 p2
    p23 <- polyAddUp sampleD (Size 2) opsPtr p2 p3
    p1b23 <- polyAddUp sampleD (Size 2) opsPtr p1 p23
    p1b23s1 <- polyAddUp sampleD (Size 1) opsPtr p1 p23
    pb223 <- polyAddUp sampleD (Size 2) opsPtr p22 p3
    p1bb223 <- polyAddUp sampleD (Size 2) opsPtr p1 pb223
    p1bb223s1 <- polyAddUp sampleD (Size 1) opsPtr p1 pb223
--    (maxArity, maxSize) <- peekSizes p2FP
--    putStrLn $ show (maxArity, maxSize)
    putStrLn $ "p1 = " ++ show p1
    putStrLn $ "p2 = " ++ show p2
    putStrLn $ "p3 = " ++ show p3
    putStrLn $ "p1 +^ p1 = " ++ show p11
    putStrLn $ "p1 +^ p2 = " ++ show p12
    putStrLn $ "p2 +^ p2 = " ++ show p22
    putStrLn $ "p2 +^ p3 = " ++ show p23
    putStrLn $ "p1 +^ (p2 +^ p3) = " ++ show p1b23
    putStrLn $ "size 1 $ p1 +^ (p2 +^ p3) = " ++ show p1b23s1
    putStrLn $ "(p2 +^ p2) +^ p3 = " ++ show pb223
    putStrLn $ "p1 +^ ((p2 +^ p2) +^ p3) = " ++ show p1bb223
    putStrLn $ "size 1 $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show p1bb223s1
    