
module Main where

import Numeric.AERN.Basics.Mutable

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.Basis.Double

import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.FFIhelper
import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Show()

import Control.Monad.ST (runST)

--import Foreign.StablePtr

main :: IO ()
main = 
    do
--    testPurePolys
    testMutablePolys
--    testAssignST

testPurePolys :: IO ()
testPurePolys =
    do
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
    where
    opsPtr = newOpsPureArithUpDnDefaultEffort sampleD
    p1 = constPoly (3::Double) (Var 2) (Size 10)
    p2 = projectionPoly sampleD (Var 0) (Var 2) (Size 10)
    p3 = projectionPoly sampleD (Var 1) (Var 2) (Size 10)
    p11 = polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p1 p1
    p12 = polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p1 p2
    p22 = polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p2 p2
    p23 = polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p2 p3
    p1b23 = polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p1 p23
    p1b23s1 = polyAddUpPureUsingPureOps sampleD (Size 1) opsPtr p1 p23
    pb223 = polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p22 p3
    p1bb223 = polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p1 pb223
    p1bb223s1 = polyAddUpPureUsingPureOps sampleD (Size 1) opsPtr p1 pb223
--    (maxArity, maxSize) = peekSizes p2
        
testMutablePolys :: IO ()
testMutablePolys =
    do
    arity <- peekArityIO p1
    putStrLn $ "arity = " ++ show arity
    putStrLn $ "p1 = " ++ show p1
    putStrLn $ "p2 = " ++ show p2
    putStrLn $ "p3 = " ++ show p3
    putStrLn $ "p11 = " ++ show p11
    putStrLn $ "p12 = " ++ show p12
    putStrLn $ "pb123 = " ++ show pb123
    where
    opsPtr = newOpsPureArithUpDnDefaultEffort sampleD
    opsMutablePtr = newOpsMutableArithUpDnDefaultEffort sampleD
    [p1,p2,p3,p11,p12,pb123] = runST $
        do
        p1M <- constPolyMutable (3::Double) (Var 2) (Size 10)
        p2M <- projectionPolyMutable sampleD (Var 0) (Var 2) (Size 10)
        p3M <- projectionPolyMutable sampleD (Var 1) (Var 2) (Size 10)
        p11M <- constPolyMutable (0::Double) (Var 2) (Size 2)
        polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p11M p1M p1M
        p12M <- constPolyMutable (0::Double) (Var 2) (Size 2)
        polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p12M p1M p2M
        pb123M <- constPolyMutable (0::Double) (Var 2) (Size 2)
        polyAddUpMutableUsingMutableOps sampleD opsMutablePtr pb123M p1M p2M
        polyAddUpMutableUsingMutableOps sampleD opsMutablePtr pb123M pb123M p3M
        mapM (unsafeReadPolyMutable sampleD) [p1M, p2M, p3M, p11M, p12M, pb123M]
    
testAssignST =
    do
    putStrLn $ "after testAssign (v1=0) (v2=1) we have: v1 = " ++ show v1 ++ "; v2 = " ++ show v2
    where
    (v1,v2) = 
        runST $
            do
            m1 <- makeMutable (0 :: Double)
            m2 <- makeMutable (1 :: Double)
            testAssign sampleD m1 m2
            v1 <- readMutable m1
            v2 <- readMutable m2
            return (v1 :: Double, v2 :: Double)
    