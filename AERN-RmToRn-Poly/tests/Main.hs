
module Main where

import Numeric.AERN.Basics.Mutable

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.Basis.Double

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly as GCPoly
import qualified Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly as DCPoly
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly (Var(..), Size(..))
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Show()
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Show()

import Control.Monad.ST (runST,unsafeIOToST)

import System.IO.Unsafe

--import Foreign.StablePtr

main :: IO ()
main = 
    do
    testPureDCPolys
--    testPureGCPolys
--    testMutableGCPolys

testPureDCPolys :: IO ()
testPureDCPolys =
    do
    putStrLn $ show (maxArity, maxSize)
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
    opsPtr = unsafePerformIO $ DCPoly.newOps DCPoly.Ops_Pure
    p1 = DCPoly.constPoly 3 (Var 2) (Size 10)
    p2 = DCPoly.projectionPoly (Var 0) (Var 2) (Size 10)
    p3 = DCPoly.projectionPoly (Var 1) (Var 2) (Size 10)
    p11 = DCPoly.polyAddUpPureUsingPureOps (Size 2) opsPtr p1 p1
    p12 = DCPoly.polyAddUpPureUsingPureOps (Size 2) opsPtr p1 p2
    p22 = DCPoly.polyAddUpPureUsingPureOps (Size 2) opsPtr p2 p2
    p23 = DCPoly.polyAddUpPureUsingPureOps (Size 2) opsPtr p2 p3
    p1b23 = DCPoly.polyAddUpPureUsingPureOps (Size 2) opsPtr p1 p23
    p1b23s1 = DCPoly.polyAddUpPureUsingPureOps (Size 1) opsPtr p1 p23
    pb223 = DCPoly.polyAddUpPureUsingPureOps (Size 2) opsPtr p22 p3
    p1bb223 = DCPoly.polyAddUpPureUsingPureOps (Size 2) opsPtr p1 pb223
    p1bb223s1 = DCPoly.polyAddUpPureUsingPureOps (Size 1) opsPtr p1 pb223
    (maxArity, maxSize) = DCPoly.peekSizes p1

testPureGCPolys :: IO ()
testPureGCPolys =
    do
    putStrLn $ show (maxArity, maxSize)
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
    opsPtr = GCPoly.newOpsPureArithUpDnDefaultEffort sampleD
    p1 = GCPoly.constPoly (3::Double) (Var 2) (Size 10)
    p2 = GCPoly.projectionPoly sampleD (Var 0) (Var 2) (Size 10)
    p3 = GCPoly.projectionPoly sampleD (Var 1) (Var 2) (Size 10)
    p11 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p1 p1
    p12 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p1 p2
    p22 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p2 p2
    p23 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p2 p3
    p1b23 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p1 p23
    p1b23s1 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 1) opsPtr p1 p23
    pb223 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p22 p3
    p1bb223 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) opsPtr p1 pb223
    p1bb223s1 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 1) opsPtr p1 pb223
    (maxArity, maxSize) = GCPoly.peekSizes p1
        
testMutableGCPolys :: IO ()
testMutableGCPolys =
    do
    arity <- GCPoly.peekArityIO p1
    putStrLn $ "arity = " ++ show arity
    putStrLn $ "p1 = " ++ show p1
    putStrLn $ "p2 = " ++ show p2
    putStrLn $ "p3 = " ++ show p3
    putStrLn $ "p11 = " ++ show p11
    putStrLn $ "p12 = " ++ show p12
    putStrLn $ "p22 = " ++ show p22
    putStrLn $ "p1b23 = " ++ show p1b23
    putStrLn $ "p23s1 = " ++ show p23s1
    putStrLn $ "pb223s1 = " ++ show pb223s1
    where
    opsPtr = GCPoly.newOpsPureArithUpDnDefaultEffort sampleD
    opsMutablePtr = GCPoly.newOpsMutableArithUpDnDefaultEffort sampleD
    [p1,p2,p3,p11,p12,p22,p1b23,p23s1,pb223s1] = runST $
        do
        p1M <- GCPoly.constPolyMutable (3::Double) (Var 2) (Size 10)
        p2M <- GCPoly.projectionPolyMutable sampleD (Var 0) (Var 2) (Size 10)
        p3M <- GCPoly.projectionPolyMutable sampleD (Var 1) (Var 2) (Size 10)
        p11M <- GCPoly.constPolyMutable (0::Double) (Var 2) (Size 2)
--        unsafeIOToST $ putStrLn $ "A"
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p11M p1M p1M
--        unsafeIOToST $ putStrLn $ "A"
        p12M <- GCPoly.constPolyMutable (0::Double) (Var 2) (Size 2)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p12M p1M p2M
        p22M <- GCPoly.constPolyMutable (0::Double) (Var 2) (Size 2)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p22M p2M p2M
        p1b23M <- GCPoly.constPolyMutable (0::Double) (Var 2) (Size 2)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p1b23M p2M p3M
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p1b23M p1M p1b23M
        p23s1M <- GCPoly.constPolyMutable (0::Double) (Var 2) (Size 1)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p23s1M p2M p3M
        pb223s1M <- GCPoly.constPolyMutable (0::Double) (Var 2) (Size 1)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr pb223s1M p22M p3M
        mapM (GCPoly.unsafeReadPolyMutable sampleD) [p1M, p2M, p3M, p11M, p12M, p22M, p1b23M, p23s1M, pb223s1M]
    
