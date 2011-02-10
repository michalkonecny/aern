
module Main where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.Basis.Double

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Mutable

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly as GCPoly
import qualified Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly as DCPoly
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly (Var(..), Size(..), Power(..))
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Show()
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Show()

import Control.Monad.ST (runST,unsafeIOToST)

import System.IO.Unsafe

import Foreign.StablePtr
import System.Mem

main :: IO ()
main = 
    do
    testPureDCPolys
--    testPureGCPolys
--    testMutableGCPolys

testPureDCPolys :: IO ()
testPureDCPolys =
    do
    putStrLn $ "p1.maxArity = " ++ show maxArity
    putStrLn $ "p1.maxSize = " ++ show maxSize
    putStrLn $ "p1.maxDegree = " ++ show maxDegree
    putStrLn $ "p1.constTerm = " ++ show constTerm
    putStrLn $ "p1 = " ++ showP p1
    putStrLn $ "p2 = " ++ showP p2
    putStrLn $ "p3 = " ++ showP p3
    putStrLn $ "p1 +^ p1 = " ++ showP p11
    putStrLn $ "p1 +^ p2 = " ++ showP p12
    putStrLn $ "p2 +^ p2 = " ++ showP p22
    putStrLn $ "p2 +^ p3 = " ++ showP p23
    putStrLn $ "p1 +^ (p2 +^ p3) = " ++ showP p1b23
    putStrLn $ "size 1 $ p1 +^ (p2 +^ p3) = " ++ showP p1b23s1
    putStrLn $ "(p2 +^ p2) +^ p3 = " ++ showP pb223
    putStrLn $ "p1 +^ ((p2 +^ p2) +^ p3) = " ++ showP p1bb223
    putStrLn $ "size 1 $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ showP p1bb223s1
    putStrLn $ "degree 0 $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ showP p1bb223d0
    putStrLn $ "boundUpThin $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bdp1bb223d0
    where
    showP = showInternals (showChebTerms, showCoeffInternals)
    showChebTerms = True
    showCoeffInternals = False
    opsPtr = unsafePerformIO $ DCPoly.newOps DCPoly.Ops_Pure
    p1 = DCPoly.constPoly 3 0 (Var 2) (Size 10) (Power 3)
    p2 = DCPoly.projectionPoly (Var 0) (Var 2) (Size 10) (Power 3)
    p3 = DCPoly.projectionPoly (Var 1) (Var 2) (Size 10) (Power 3)
    p11 = DCPoly.polyAddUpPureUsingPureOps (Size 2) (Power 3) opsPtr p1 p1
    p12 = DCPoly.polyAddUpPureUsingPureOps (Size 2) (Power 3) opsPtr p1 p2
    p22 = DCPoly.polyAddUpPureUsingPureOps (Size 2) (Power 3) opsPtr p2 p2
    p23 = DCPoly.polyAddUpPureUsingPureOps (Size 2) (Power 3) opsPtr p2 p3
    p1b23 = DCPoly.polyAddUpPureUsingPureOps (Size 2) (Power 3) opsPtr p1 p23
    p1b23s1 = DCPoly.polyAddUpPureUsingPureOps (Size 1) (Power 3) opsPtr p1 p23
    pb223 = DCPoly.polyAddUpPureUsingPureOps (Size 2) (Power 3) opsPtr p22 p3
    p1bb223 = DCPoly.polyAddUpPureUsingPureOps (Size 2) (Power 3) opsPtr p1 pb223
    p1bb223s1 = DCPoly.polyAddUpPureUsingPureOps (Size 1) (Power 3) opsPtr p1 pb223
    p1bb223d0 = DCPoly.polyAddUpPureUsingPureOps (Size 2) (Power 0) opsPtr p1 pb223
    (maxArity, maxSize, maxDegree) = DCPoly.peekSizes p1
    constTerm = DCPoly.peekConst p1
    bdp1bb223d0 = DCPoly.polyBoundUpThin opsPtr p1bb223d0

testPureGCPolys :: IO ()
testPureGCPolys =
    do
    putStrLn $ "p1.maxArity = " ++ show maxArity
    putStrLn $ "p1.maxSize = " ++ show maxSize
    putStrLn $ "p1.maxDegree = " ++ show maxDegree
    putStrLn $ "p1.constTerm = " ++ show constTerm
    putStrLn $ "p1 = " ++ showP p1
    GCPoly.printPoly p1
    putStrLn $ "p2 = " ++ showP p2
    GCPoly.printPoly p2
    putStrLn $ "p3 = " ++ showP p3
    GCPoly.printPoly p3
    putStrLn $ "p1 +^ p1 = " ++ showP p11
    GCPoly.printPoly p11
    putStrLn $ "p1 +^ p2 = " ++ showP p12
    GCPoly.printPoly p22
    putStrLn $ "p2 +^ p2 = " ++ showP p22
    GCPoly.printPoly p22
    putStrLn $ "p2 +^ p3 = " ++ showP p23
    GCPoly.printPoly p23
    putStrLn $ "p1 +^ (p2 +^ p3) = " ++ showP p1b23
    GCPoly.printPoly p1b23
    putStrLn $ "size 1 $ p1 +^ (p2 +^ p3) = " ++ showP p1b23s1
    GCPoly.printPoly p1b23s1
    putStrLn $ "(p2 +^ p2) +^ p3 = " ++ showP pb223
    GCPoly.printPoly pb223
    putStrLn $ "p1 +^ ((p2 +^ p2) +^ p3) = " ++ showP p1bb223
    GCPoly.printPoly p1bb223
    putStrLn $ "size 1 $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ showP p1bb223s1
    GCPoly.printPoly p1bb223s1
    putStrLn $ "degree 0 $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ showP p1bb223d0
    GCPoly.printPoly p1bb223d0
    where
    showP = showInternals (showChebTerms, showCoeffInternals)
    showChebTerms = True
    showCoeffInternals = False
    opsPtr = GCPoly.newOpsPureArithUpDnDefaultEffort sampleD
    p1 = GCPoly.constPoly (3::Double) 0 (Var 2) (Size 10) (Power 3)
    p2 = GCPoly.projectionPoly sampleD (Var 0) (Var 2) (Size 10) (Power 3)
    p3 = GCPoly.projectionPoly sampleD (Var 1) (Var 2) (Size 10) (Power 3)
    p11 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p1 p1
    p12 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p1 p2
    p22 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p2 p2
    p23 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p2 p3
    p1b23 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p1 p23
    p1b23s1 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 1) (Power 3) opsPtr p1 p23
    pb223 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p22 p3
    p1bb223 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p1 pb223
    p1bb223s1 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 1) (Power 3) opsPtr p1 pb223
    p1bb223d0 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 0) opsPtr p1 pb223
    (maxArity, maxSize, maxDegree) = GCPoly.peekSizes p1
    constTerm = GCPoly.peekConst p1
        
testMutableGCPolys :: IO ()
testMutableGCPolys =
    do
    arity <- GCPoly.peekArityIO p1
    putStrLn $ "maxArity = " ++ show arity
    putStrLn $ "p1 = " ++ showP p1
    putStrLn $ "p2 = " ++ showP p2
    putStrLn $ "p3 = " ++ showP p3
    putStrLn $ "p11 = " ++ showP p11
    putStrLn $ "p12 = " ++ showP p12
    putStrLn $ "p22 = " ++ showP p22
    putStrLn $ "p1b23 = " ++ showP p1b23
    putStrLn $ "pb223 = " ++ showP pb223
    putStrLn $ "p23s1 = " ++ showP p23s1
    putStrLn $ "pb223s1 = " ++ showP pb223s1
    putStrLn $ "pb223d0 = " ++ showP pb223d0
    where
    showP = showInternals (showChebTerms, showCoeffInternals)
    showChebTerms = True
    showCoeffInternals = False
    opsPtr = GCPoly.newOpsPureArithUpDnDefaultEffort sampleD
    opsMutablePtr = GCPoly.newOpsMutableArithUpDnDefaultEffort sampleD
    [p1,p2,p3,p11,p12,p22,p1b23,pb223,p23s1,pb223s1, pb223d0] = runST $
        do
        p1M <- GCPoly.constPolyMutable (3::Double) 0 (Var 2) (Size 10) (Power 3)
        p2M <- GCPoly.projectionPolyMutable sampleD (Var 0) (Var 2) (Size 10) (Power 3)
        p3M <- GCPoly.projectionPolyMutable sampleD (Var 1) (Var 2) (Size 10) (Power 3)
        p11M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 2) (Power 3)
--        unsafeIOToST $ putStrLn $ "A"
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p11M p1M p1M
--        unsafeIOToST $ putStrLn $ "A"
        p12M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 2) (Power 3)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p12M p1M p2M
        p22M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 2) (Power 3)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p22M p2M p2M
        p1b23M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 2) (Power 3)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p1b23M p2M p3M
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p1b23M p1M p1b23M
        pb223M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 2) (Power 3)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr pb223M p22M p3M
        p23s1M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 1) (Power 3)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr p23s1M p2M p3M
        pb223s1M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 1) (Power 3)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr pb223s1M p22M p3M
        pb223d0M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 2) (Power 0)
        GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr pb223d0M p22M p3M
        mapM (GCPoly.unsafeReadPolyMutable sampleD) [p1M, p2M, p3M, p11M, p12M, p22M, p1b23M, pb223M, p23s1M, pb223s1M, pb223d0M]
    
