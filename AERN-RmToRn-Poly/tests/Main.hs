
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
--    testPureDCPolys
--    testMutableDCPolys
--    testPureGCPolys
    testMutableGCPolys

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
    putStrLn $ "boundUpThin $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bdupthp1bb223d0
    putStrLn $ "boundDnThin $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bddnthp1bb223d0
    putStrLn $ "boundUp $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bdupp1bb223d0
    putStrLn $ "boundDn $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bddnp1bb223d0
    where
    showP = showInternals (showChebTerms, showCoeffInternals)
    showChebTerms = True
    showCoeffInternals = False
    opsPtr = unsafePerformIO $ DCPoly.newOps DCPoly.Ops_Pure
    p1 = DCPoly.constPoly 3 0 (Var 2) (Size 10) (Power 3)
    p2 = DCPoly.projectionPoly (Var 0) (Var 2) (Size 10) (Power 3)
    p3 = DCPoly.projectionPoly (Var 1) (Var 2) (Size 10) (Power 3)
    p11 = DCPoly.polyAddUpPure (Size 2) (Power 3) opsPtr p1 p1
    p12 = DCPoly.polyAddUpPure (Size 2) (Power 3) opsPtr p1 p2
    p22 = DCPoly.polyAddUpPure (Size 2) (Power 3) opsPtr p2 p2
    p23 = DCPoly.polyAddUpPure (Size 2) (Power 3) opsPtr p2 p3
    p1b23 = DCPoly.polyAddUpPure (Size 2) (Power 3) opsPtr p1 p23
    p1b23s1 = DCPoly.polyAddUpPure (Size 1) (Power 3) opsPtr p1 p23
    pb223 = DCPoly.polyAddUpPure (Size 2) (Power 3) opsPtr p22 p3
    p1bb223 = DCPoly.polyAddUpPure (Size 2) (Power 3) opsPtr p1 pb223
    p1bb223s1 = DCPoly.polyAddUpPure (Size 1) (Power 3) opsPtr p1 pb223
    p1bb223d0 = DCPoly.polyAddUpPure (Size 2) (Power 0) opsPtr p1 pb223
    (maxArity, maxSize, maxDegree) = DCPoly.peekSizes p1
    constTerm = DCPoly.peekConst p1
    bdupthp1bb223d0 = DCPoly.polyBoundUpThin opsPtr p1bb223
    bddnthp1bb223d0 = DCPoly.polyBoundDnThin opsPtr p1bb223
    bdupp1bb223d0 = DCPoly.polyBoundUp opsPtr p1bb223
    bddnp1bb223d0 = DCPoly.polyBoundDn opsPtr p1bb223

testMutableDCPolys :: IO ()
testMutableDCPolys =
    do
    arity <- DCPoly.peekArityIO p1
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
    putStrLn $ "pb223d0 = " ++ showP pb223d0
    putStrLn $ "scaleUpThin 0.1 x = " ++ show sux
    putStrLn $ "scaleDnThin 0.1 x = " ++ show sdx
    putStrLn $ "scaleEncl 0.1 x = " ++ show sex
    where
    showP = showInternals (showChebTerms, showCoeffInternals)
    showChebTerms = True
    showCoeffInternals = False
    opsMutablePtr = unsafePerformIO $ DCPoly.newOps DCPoly.Ops_Pure
    [p1,p2,p3,p11,p12,p22,p1b23,pb223,p23s1,pb223s1,pb223d0,sux,sdx,sex] = runST $
        do
        let mkConst c = DCPoly.constPolyMutable (c::Double) 0 (Var 2) (Size 10) (Power 3)
        let mkVar n = DCPoly.projectionPolyMutable (Var n) (Var 2) (Size 10) (Power 3)
        let addUp = DCPoly.polyAddUpMutable opsMutablePtr
        let scaleUpThin = DCPoly.polyScaleUpInPlace opsMutablePtr
        let scaleDnThin = DCPoly.polyScaleDnInPlace opsMutablePtr
        let scaleEncl = DCPoly.polyScaleEnclInPlace opsMutablePtr
        
        p1M <- mkConst 0
        p2M <- mkVar 0 -- "x"
        p3M <- mkVar 1 -- "y"
        p11M <- mkConst 0 -- allocate space
        addUp p11M p1M p1M -- p11M := p1M +^ p1M
        p12M <- mkConst 0
        addUp p12M p1M p2M
        p22M <- mkConst 0
        addUp p22M p2M p2M
        p1b23M <- mkConst 0
        addUp p1b23M p2M p3M
        addUp p1b23M p1M p1b23M
        pb223M <- mkConst 0
        addUp pb223M p22M p3M
        p23s1M <- DCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 1) (Power 3)
        addUp p23s1M p2M p3M
        pb223s1M <- DCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 1) (Power 3)
        addUp pb223s1M p22M p3M
        pb223d0M <- DCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 2) (Power 0)
        addUp pb223d0M p22M p3M
        suxM <- mkVar 0
        scaleUpThin 0.1 suxM
        sdxM <- mkVar 0
        scaleDnThin 0.1 sdxM
        sexM <- mkVar 0
        scaleEncl 0.1 sexM
        return [p1M, p2M, p3M, p11M, p12M, p22M, p1b23M, pb223M, p23s1M, pb223s1M, pb223d0M, suxM, sdxM, sexM]
    
testPureGCPolys :: IO ()
testPureGCPolys =
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
    putStrLn $ "boundUpThin $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bdupthp1bb223d0
    putStrLn $ "boundDnThin $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bddnthp1bb223d0
    putStrLn $ "boundUp $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bdupp1bb223d0
    putStrLn $ "boundDn $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bddnp1bb223d0
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
    bdupthp1bb223d0 = GCPoly.polyBoundUpThin opsPtr p1bb223
    bddnthp1bb223d0 = GCPoly.polyBoundDnThin opsPtr p1bb223
    bdupp1bb223d0 = GCPoly.polyBoundUp opsPtr p1bb223
    bddnp1bb223d0 = GCPoly.polyBoundDn opsPtr p1bb223
        
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
    putStrLn $ "scaleUpThin 0.1 x = " ++ showP sux
    putStrLn $ "scaleDnThin 0.1 x = " ++ showP sdx
    putStrLn $ "scaleEncl 0.1 x = " ++ showP sex
    putStrLn $ "reduceDegreeEncl 0 pb223 = " ++ showP rd0pb223
    putStrLn $ "copyEncl x y = " ++ showP cpres
    where
    showP = showInternals (showChebTerms, showCoeffInternals)
    showChebTerms = True
    showCoeffInternals = False
    opsPtr = GCPoly.newOpsPureArithUpDnDefaultEffort sampleD
    opsMutablePtr = GCPoly.newOpsMutableArithUpDnDefaultEffort sampleD
    [p1,p2,p3,p11,p12,p22,p1b23,pb223,p23s1,pb223s1,pb223d0,sux,sdx,sex,rd0pb223,cpres] = runST $
        do
        let mkConst c = GCPoly.constPolyMutable (c::Double) 0 (Var 2) (Size 10) (Power 3)
        let mkVar n = GCPoly.projectionPolyMutable sampleD (Var n) (Var 2) (Size 10) (Power 3)
        let addUp = GCPoly.polyAddUpMutableUsingMutableOps sampleD opsMutablePtr
        let scaleUpThin c = GCPoly.polyScaleUpMutableUsingMutableOps 0 opsMutablePtr (c::Double) 
        let scaleDnThin c = GCPoly.polyScaleDnMutableUsingMutableOps 0 opsMutablePtr (c::Double) 
        let scaleEncl c = GCPoly.polyScaleEnclMutableUsingMutableOps opsMutablePtr (c::Double) 
        let reduceDegree d = GCPoly.polyReduceDegreeEnclMutableUsingMutableOps opsMutablePtr (Power d) 
        let copyEncl = GCPoly.polyCopyEnclMutableUsingMutableOpsGenCf opsMutablePtr
        
        p1M <- mkConst 0
        p2M <- mkVar 0 -- "x"
        p3M <- mkVar 1 -- "y"
        p11M <- mkConst 0 -- allocate space
        addUp p11M p1M p1M -- p11M := p1M +^ p1M
        p12M <- mkConst 0
        addUp p12M p1M p2M
        p22M <- mkConst 0
        addUp p22M p2M p2M
        p1b23M <- mkConst 0
        addUp p1b23M p2M p3M
        addUp p1b23M p1M p1b23M
        pb223M <- mkConst 0
        addUp pb223M p22M p3M
        p23s1M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 1) (Power 3)
        addUp p23s1M p2M p3M
        pb223s1M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 1) (Power 3)
        addUp pb223s1M p22M p3M
        pb223d0M <- GCPoly.constPolyMutable (0::Double) 0 (Var 2) (Size 2) (Power 0)
        addUp pb223d0M p22M p3M
        suxM <- mkVar 0
        scaleUpThin 0.1 suxM
        sdxM <- mkVar 0
        scaleDnThin 0.1 sdxM
        sexM <- mkVar 0
        scaleEncl 0.1 sexM 
        rd0pb223M <- mkVar 0
        scaleEncl 2.0 rd0pb223M
        addUp rd0pb223M p3M rd0pb223M
        reduceDegree 0 rd0pb223M
        cpresM <- mkVar 0 --"x"
        copyEncl cpresM p3M -- copy "y" over "x"
        mapM (GCPoly.unsafeReadPolyMutable sampleD) 
          [p1M, p2M, p3M, p11M, p12M, p22M, p1b23M, pb223M, p23s1M, pb223s1M, pb223d0M, 
           suxM, sdxM, sexM, rd0pb223M, cpresM]
 