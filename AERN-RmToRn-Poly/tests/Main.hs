
module Main where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.Basis.Double

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Mutable

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff as GCPoly
import qualified Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff as DCPoly
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly (Var(..), Size(..), Power(..))

import Control.Monad.ST (runST,unsafeIOToST)

import System.IO.Unsafe

import Foreign.StablePtr
import System.Mem (performGC)
import Control.Concurrent (threadDelay, yield)

main :: IO ()
main = 
    do
--    testMutableDCPolys
    testMutableGCPolys
        
testMutableGCPolys :: IO ()
testMutableGCPolys =
    do
    putStrLn $ "p1 = " ++ showP p1
    putStrLn $ "p2 = " ++ showP p2
    putStrLn $ "p3 = " ++ showP p3
    putStrLn $ "p4 = " ++ showP p4

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
  
    putStrLn $ "copyEncl (" ++ showP cpetarg ++ ") (" ++ showP cpesrc ++ ") = " ++ showP cperes
    putStrLn $ "copyUpThin (" ++ showP cpuptarg ++ ") (" ++ showP cpupsrc ++ ") = " ++ showP cpupres
    putStrLn $ "copyDnThin (" ++ showP cpdntarg ++ ") (" ++ showP cpdnsrc ++ ") = " ++ showP cpdnres
--    performGC
--    threadDelay 1000000
    where
    showP :: GCPoly.Poly Double -> String
    showP = showInternals (showChebTerms, showCoeffInternals)
--    showChebTerms = True
    showChebTerms = False
    showCoeffInternals = False
    opsFP = GCPoly.opsFPArithUpDnDefaultEffort sampleD
    [
      p1,p2,p3,p4
      ,
      p11,p12,p22
      ,
      p1b23,pb223
      ,
      p23s1,pb223s1,pb223d0
      ,
      sux,sdx,sex
      ,
      cpesrc,cpetarg,cperes,cpupsrc,cpuptarg,cpupres,cpdnsrc,cpdntarg,cpdnres
     ] = runST $
        do
        let mkConst c = GCPoly.constPolyM opsFP (Var 3) (Size 10) (Power 3) (Var 3) (c::Double) 0
        let mkConstConst c = GCPoly.constPolyM opsFP (Var 3) (Size 1) (Power 3) (Var 3) (c::Double) 0
        let mkVar n = GCPoly.projectionPolyM opsFP (Var 3) (Size 10) (Power 3) (Var 3) (Var n)

        let addUp = GCPoly.polyAddUp

        let scaleUpThin c = GCPoly.polyScaleUp (c::Double) 
        let scaleDnThin c = GCPoly.polyScaleDn (c::Double) 
        let scaleEncl c = GCPoly.polyScaleEncl (c::Double) 

        let copyEncl = GCPoly.polyCopyEncl
        let copyUpThin = GCPoly.polyCopyUpThin
        let copyDnThin = GCPoly.polyCopyDnThin
        
        p1M <- mkConst 0
        p2M <- mkVar 0 -- "x"
        p3M <- mkVar 1 -- "y"
        p4M <- mkVar 2 -- "z"
        
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

        p23s1M <- GCPoly.constPolyM opsFP (Var 3) (Size 1) (Power 3) (Var 3) (0::Double) 0
        addUp p23s1M p2M p3M
        pb223s1M <- GCPoly.constPolyM opsFP (Var 3) (Size 1) (Power 3) (Var 3) (0::Double) 0
        addUp pb223s1M p22M p3M
        pb223d0M <- GCPoly.constPolyM opsFP (Var 3) (Size 2) (Power 0) (Var 3) (0::Double) 0
        addUp pb223d0M p22M p3M
        
        suxM <- mkVar 0
        scaleUpThin 0.1 suxM
        sdxM <- mkVar 0
        scaleDnThin 0.1 sdxM
        sexM <- mkVar 0
        scaleEncl 0.1 sexM
        
        cpesrcM  <- mkConst 1 
        addUp cpesrcM cpesrcM p2M
        addUp cpesrcM cpesrcM p3M
        addUp cpesrcM cpesrcM p4M
        cpetargM <- mkConstConst 1
        cperesM  <- mkConstConst 1
        copyEncl cperesM cpesrcM

        cpupsrcM  <- mkConst 1 
        addUp cpupsrcM cpupsrcM p2M
        addUp cpupsrcM cpupsrcM p3M
        addUp cpupsrcM cpupsrcM p4M
        cpuptargM <- mkConstConst 1
        cpupresM  <- mkConstConst 1
        copyUpThin cpupresM cpupsrcM

        cpdnsrcM  <- mkConst 1 
        addUp cpdnsrcM cpdnsrcM p2M
        addUp cpdnsrcM cpdnsrcM p3M
        addUp cpdnsrcM cpdnsrcM p4M
        cpdntargM <- mkConstConst 1
        cpdnresM  <- mkConstConst 1
        copyDnThin cpdnresM cpdnsrcM
        
        mapM (unsafeReadMutable) 
          [
            p1M, p2M, p3M, p4M
            , 
            p11M, p12M, p22M
            , 
            p1b23M, pb223M
            , 
            p23s1M, pb223s1M, pb223d0M
            , 
            suxM, sdxM, sexM
            , 
            cpesrcM, cpetargM, cperesM, cpupsrcM, cpuptargM, cpupresM, cpdnsrcM, cpdntargM, cpdnresM
           ]
 
--testPureGCPolys :: IO ()
--testPureGCPolys =
--    do
--    putStrLn $ "p1.maxArity = " ++ show maxArity
--    putStrLn $ "p1.maxSize = " ++ show maxSize
--    putStrLn $ "p1.maxDegree = " ++ show maxDegree
--    putStrLn $ "p1.constTerm = " ++ show constTerm
--    putStrLn $ "p1 = " ++ showP p1
--    putStrLn $ "p2 = " ++ showP p2
--    putStrLn $ "p3 = " ++ showP p3
--    putStrLn $ "p1 +^ p1 = " ++ showP p11
--    putStrLn $ "p1 +^ p2 = " ++ showP p12
--    putStrLn $ "p2 +^ p2 = " ++ showP p22
--    putStrLn $ "p2 +^ p3 = " ++ showP p23
--    putStrLn $ "p1 +^ (p2 +^ p3) = " ++ showP p1b23
--    putStrLn $ "size 1 $ p1 +^ (p2 +^ p3) = " ++ showP p1b23s1
--    putStrLn $ "(p2 +^ p2) +^ p3 = " ++ showP pb223
--    putStrLn $ "p1 +^ ((p2 +^ p2) +^ p3) = " ++ showP p1bb223
--    putStrLn $ "size 1 $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ showP p1bb223s1
--    putStrLn $ "degree 0 $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ showP p1bb223d0
--    putStrLn $ "boundUpThin $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bdupthp1bb223d0
--    putStrLn $ "boundDnThin $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bddnthp1bb223d0
--    putStrLn $ "boundUp $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bdupp1bb223d0
--    putStrLn $ "boundDn $ p1 +^ ((p2 +^ p2) +^ p3) = " ++ show bddnp1bb223d0
--    where
--    showP = showInternals (showChebTerms, showCoeffInternals)
--    showChebTerms = True
--    showCoeffInternals = False
--    opsPtr = GCPoly.newOpsPureArithUpDnDefaultEffort sampleD
--    p1 = GCPoly.constPoly (3::Double) 0 (Var 2) (Size 10) (Power 3)
--    p2 = GCPoly.projectionPoly sampleD (Var 0) (Var 2) (Size 10) (Power 3)
--    p3 = GCPoly.projectionPoly sampleD (Var 1) (Var 2) (Size 10) (Power 3)
--    p11 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p1 p1
--    p12 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p1 p2
--    p22 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p2 p2
--    p23 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p2 p3
--    p1b23 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p1 p23
--    p1b23s1 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 1) (Power 3) opsPtr p1 p23
--    pb223 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p22 p3
--    p1bb223 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 3) opsPtr p1 pb223
--    p1bb223s1 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 1) (Power 3) opsPtr p1 pb223
--    p1bb223d0 = GCPoly.polyAddUpPureUsingPureOps sampleD (Size 2) (Power 0) opsPtr p1 pb223
--    (maxArity, maxSize, maxDegree) = GCPoly.peekSizes p1
--    constTerm = GCPoly.peekConst p1
--    bdupthp1bb223d0 = GCPoly.polyBoundUpThin opsPtr p1bb223
--    bddnthp1bb223d0 = GCPoly.polyBoundDnThin opsPtr p1bb223
--    bdupp1bb223d0 = GCPoly.polyBoundUp opsPtr p1bb223
--    bddnp1bb223d0 = GCPoly.polyBoundDn opsPtr p1bb223
 
