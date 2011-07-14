
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
import System.IO

main :: IO ()
main = 
    do
    hSetBuffering stdout LineBuffering
--    hSetBuffering stdout NoBuffering
--    testMutableDCPolys
    testMutableGCPolys
        
testMutableGCPolys :: IO ()
testMutableGCPolys =
    do
    putStrLn $ "t = " ++ showP t

    putStrLn $ "p1 = " ++ showP p1
    putStrLn $ "p2 = " ++ showP p2
    putStrLn $ "p3 = " ++ showP p3
    putStrLn $ "p4 = " ++ showP p4

    putStrLn $ "p11 = " ++ showP p11
    putStrLn $ "p12 = " ++ showP p12
    putStrLn $ "p22 = " ++ showP p22
    
    putStrLn $ "p1b23 = " ++ showP p1b23
    putStrLn $ "pb223 = " ++ showP pb223

    putStrLn $ "p23 (maxSize = 1) = " ++ showP p23s1
    putStrLn $ "pb223 (maxSize = 1) = " ++ showP pb223s1
    putStrLn $ "pb223 (maxDegree = 0) = " ++ showP pb223d0
    putStrLn $ "pb223 (maxTermArity = 0) = " ++ showP pb223d0

    putStrLn $ "scaleUpThin 0.1 x = " ++ showP sux
    putStrLn $ "scaleDnThin 0.1 x = " ++ showP sdx
    putStrLn $ "scaleEncl 0.1 x = " ++ showP sex
  
    putStrLn $ "copyEncl (" ++ showP cpetarg ++ ") (" ++ showP cpesrc ++ ") = " ++ showP cperes
    putStrLn $ "copyUpThin (" ++ showP cpuptarg ++ ") (" ++ showP cpupsrc ++ ") = " ++ showP cpupres
    putStrLn $ "copyDnThin (" ++ showP cpdntarg ++ ") (" ++ showP cpdnsrc ++ ") = " ++ showP cpdnres
    
    putStrLn $ "mupxpy = " ++ showP mupxpy
    putStrLn $ "mupxpxy = " ++ showP mupxpxy
    putStrLn $ "mupxpxy (maxDegree = 1) = " ++ showP mupxpxyd1
    putStrLn $ "mupxpxy (maxTermArity = 1) = " ++ showP mupxpxya1
    putStrLn $ "mupxpxy (maxSize = 1) = " ++ showP mupxpxys1

    putStrLn $ "mupx1pxy1 = " ++ showP mupx1pxy1
    putStrLn $ "mupx1pxy1 (maxDegree = 2) = " ++ showP mupx1pxy1d2
    putStrLn $ "mupx1pxy1 (maxSize = 2) = " ++ showP mupx1pxy1s2
    
    putStrLn $ "mepxpy = " ++ showP mepxpy
    putStrLn $ "mepxpxy = " ++ showP mepxpxy
    putStrLn $ "mepxpxy (maxDegree = 1) = " ++ showP mepxpxyd1
    putStrLn $ "mepxpxy (maxTermArity = 1) = " ++ showP mepxpxya1
    putStrLn $ "mepxpxy (maxSize = 1) = " ++ showP mepxpxys1

    putStrLn $ "mepx1pxy1 = " ++ showP mepx1pxy1
    putStrLn $ "mepx1pxy1 (maxDegree = 2) = " ++ showP mepx1pxy1d2
    putStrLn $ "mepx1pxy1 (maxSize = 2) = " ++ showP mepx1pxy1s2
    
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
      t
      ,
      p1,p2,p3,p4
      ,
      p11,p12,p22
      ,
      p1b23,pb223
      ,
      p23s1,pb223s1,pb223d0,pb223a0
      ,
      sux,sdx,sex
      ,
      cpesrc,cpetarg,cperes,cpupsrc,cpuptarg,cpupres,cpdnsrc,cpdntarg,cpdnres
      ,
      mupxpy, mupxpxy, mupxpxyd1, mupxpxya1, mupxpxys1, mupx1pxy1, mupx1pxy1d2, mupx1pxy1s2
      ,
      mepxpy, mepxpxy, mepxpxyd1, mepxpxya1, mepxpxys1, mepx1pxy1, mepx1pxy1d2, mepx1pxy1s2
     ] = runST $
        do
        let mkConst c = GCPoly.constPolyM opsFP (Var 3) (Size 10) (Power 3) (Var 3) (c::Double) 0
        let mkConstSzPwTa c sz pw ta = GCPoly.constPolyM opsFP (Var 3) (Size sz) (Power pw) (Var ta) (c::Double) 0
        let mkVar n = GCPoly.projectionPolyM opsFP (Var 3) (Size 10) (Power 3) (Var 3) (Var n)

        let addUp = GCPoly.polyAddUp
--        let addDn = GCPoly.polyAddDn
        let addEncl = GCPoly.polyAddEncl

        let multiplyUp = GCPoly.polyMultiplyUp
--        let multiplyDn = GCPoly.polyMultiplyDn
        let multiplyEncl = GCPoly.polyMultiplyEncl

        let scaleUpThin c = GCPoly.polyScaleUp (c::Double) 
        let scaleDnThin c = GCPoly.polyScaleDn (c::Double) 
        let scaleEncl c = GCPoly.polyScaleEncl (c::Double) 

        let copyEncl = GCPoly.polyCopyEncl
        let copyUpThin = GCPoly.polyCopyUpThin
        let copyDnThin = GCPoly.polyCopyDnThin
        
        tM <- GCPoly.testPolyM opsFP 1 2 3 (0 :: Double)
        
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

        p23s1M <- mkConstSzPwTa 0 1 3 3
        addUp p23s1M p2M p3M
        pb223s1M <- mkConstSzPwTa 0 1 3 3
        addUp pb223s1M p22M p3M
        pb223d0M <- mkConstSzPwTa 0 10 0 3
        addUp pb223d0M p22M p3M
        pb223a0M <- mkConstSzPwTa 0 10 3 0
        addUp pb223a0M p22M p3M
        
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
        cpetargM <- mkConstSzPwTa 1 1 3 3
        cperesM  <- mkConstSzPwTa 1 1 3 3
        copyEncl cperesM cpesrcM

        cpupsrcM  <- mkConst 1 
        addUp cpupsrcM cpupsrcM p2M
        addUp cpupsrcM cpupsrcM p3M
        addUp cpupsrcM cpupsrcM p4M
        cpuptargM <- mkConstSzPwTa 1 1 3 3
        cpupresM  <- mkConstSzPwTa 1 1 3 3
        copyUpThin cpupresM cpupsrcM

        cpdnsrcM  <- mkConst 1 
        addUp cpdnsrcM cpdnsrcM p2M
        addUp cpdnsrcM cpdnsrcM p3M
        addUp cpdnsrcM cpdnsrcM p4M
        cpdntargM <- mkConstSzPwTa 1 1 3 3
        cpdnresM  <- mkConstSzPwTa 1 1 3 3
        copyDnThin cpdnresM cpdnsrcM
        
        c1M <- mkConst 1
        xM <- mkVar 0 -- "x"
        yM <- mkVar 1 -- "y"
        temp1M <- mkConst 0
        temp2M <- mkConst 0

        mupxpyM <- mkConst 1
        multiplyUp mupxpyM xM yM
        mupxpxyM <- mkConst 1
        multiplyUp mupxpxyM mupxpyM xM
        mupxpxyd1M <- mkConstSzPwTa 1 10 1 3
        multiplyUp mupxpxyd1M mupxpyM xM
        mupxpxya1M <- mkConstSzPwTa 1 10 3 1
        multiplyUp mupxpxya1M mupxpyM xM
        mupxpxys1M <- mkConstSzPwTa 1 1 3 3
        multiplyUp mupxpxys1M mupxpyM xM

        mupx1pxy1M <- mkConst 1
        addUp temp1M xM c1M
        addUp temp2M mupxpyM c1M
        multiplyUp mupx1pxy1M temp1M temp2M

        mupx1pxy1d2M <- mkConstSzPwTa 1 10 2 3
        multiplyUp mupx1pxy1d2M temp1M temp2M
        mupx1pxy1s2M <- mkConstSzPwTa 1 2 3 3
        multiplyUp mupx1pxy1s2M temp1M temp2M
        
        mepxpyM <- mkConst 1
        multiplyEncl mepxpyM xM yM
        mepxpxyM <- mkConst 1
        multiplyEncl mepxpxyM mepxpyM xM
        mepxpxyd1M <- mkConstSzPwTa 1 10 1 3
        multiplyEncl mepxpxyd1M mepxpyM xM
        mepxpxya1M <- mkConstSzPwTa 1 10 3 1
        multiplyEncl mepxpxya1M mepxpyM xM
        mepxpxys1M <- mkConstSzPwTa 1 1 3 3
        multiplyEncl mepxpxys1M mepxpyM xM

        mepx1pxy1M <- mkConst 1
        addEncl temp1M xM c1M
        addEncl temp2M mepxpyM c1M
        multiplyEncl mepx1pxy1M temp1M temp2M

        mepx1pxy1d2M <- mkConstSzPwTa 1 10 2 3
        multiplyEncl mepx1pxy1d2M temp1M temp2M
        mepx1pxy1s2M <- mkConstSzPwTa 1 2 3 3
        multiplyEncl mepx1pxy1s2M temp1M temp2M
        
        mapM (unsafeReadMutable) 
          [
            tM
            ,
            p1M, p2M, p3M, p4M
            , 
            p11M, p12M, p22M
            , 
            p1b23M, pb223M
            , 
            p23s1M, pb223s1M, pb223d0M, pb223a0M
            , 
            suxM, sdxM, sexM
            , 
            cpesrcM, cpetargM, cperesM, cpupsrcM, cpuptargM, cpupresM, cpdnsrcM, cpdntargM, cpdnresM
            ,
            mupxpyM, mupxpxyM, mupxpxyd1M, mupxpxya1M, mupxpxys1M, mupx1pxy1M, mupx1pxy1d2M, mupx1pxy1s2M
            ,
            mepxpyM, mepxpxyM, mepxpxyd1M, mepxpxya1M, mepxpxys1M, mepx1pxy1M, mepx1pxy1d2M, mepx1pxy1s2M
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
 
