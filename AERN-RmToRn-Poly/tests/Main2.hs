
module Main where

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff as GCPoly
import Numeric.AERN.RealArithmetic.Basis.Double()

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.MinimalFnBasis

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.ShowInternals

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.OpsDefaultEffort

import Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)

type P = FnEndpoint (GCPoly.Poly Double)

main :: IO ()
main =
    do
    putStrLn $ "x = " ++ showP x
    putStrLn $ "y = " ++ showP y
    putStrLn $ "c0 = " ++ showP c0
    putStrLn $ "c2 = " ++ showP c2
    putStrLn $ "x +^ 2 = " ++ showP xc2
    putStrLn $ "(x +^ 2) *^ y^2 = " ++ showP xc2Ty2
    putStrLn $ "((x +^ 2) *^ y^2)*3 = " ++ showP xc2Ty2T3
    putStrLn $ "((x +^ 2) *^ y^2)*3 + 1 = " ++ showP xc2Ty2T3P1
    putStrLn $ "x *^ x = " ++ showP x2
    putStrLn $ "x +^ 2 >=? 1 = " ++ (show $ xc2 >=? c1)
    putStrLn $ "x +^ 2 >? 1 = " ++ (show $ xc2 >? c1)
    putStrLn $ "x +^ 2 >? 2 = " ++ (show $ xc2 >? c2)
    putStrLn $ "x +^ 2 >? 3 = " ++ (show $ xc2 >? c3)
    putStrLn $ "x^2 >=? 0 = " ++ (show $ x2 >=? c0)
    putStrLn $ "lower bound of x^2 = " ++ (show $ lowerBound x2)
    where
    showP = showInternals (showChebTerms, showCoeffInternals)
    showChebTerms = True
    showCoeffInternals = False
    domainBox = fromAscList $ [(0, unitInterval),(1, unitInterval),(2, unitInterval)]
    unitInterval = Interval (-1) 1
    lowerBound :: P -> Maybe Double
    lowerBound f = ArithUpDn.convertDnEff () f
    opsFP = GCPoly.opsFPArithUpDnDefaultEffort (0 :: Double)
    x :: P
    [x,y,c0,c1,c2,c3,xc2,xc2Ty2,xc2Ty2T3,xc2Ty2T3P1,x2] =
        runST $
            do
            xM <- unsafeMakeMutable $ newProjection (opsFP,10,3,3) domainBox 0
            yM <- unsafeMakeMutable $ newProjection (opsFP,10,3,3) domainBox 1
            c0M <- unsafeMakeMutable $ newConstFn (opsFP,10,3,3) domainBox 0
            c1M <- unsafeMakeMutable $ newConstFn (opsFP,10,3,3) domainBox 1
            c2M <- unsafeMakeMutable $ newConstFn (opsFP,10,3,3) domainBox 2
            c3M <- unsafeMakeMutable $ newConstFn (opsFP,10,3,3) domainBox 3
            xc2M <- cloneMutable xM
            xc2M +^= c2M
            xc2Ty2M <- cloneMutable xc2M
            xc2Ty2M *^= yM
            xc2Ty2M *^= yM
            xc2Ty2T3M <- cloneMutable xc2Ty2M
            xc2Ty2T3M *^|= (3::Double) 
            xc2Ty2T3P1M <- cloneMutable xc2Ty2T3M
            xc2Ty2T3P1M +^|= (1::Double)
            x2M <- cloneMutable xM
            x2M *^= xM 
            mapM unsafeReadMutable [xM,yM,c0M,c1M,c2M,c3M,xc2M, xc2Ty2M, xc2Ty2T3M, xc2Ty2T3P1M, x2M]
    
    