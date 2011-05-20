
module Main where

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff as GCPoly
import Numeric.AERN.RealArithmetic.Basis.Double()

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.ShowInternals

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.OpsDefaultEffort

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)

type P = GCPoly.Poly Double

main :: IO ()
main =
    do
    putStrLn $ "x = " ++ showP x
    putStrLn $ "y = " ++ showP y
    putStrLn $ "c1 = " ++ showP c1
    putStrLn $ "x +^ c1 = " ++ showP xc1
    where
    showP = showInternals (showChebTerms, showCoeffInternals)
    showChebTerms = True
    showCoeffInternals = False
    domainBox = fromAscList $ [(0, unitInterval),(1, unitInterval),(2, unitInterval)]
    unitInterval = Interval (-1) 1
    opsFP = GCPoly.opsFPArithUpDnDefaultEffort (0 :: Double)
    x :: P
    [x,y,c1,xc1] =
        runST $
            do
            xM <- unsafeMakeMutable $ newProjection (opsFP,10,3) domainBox 0
            yM <- unsafeMakeMutable $ newProjection (opsFP,10,3) domainBox 1
            c1M <- unsafeMakeMutable $ newConstFn (opsFP,10,3) domainBox 1
            xc1M <- cloneMutable xM
            xc1M +^= c1M
            mapM unsafeReadMutable [xM,yM,c1M,xc1M]
    
    