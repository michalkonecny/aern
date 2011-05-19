
module Main where

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff as GCPoly
import Numeric.AERN.RealArithmetic.Basis.Double()

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.ShowInternals

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

type P = GCPoly.Poly Double

main :: IO ()
main =
    do
    putStrLn $ "x = " ++ showP x
    putStrLn $ "y = " ++ showP y
    putStrLn $ "c1 = " ++ showP c1
    where
    showP = showInternals (showChebTerms, showCoeffInternals)
    showChebTerms = True
    showCoeffInternals = False
    domainBox = fromAscList $ [(0, unitInterval),(1, unitInterval),(2, unitInterval)]
    unitInterval = Interval (-1) 1
    x = newProjection Nothing (opsFP,10,3) domainBox 0 :: P
    y = newProjection Nothing (opsFP,10,3) domainBox 1 :: P
    c1 = newConstFn Nothing (opsFP,10,3) domainBox 1 :: P
    opsFP = GCPoly.opsFPArithUpDnDefaultEffort (0 :: Double)
    
    