{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Show
    Description :  string representation of Poly
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    String representation of Poly with generic coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Show 
(
    showPolyFPWithVars
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly

import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.Basics.Interval
import Numeric.AERN.RealArithmetic.Interval()

import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsDefaultEffort

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

--import Numeric.AERN.Misc.Debug


import qualified Data.Map as Map
import Data.List

import Foreign.Storable

import System.IO.Unsafe

instance 
        (ShowInternals cf, Storable cf,
         ArithUpDn.RoundedReal cf) => 
        (Show (PolyFP cf)) 
    where
    show polyFP =
        showPolyFPWithVars polyFP $ varNames (fromIntegral maxArity)
        where
        (Var maxArity) = peekArity polyFP
        varNames arity = 
            take arity $ 
                ["x", "y", "z"] 
                ++ (map (\n -> "v" ++ show n ) [3..(arity - 1)])
        
showPolyFPWithVars :: 
    (ShowInternals cf, 
     Storable cf,
     ArithUpDn.RoundedReal cf) => 
    PolyFP cf -> [HVar] -> String
showPolyFPWithVars polyFP varNames =
    show $
            evalAtPtChebBasis 
                polyFP 
                (map hpolyVar varNames) 
                hpolyOne
                (hpolyAdd (<+>))
                (hpolySubtr neg (<+>))
                (hpolyMult (<+>) (<*>))
                (\coeff -> hpolyConst $ Interval coeff coeff)

newtype HPoly cf = HPoly (Map.Map HTerm cf) -- deriving (Show)
newtype HTerm = HTerm (Map.Map HVar Int) deriving (Eq, Ord) -- Show)
type HVar = String

instance (Show cf, HasZero cf, HasOne cf, NumOrd.PartialComparison cf) => Show (HPoly cf) where
    show (HPoly terms) 
        | null termsNonZero = "0"
        | otherwise =
            intercalate " + " $
            map showTerm termsNonZero
        where
        termsNonZero =
            filter nonZero $ Map.toList terms
        nonZero (_, coeff) =
                case (coeff ==? zero) of
                    (Just b) -> not b
                    _ -> True 
        showTerm (t@(HTerm term), coeff) = 
                case (coeff ==? one, Map.null term) of
                    (Just True, True) -> "1"
                    (Just True, _) -> show t  
                    (_, True) -> show coeff
                    _ -> show coeff ++ "*" ++ show t

instance Show HTerm where
    show (HTerm vars) =
        intercalate "*" $ map showVar $ Map.toList vars
        where
        showVar (var, power) 
            | power == 1 = 
                var
            | otherwise = 
                var ++ "^" ++ show power

--hpolyZero :: HPoly cf
--hpolyZero = HPoly Map.empty

hpolyConst :: 
    (Show cf, HasZero cf, HasOne cf, NumOrd.PartialComparison cf) =>
     cf -> HPoly cf
hpolyConst value =
--    unsafePrintReturn
--    (
--        "hpolyConst: value = " ++ show value ++ "; poly = " 
--    ) $ 
    HPoly (Map.singleton (HTerm Map.empty) value)

hpolyOne :: 
    (Show cf, HasZero cf, HasOne cf, NumOrd.PartialComparison cf) => 
    (HPoly cf)
hpolyOne = hpolyConst one

hpolyVar :: (HasOne t) => HVar -> HPoly t
hpolyVar varName = HPoly (Map.singleton (HTerm $ Map.singleton varName 1) one)

hpolyAdd ::
    (Show cf, HasZero cf, HasOne cf, NumOrd.PartialComparison cf) =>
    (cf -> cf -> cf) ->
    (HPoly cf -> HPoly cf -> HPoly cf)
hpolyAdd coeffAdd p1@(HPoly terms1) p2@(HPoly terms2) =
--    unsafePrintReturn
--    (
--        "hpolyAdd: " ++ show p1 ++ " + " ++ show p2 ++ " = "
--    ) $
    HPoly terms
    where
    terms = Map.unionWith coeffAdd terms1 terms2

hpolySubtr ::
    (Show cf, HasZero cf, HasOne cf, NumOrd.PartialComparison cf) =>
    (cf -> cf) ->
    (cf -> cf -> cf) ->
    (HPoly cf -> HPoly cf -> HPoly cf)
hpolySubtr coeffNeg coeffAdd p1@(HPoly terms1) p2@(HPoly terms2) =
--    unsafePrintReturn
--    (
--        "hpolySubtr: " ++ show p1 ++ " - " ++ show p2 ++ " = "
--    ) $
    HPoly terms
    where
    terms = Map.unionWith coeffAdd terms1 (Map.map coeffNeg terms2)

hpolyMult ::
    (Show cf, HasZero cf, HasOne cf, NumOrd.PartialComparison cf) =>
    (cf -> cf -> cf) ->
    (cf -> cf -> cf) ->
    (HPoly cf -> HPoly cf -> HPoly cf)
hpolyMult coeffAdd coeffMult p1@(HPoly terms1) p2@(HPoly terms2) =
--    unsafePrintReturn
--    (
--        "hpolyMult: " ++ show p1 ++ " * " ++ show p2 ++ " = "
--    ) $
    foldl (hpolyAdd coeffAdd) (HPoly Map.empty) $
        [HPoly $ 
            Map.singleton 
                (HTerm $ Map.unionWith (+) term1 term2) 
                (coeffMult cf1 cf2) 
        | 
            (HTerm term1, cf1) <- Map.toList terms1
        , 
            (HTerm term2, cf2) <- Map.toList terms2
        ]



--mkOpsPoly =
--    do
--    zeroSP <- newStablePtr (hpolyZero :: HPoly DI)
--    oneSP <- newStablePtr hpolyOne
--    absUpSP <- newStablePtr $ undefined 
--    absDnSP <- newStablePtr $ undefined 
--    addUpSP <- newStablePtr $ hpolyAdd (<+>)  
--    addDnSP <- newStablePtr $ hpolyAdd (<+>)  
--    multUpSP <- newStablePtr $ hpolyMult (<+>) (<*>)  
--    multDnSP <- newStablePtr $ hpolyMult (<+>) (<*>)  
--    return $
--      Ops
--        zeroSP oneSP 
--        absUpSP absDnSP
--        addUpSP addDnSP
--        multUpSP multDnSP
       

--xT = HTerm $ Map.fromList [("x",1)] 
--xyT = HTerm $ Map.fromList [("x",1),("y",1)] 
--x2yT = HTerm $ Map.fromList [("x",2),("y",1)] 
--t = HPoly $ Map.fromList [(xT,2),(xyT,1),(x2yT,3)]


