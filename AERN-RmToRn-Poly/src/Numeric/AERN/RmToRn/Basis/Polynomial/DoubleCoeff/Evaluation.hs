{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Evaluation
    Description :  evaluation of a PolyFP in a custom interpretation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Evaluation of a PolyFP in a custom interpretation.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Evaluation where

import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Domain()

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly (PolyFP)

--import Numeric.AERN.RealArithmetic.NumericOrderRounding

--import Numeric.AERN.Basics.Exception
--import Control.Exception

--import Foreign.Ptr(Ptr)
--import System.IO.Unsafe (unsafePerformIO)

data PolyFPEvalOps t =
    PolyFPEvalOps
    {
        polyFPevalFromCoeff :: Double -> t,
        polyFPevalTimes :: t -> t -> t,
        polyFPevalPlus :: t -> t -> t,
        polyFPevalMinus :: t -> t -> t
    }

instance CanEvaluateOtherType PolyFP where
    type EvalOps PolyFP = PolyFPEvalOps
    evalOtherType evalOps varValuesBox p =
        Poly.evalAtPtChebBasis p
            (map snd $ toAscList varValuesBox)
            (polyFPevalFromCoeff evalOps 1)
            (polyFPevalPlus evalOps)
            (polyFPevalMinus evalOps)
            (polyFPevalTimes evalOps)
            (polyFPevalFromCoeff evalOps)


