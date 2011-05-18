{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Evaluation
    Description :  evaluation of a PolyFP in a custom interpretation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Evaluation of a PolyFP in a custom interpretation.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Evaluation where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Domain()

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (PolyPure)
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)
import Foreign.Storable

data PolyPureEvalOps cf t =
    PolyPureEvalOps
    {
        polyPureEvalFromCoeff :: cf -> t,
        polyPureEvalTimes :: t -> t -> t,
        polyPureEvalPlus :: t -> t -> t,
        polyPureEvalMinus :: t -> t -> t
    }

instance
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    => 
    CanEvaluateOtherType (PolyPure cf) 
    where
    type EvalOps (PolyPure cf) = PolyPureEvalOps cf
    evalOtherType evalOps varValuesBox p =
        runST $
            do
            pM <- unsafeMakeMutable p
            return $
                evalAtPtChebBasis pM
                    (map snd $ toAscList varValuesBox)
                    (polyPureEvalFromCoeff evalOps one)
                    (polyPureEvalPlus evalOps)
                    (polyPureEvalMinus evalOps)
                    (polyPureEvalTimes evalOps)
                    (polyPureEvalFromCoeff evalOps)


