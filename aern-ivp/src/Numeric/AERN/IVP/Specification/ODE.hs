{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.IVP.Specification.ODE
    Description :  specification of ODE IVP with uncertainty
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Specification of ODE IVP with uncertainty.
-}

module Numeric.AERN.IVP.Specification.ODE
--(
--)
where

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New
--import Numeric.AERN.RmToRn.Evaluation
--import Numeric.AERN.RmToRn.Integration
--
--import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
--import Numeric.AERN.RealArithmetic.ExactOps
--
--import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsDefaultEffort
--
--import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

--import Numeric.AERN.Misc.Debug

data ODEIVP f =
    ODEIVP
    {
        odeivp_field :: [f] -> [f]
    ,
        odeivp_componentNames :: [Var f]
    , 
        odeivp_tVar :: Var f -- ^ @tVar@
    ,
        odeivp_tStart :: Domain f -- ^ @tStart@
    ,
        odeivp_tEnd :: Domain f -- ^ @tEnd@
    ,
        odeivp_makeInitialValueFnVec :: Var f -> Domain f -> [f] 
        {-^ the parameters are an initial time variable @t0@ a time domain @T0@, 
            the result are functions whose first variable is @t0 : T0@ 
            - the initial value may depend on initial time
        -} 
    ,
        odeivp_t0End :: Domain f 
        {-^ 
            @t0End@ -- initial time uncertainty; 
            must satisfy @tStart <= t0End <= tEnd@;
            with exact initial time, we have @t0End = tStart@
        -}
    }

makeFnVecFromInitialValues ::
     (HasSizeLimits f,
      HasProjections f) 
      =>
     f {-^ a sample function, only its size limits matter -} ->
     [Var f] {-^ names for solution vector components -} -> 
     [Domain f] {-^ initial values, one for each component -}
     ->
     Var f {-^ @tVar@ - time variable -} -> 
     Domain f {-^ time domain  -} 
     -> 
     [f]
makeFnVecFromInitialValues sampleF componentNames initialValues tVar timeDomain =
    initialValuesFnVec
    where    
    initialValuesFnVec =
        map initialValueFn componentNames
    initialValueFn var =
        newProjection sizeLimits dombox var
    dombox =
        fromList $ (tVar, timeDomain) : (zip componentNames initialValues)
    sizeLimits = getSizeLimits sampleF
        
    