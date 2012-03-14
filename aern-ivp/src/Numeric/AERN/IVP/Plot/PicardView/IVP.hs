{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Plot.PicardView.IVP
    Description :  definition of IVP to simulate
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Internal module for PicardView.
    Definition of IVP to simulate.
-}
module Numeric.AERN.RmToRn.Plot.PicardView.IVP
--(
--)
where

import Numeric.AERN.RmToRn.Domain

--import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

--import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Misc.Debug

data IVP f =
    IVP
    {
        ivpInitialValues :: [Domain f],  
        ivpVectorField :: [f] -> [f]
            {- ^ both input and output have the same number of components 
                 as the initial values list -}
    }
    
