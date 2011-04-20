{-|
    Module      :  Numeric.AERN.DoubleBasis.MInterval
    Description :  Interval Double type and operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Mutable intervals with Double endpoints.
-}
module Numeric.AERN.DoubleBasis.MInterval 
(
    -- |
    -- A convenience module re-exporting various in-place interval 
    -- operations with default effort indicators.

    -- * Main type
    MDI,
    
    -- * In place field operations
    
    -- ** Explicit out parameter versions    
    addInInPlace,addOutInPlace,
    subtrInInPlace,subtrOutInPlace,
    absInInPlace,absOutInPlace,
    multInInPlace,multOutInPlace,
    powerToNonnegIntInInPlace,powerToNonnegIntOutInPlace,
    divInInPlace,divOutInPlace,

    -- ** Assignment operator versions

    -- * Base class and associted type
    CanBeMutable(..)
)
where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Interval
import Numeric.AERN.RealArithmetic.Basis.Double
import Numeric.AERN.RealArithmetic.Interval.Mutable
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as NumOrd
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsDefaultEffort
import Numeric.AERN.DoubleBasis.Interval

import Control.Monad.ST (runST)

-- | 
-- Mutable intervals with Double endpoints. Created and handled using
-- the instance methods of 'CanBeMutable' as in e.g.
-- 
-- > identity :: DI -> DI 
-- > identity x =
-- >   runST $
-- >     do
-- >     xM <- makeMutable x
-- >     result <- readMutable xM
-- >     return result
type MDI = Mutable DI

transl :: DI -> DI -> DI
transl x y =
  runST $
    do
    xM <- makeMutable x
    yM <- makeMutable y
    addOutInPlace xM xM yM
--    addOutInPlaceEff x (addDefaultEffort x) xM xM yM 
    result <- readMutable xM
    return result

pw4 :: DI -> DI
pw4 x =
  runST $
    do
    xM <- makeMutable x
    multOutInPlaceEff eff xM xM xM
    multOutInPlaceEff eff xM xM xM
    result <- readMutable xM
    return result
  where
  eff = multDefaultEffort x