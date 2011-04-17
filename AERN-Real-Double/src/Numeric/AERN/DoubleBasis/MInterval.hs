
module Numeric.AERN.DoubleBasis.MInterval 
(
    MDI,
    makeMutable,writeMutable,readMutable,
    unsafeMakeMutable,unsafeWriteMutable,unsafeReadMutable,
    assignMutable,swapMutable,cloneMutable,
    addInInPlace,addOutInPlace,
    subtrInInPlace,subtrOutInPlace,
    absInInPlace,absOutInPlace,
    multInInPlace,multOutInPlace,
    powerToNonnegIntInInPlace,powerToNonnegIntOutInPlace,
    divInInPlace,divOutInPlace
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