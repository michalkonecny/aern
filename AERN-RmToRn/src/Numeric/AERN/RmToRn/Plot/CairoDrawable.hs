{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Plot.CairoDrawable
    Description :  abstraction for drawing on a Cairo canvas
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Abstraction for drawing on a Cairo canvas.
-}
module Numeric.AERN.RmToRn.Plot.CairoDrawable 
(
    CairoDrawableFn(..),
)
where

import Numeric.AERN.RmToRn.Plot.Params (PlotParams,FnPlotStyle)

import Numeric.AERN.RmToRn.Domain

import Graphics.Rendering.Cairo (Render)

class
    CairoDrawableFn f
    where
    type CairoDrawFnEffortIndicator f
    cairoDrawFnDefaultEffort ::
        f -> (CairoDrawFnEffortIndicator f)
    {-|
        Plot an element on the active cairo canvas.
     -}
    cairoDrawFn ::
        CairoDrawFnEffortIndicator f -> 
        PlotParams (Domain f) ->
        ((Domain f, Domain f) -> (Double, Double)) {-^ conversion from [0,1]^2 (origin bottom left) to screen coords -} ->
        FnPlotStyle ->
        f -> 
        Render ()

