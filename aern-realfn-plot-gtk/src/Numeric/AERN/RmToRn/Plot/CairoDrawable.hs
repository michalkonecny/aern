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

import Numeric.AERN.RmToRn.Plot.Params (CanvasParams,FnPlotStyle)

import Numeric.AERN.RmToRn.Domain

import Graphics.Rendering.Cairo (Render)

class
    CairoDrawableFn f
    where
    type CairoDrawFnEffortIndicator f
    cairoDrawFnDefaultEffort ::
        f -> (CairoDrawFnEffortIndicator f)
    {-|
        Plot the graph of a uni-variate function on the active cairo canvas.
        A multi-variate function is transformed into an univariate function
        by ranging all other variables over their entire domains and taking
        the union of all the uni-variate functions thus obtained.
     -}
    cairoDrawFn ::
        CairoDrawFnEffortIndicator f -> 
        CanvasParams (Domain f) ->
        ((Domain f, Domain f) -> (Double, Double)) {-^ conversion from [0,1]^2 (origin bottom left) to screen coords -} ->
        FnPlotStyle ->
        Var f ->
        f -> 
        Render ()

