{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Laws
    Description :  auxiliary functions for testable properties  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Auxiliary functions for testable properties.
-}

module Numeric.AERN.RmToRn.Laws where

import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Misc.Debug

roundedRefinementIsotoneDom ::
    (RefOrd.PartialComparison t, 
     RefOrd.ArbitraryOrderedTuple (DomainBox f),
     Show (DomainBox f),
     Show (Domain f),
     Show t) 
    =>
    f ->
    String ->
    (ei -> (DomainBox f) -> t) ->
    (ei -> (DomainBox f) -> t) ->
    (RefOrd.LEPair (DomainBox f)) -> 
    ei -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
roundedRefinementIsotoneDom _ contextDescription exprUp exprDn (RefOrd.LEPair (dom1L, dom1H)) effort effortComp =
--    unsafePrint
--    (
--        "roundedRefinementIsotoneDom: "
--        ++ "\n dom1L = " ++ show dom1L
--        ++ "\n dom1H = " ++ show dom1H
--        ++ "\n resUp = " ++ show resUp
--        ++ "\n resDn = " ++ show resDn
--    ) $
    case RefOrd.pLeqEff effortComp resDn resUp of
        Just b -> b
        _ -> True
    where
    resUp = check $ exprUp effort dom1H
    resDn = check $ exprDn effort dom1L
    check = id -- detectIllegalValues $ contextDescription ++ " refinement isotone"
    
