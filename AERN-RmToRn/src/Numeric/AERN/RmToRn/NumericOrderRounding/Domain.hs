{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RmToRn.NumericOrderRounding.Domain
    Description :  operations focusing on function domains  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Operations focusing on function domains.
-}

module Numeric.AERN.RmToRn.NumericOrderRounding.Domain where

class HasDomain f where
    type Domain f
    getDomainStartUp :: f -> Domain f
    getDomainStartDn :: f -> Domain f
    getDomainEndUp :: f -> Domain f
    getDomainEndDn :: f -> Domain f
    