{-# LANGUAGE DeriveDataTypeable #-}
{-|
    Module      :  Numeric.AERN.Basics.Exception
    Description :  extension of Prelude.Ordering with non-comparable variant  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    An exception type to be thrown on various arithmetic exceptions
    whose proper pure handling would be inefficient.
    'Control.Exception.ArithmeticException' is not
    flexible enough, eg because often we will not be checking
    overflows but NaNs instead.
    
    The default exeception policy is:
    
    * no operation should return NaN but raise an AERN exception instead
    
    * intervals support infinite endpoints, overflows are OK
    
    * polynomial coefficients must be finite, overflows of coefficients detected
      and result in a special polynomial denoting the constant infinity
-}
module Numeric.AERN.Basics.Exception where

import Prelude hiding (catch)

import Control.Exception
import Data.Typeable
import System.IO.Unsafe

data AERNException =
    AERNException String
    deriving (Show, Typeable)

instance Exception AERNException

evalCatchAERNExceptions :: t -> Either String t
evalCatchAERNExceptions a =
    unsafePerformIO $ catch (evaluateEmbed a) handler
    where
    handler (AERNException msg) = 
        return (Left msg)
    evaluateEmbed a =
        do
        aa <- evaluate a
        return $ Right aa

raisesAERNException :: t -> Bool
raisesAERNException a =
    case (evalCatchAERNExceptions a) of
        (Left _) -> True
        _ -> False

