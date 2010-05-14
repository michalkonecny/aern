{-# LANGUAGE DeriveDataTypeable #-}
{-|
    Module      :  Numeric.AERN.Basics.Exception
    Description :  exception type to signal AERN specific errors  
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
    
    * no operation should ever return NaN; when a NaN represents "any value"/bottom
      (eg with 0/0 or &infin - &infin),
      it should be rounded up to +&infin; or down to -&infin;; when a NaN represent
      an illegal argument exception (eg with log(-1)), an AERN NaN exception should
      be thrown with an appropriate message
    
    * intervals support infinite endpoints
    
    * polynomial coefficients must be finite, overflows of coefficients detected
      and result in a special polynomial denoting the constant function +&infin; or -&infin;
-}
module Numeric.AERN.Basics.Exception where

import Prelude hiding (catch)

import Control.Exception
import Data.Typeable
import System.IO.Unsafe

data AERNException =
    AERNException String
    | AERNNaNException String
    deriving (Show, Typeable)

instance Exception AERNException

evalCatchAERNExceptions :: t -> Either String t
evalCatchAERNExceptions a =
    unsafePerformIO $ catch (evaluateEmbed a) handler
    where
    handler (AERNException msg) =
        do
        putStrLn $ "caught AERN exception: " ++ msg
        return (Left msg)
    handler (AERNNaNException msg) =
        do 
        putStrLn $ "caught AERN NaN exception: " ++ msg
        return (Left $ "NaN: " ++ msg)
    evaluateEmbed a =
        do
        aa <- evaluate a
        return $ Right aa

evalCatchNaNExceptions :: t -> Either String t
evalCatchNaNExceptions a =
    unsafePerformIO $ catch (evaluateEmbed a) handler
    where
    handler (AERNNaNException msg) = 
        do 
        putStrLn $ "caught AERN NaN exception: " ++ msg
        return (Left msg)
    handler e = throw e -- rethrow other exceptions
    evaluateEmbed a =
        do
        aa <- evaluate a
        return $ Right aa

raisesAERNException :: t -> Bool
raisesAERNException a =
    case (evalCatchAERNExceptions a) of
        (Left _) -> True
        _ -> False

raisesNaNException :: t -> Bool
raisesNaNException a =
    case (evalCatchNaNExceptions a) of
        (Left _) -> True
        _ -> False

