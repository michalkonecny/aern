{-# LANGUAGE DeriveDataTypeable #-}
{-|
    Module      :  Numeric.AERN.Basics.PartialOrdering
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

catchAERNExceptions :: a -> Either String a
catchAERNExceptions e =
    unsafePerformIO $
        catch (return (Right e))
            (\(AERNException msg) -> return (Left msg))

