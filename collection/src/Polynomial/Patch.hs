{-# language DeriveDataTypeable #-}
{-# language TypeFamilies #-}

module Polynomial.Patch where

import Data.Typeable
import Autolib.Reader
import Autolib.ToDoc
import Control.Applicative hiding ((<|>))

class Pattern p where
     type Base p 
     match :: p -> Base p -> Bool
     inject :: Base p -> p
     base :: p -> Base p
     -- | randomly obfuscate
     robfuscate :: Base p -> IO p

     -- | with dummy argument
     default_ :: p -> Base p

     -- any attempt to call this function
     -- will give "type function not injective"
     -- default_ :: Base p
     

data Patch v = Any | This v deriving Typeable

instance ToDoc v => ToDoc (Patch v) where
    toDoc p = case p of
        Any -> text "_" ; This v -> toDoc v
instance Reader v => Reader (Patch v) where
    reader = do my_reservedOp "_" ; return Any
        <|> ( This <$> reader )

