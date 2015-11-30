{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Student.Type where

import Control.Types
import Operate.Crypt

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Student =
     Student { snr :: SNr
	     , unr :: UNr
	     , mnr :: MNr
	     , name :: Name
	     , vorname :: Name
             , email :: Email -- ^ falls Auth über Shibboleth,
               -- dann ist das EduPersonPrincipalName,
               -- (falls pseudonym, dann TargetedId?)
	     , passwort :: Crypt
             , next_passwort :: Crypt
	     }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Student])

instance Show Student where show = render . toDoc
