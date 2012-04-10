{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Aufgabe.Typ where

import Control.Types

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

-- | das sollte exactly das sein, was auch in DB-tabelle  steht

data Aufgabe  =
     Aufgabe { anr :: ANr
	     , vnr :: VNr
	     , name :: Name
             , server :: Server
             , typ :: Typ               
	     , config :: Config
             , signature :: Signature  
	     , remark :: Remark
	     , highscore :: HiLo
	     , status :: Status
	     , von :: Time
	     , bis :: Time
	     -- | das wird bei DB-zugriff berechnet (NOW between VON and BIS)
	     -- damit wir außerhalb der DB keine Uhr brauchen
	     -- und keine zeiten vergleichen müssen
	     , timeStatus :: TimeStatus
	     }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Aufgabe])


-- | for backwards compatibility
current :: Aufgabe -> Bool
current a = Current == timeStatus a



