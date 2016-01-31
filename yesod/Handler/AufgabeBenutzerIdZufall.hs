module Handler.AufgabeBenutzerIdZufall where

import Import
import System.Random (randomRIO)

getAufgabeBenutzerIdZufallR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeBenutzerIdZufallR server aTyp konfiguration = do
  benutzerId <- lift $ liftM (pack . show) $ randomRIO (0, 999999 :: Integer)
  redirect $ AufgabeTestenR server aTyp konfiguration benutzerId
