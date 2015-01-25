module Handler.Gruppen where

import Import
import qualified Control.Gruppe.DB as GruppeDB
import qualified Control.Gruppe.Typ as Gruppe
import qualified Control.Vorlesung.DB as VorlesungDB
import qualified Control.Vorlesung.Typ as Vorlesung
import Control.Types

getGruppenR :: VorlesungId -> Handler Html
getGruppenR = postGruppenR

postGruppenR :: VorlesungId -> Handler Html
postGruppenR vorlesung = do
  mid <- maybeAuthId
  gruppen <- lift $ GruppeDB.get_this $ VNr vorlesung
  gruppenBesucht <- lift $ fmap (concat . maybeToList) $ mapM (\ms -> GruppeDB.get_attended (VNr vorlesung) (SNr ms)) mid
  istTutor' <- lift $ istAutorisiert mid $ VorlesungR vorlesung
  vorlesung' <- lift $ VorlesungDB.get_this $ VNr vorlesung
  let istTutor = istTutor' == Just True
      fromName name = let Name n = name
                      in n
      fromGNr gnr = let GNr nr = gnr
                    in nr
      gruppenBesuchtIds = map Gruppe.gnr gruppenBesucht
  defaultLayout $ do
    $(widgetFile "gruppen")
