module Handler.Gruppen where

import Import
import qualified Control.Gruppe.DB as GruppeDB
import qualified Control.Gruppe.Typ as Gruppe
import qualified Control.Stud_Grp.DB as EinschreibungDB
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
  istTutor' <- istAutorisiert mid $ VorlesungR vorlesung
  vorlesung' <- lift $ VorlesungDB.get_this $ VNr vorlesung
  _ <- mapM (formAuswerten mid gruppenBesucht) gruppen
  gruppenBesucht' <- lift $ fmap (concat . maybeToList) $ mapM (\ms -> GruppeDB.get_attended (VNr vorlesung) (SNr ms)) mid
  gruppenForms <- mapM (generiereForm gruppenBesucht') gruppen
  darfGruppenSehen <- istAutorisiert mid $ AufgabenAktuellR vorlesung
  let istTutor = istTutor' == Just True
      fromName name = let Name n = name
                      in n
      fromGNr gnr = let GNr nr = gnr
                    in nr
  defaultLayout $ do
    $(widgetFile "gruppen")

formAuswerten :: Maybe (AuthId Autotool) -> [Gruppe.Gruppe] -> Gruppe.Gruppe -> Handler ()
formAuswerten mid gruppenBesucht gruppe = do
  let gnr = let GNr nr = Gruppe.gnr gruppe in nr
      maybeChange f g = mapM (\ms -> lift $ f (SNr ms) g) mid
  if Gruppe.gnr gruppe `elem` map Gruppe.gnr gruppenBesucht
    then do
      ((austragenResult, _), _) <- runFormPost $ austragenForm gnr
      case austragenResult of
        FormMissing -> return ()
        FormFailure _ -> return ()
        FormSuccess g -> do
          _ <- maybeChange EinschreibungDB.delete $ GNr g
          setMessageI MsgGruppeAusgetragen
    else do
      ((einschreibenResult, _), _) <- runFormPost $ einschreibenForm gnr
      case einschreibenResult of
        FormMissing -> return ()
        FormFailure _ -> return ()
        FormSuccess g -> do
          einschreibungen <- lift $ EinschreibungDB.attendance $ Gruppe.gnr gruppe
          if Gruppe.maxStudents gruppe <= einschreibungen
             then do
               setMessageI MsgGruppeVoll
             else do
               _ <- maybeChange EinschreibungDB.insert $ GNr g
               _ <- mapM ((maybeChange EinschreibungDB.delete) . Gruppe.gnr) gruppenBesucht
               setMessageI MsgGruppeEingeschrieben

generiereForm :: [Gruppe.Gruppe] -> Gruppe.Gruppe -> Handler (Gruppe.Gruppe, Integer, (Widget, Enctype))
generiereForm gruppenBesucht gruppe = do
  let gnr = let GNr nr = Gruppe.gnr gruppe in nr
  einschreibungen <- lift $ EinschreibungDB.attendance $ Gruppe.gnr gruppe
  form <-
    if Gruppe.gnr gruppe `elem` map Gruppe.gnr gruppenBesucht
      then generateFormPost $ austragenForm gnr
      else generateFormPost $ einschreibenForm gnr
  return (gruppe, einschreibungen, form)

einschreibenForm :: GruppeId -> Form GruppeId
einschreibenForm gruppe = identifyForm (pack $ show gruppe ++ "-1") $ renderDivs $
  areq hiddenField "" (Just gruppe)
  <* bootstrapSubmit (BootstrapSubmit MsgEinschreiben "btn-success btn-block" [])

austragenForm :: GruppeId -> Form GruppeId
austragenForm gruppe = identifyForm (pack $ show gruppe ++ "-2") $ renderDivs $
  areq hiddenField "" (Just gruppe)
  <* bootstrapSubmit (BootstrapSubmit MsgAustragen "btn-danger btn-block" [])
