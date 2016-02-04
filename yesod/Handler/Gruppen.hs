module Handler.Gruppen where

import Import
import Control.Types
import Data.Time (getCurrentTime)

getGruppenR :: VorlesungId -> Handler Html
getGruppenR = postGruppenR

postGruppenR :: VorlesungId -> Handler Html
postGruppenR vorlesungId = do
  authId <- requireAuthId
  vorlesung <- runDB $ get404 vorlesungId
  gruppen <- runDB $ selectList [GruppeVorlesungId ==. vorlesungId] []
  einschreibungen <- runDB $ selectKeysList [EinschreibungId <-. map (EinschreibungKey authId . entityKey) gruppen] []
  let gs `haben` es = map (\g -> EinschreibungKey authId (entityKey g) `elem` es) gs
  _ <- mapM (formAuswerten authId einschreibungen) $ zip (gruppen `haben` einschreibungen) gruppen
  einschreibungen' <- runDB $ selectKeysList [EinschreibungId <-. map (EinschreibungKey authId . entityKey) gruppen] []
  let wirdBesucht = gruppen `haben` einschreibungen'
  gruppenForms <- mapM (generiereForm authId) $ zip wirdBesucht gruppen
  darfGruppenSehen <- istAutorisiert (Just authId) $ AufgabenAktuellR vorlesungId
  zeit <- liftIO $ getCurrentTime
  defaultLayout $ do
    $(widgetFile "gruppen")

formAuswerten :: AuthId Autotool -> [EinschreibungId] -> (Bool, Entity Gruppe) -> Handler ()
formAuswerten authId gruppenBesucht (eingeschrieben, gruppe) =
  if eingeschrieben
  then do
    ((austragenResult, _), _) <- runFormPost $ austragenForm $ entityKey gruppe
    case austragenResult of
      FormMissing -> return ()
      FormFailure _ -> return ()
      FormSuccess g -> do
        runDB $ delete $ EinschreibungKey authId g
        setMessageI MsgGruppeAusgetragen
  else do
    ((einschreibenResult, _), _) <- runFormPost $ einschreibenForm $ entityKey gruppe
    case einschreibenResult of
      FormMissing -> return ()
      FormFailure _ -> return ()
      FormSuccess g -> do
        einschreibungen <- liftM length $ runDB $ selectList [EinschreibungGruppeId ==. entityKey gruppe] []
        if gruppePlaetze (entityVal gruppe) <= einschreibungen
           then do
             setMessageI MsgGruppeVoll
           else do
             _ <- runDB $ insert $ Einschreibung authId g
             runDB $ deleteWhere [EinschreibungId <-. gruppenBesucht]
             setMessageI MsgGruppeEingeschrieben

generiereForm :: AuthId Autotool -> (Bool, Entity Gruppe) -> Handler (Entity Gruppe, Bool, Int, (Widget, Enctype))
generiereForm authId (eingeschrieben, gruppe) = do
  einschreibungen <- liftM length $ runDB $ selectList [EinschreibungGruppeId ==. entityKey gruppe] []
  darfGruppeBearbeiten <- (== Just True) <$> (istAutorisiert (Just authId) $ GruppeR $ entityKey gruppe)
  form <-
    if eingeschrieben
       then generateFormPost $ austragenForm $ entityKey gruppe
       else generateFormPost $ einschreibenForm $ entityKey gruppe
  return (gruppe, darfGruppeBearbeiten, einschreibungen, form)

einschreibenForm :: GruppeId -> Form GruppeId
einschreibenForm gruppe = identifyForm (pack $ show gruppe ++ "-1") $ renderDivs $
  areq hiddenField "" (Just gruppe)
  <* bootstrapSubmit (BootstrapSubmit MsgEinschreiben "btn-success btn-block" [])

austragenForm :: GruppeId -> Form GruppeId
austragenForm gruppe = identifyForm (pack $ show gruppe ++ "-2") $ renderDivs $
  areq hiddenField "" (Just gruppe)
  <* bootstrapSubmit (BootstrapSubmit MsgAustragen "btn-danger btn-block" [])
