module Handler.Einsendung where

import Import
import Handler.EinsendungAnlegen (getAufgabeInstanz, getCrc, getDefaultParam)
import Handler.AufgabeKonfiguration (checkKonfiguration)
import Handler.Statistik (tdRadioField)

import Data.Text.Lazy (toStrict)
import Network.HTTP.Types (temporaryRedirect307)
import System.IO (readFile)
import System.Directory
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Control.Punkt (bepunkteStudentDB)
import qualified Control.Stud_Aufg.DB as EinsendungDB
import qualified Control.Stud_Aufg.Typ as Einsendung
import qualified Control.Types as T
import Operate.Bank (bank)
import qualified Operate.Param as P
import qualified Operate.Store as Store (Type (Input, Instant), location)
import Operate.Types (signed_task_config)
import Types.Signed (Signed (signature))
import qualified Util.Datei as D

getEinsendungR :: AufgabeId -> StudentId -> Handler Html
getEinsendungR = postEinsendungR

postEinsendungR :: AufgabeId -> StudentId -> Handler Html
postEinsendungR aufgabeId studentId = do
  meinsendung <- liftIO $ fmap listToMaybe $ EinsendungDB.get_snr_anr (T.SNr $ keyToInt studentId) $ T.ANr $ keyToInt aufgabeId
  aufgabe <- runDB $ get404 aufgabeId
  student <- runDB $ get404 studentId
  istTutor <- runDB $ not . null <$> selectList [TutorStudentId ==. studentId, TutorVorlesungId ==. aufgabeVorlesungId aufgabe] []
  einsendung <- liftIO $ maybe (EinsendungDB.put_blank (T.SNr $ keyToInt studentId) (T.ANr $ keyToInt aufgabeId)) return meinsendung
  mbewertung <- liftIO $ sequence $ fmap preEscapedToHtml . readFile . T.toString <$> Einsendung.report einsendung
  ((formResult, formWidget), formEnctype) <- runFormPost $ bewertungBearbeitenForm mbewertung
  case formResult of
    FormSuccess (mneueBewertung, mneuerWert) -> do
      let param = getDefaultParam student studentId aufgabe aufgabeId
          continue = case mneuerWert of
            Just T.Pending -> True
            _ -> isJust mneueBewertung
      if continue
         then do
           msg <- lift $ liftIO $ bank param {
               P.report = mneueBewertung,
               P.mresult = mplus mneuerWert (Einsendung.result einsendung) -- Warum input und instant nicht setzen?
             }
           setMessage $ toHtml $ pack msg
         else setMessageI MsgEinsendungBewertungNotwendig
    _ -> return ()
  input <- fixInput student studentId aufgabe aufgabeId einsendung
  instant <- fixInstant student studentId aufgabe aufgabeId einsendung
  -- INFO: Logging entfernt ("Super.view")
  maufgabenstellung <- liftIO $ sequence $ fmap (fmap preEscapedToHtml . readFile . T.toString) instant
  -- INFO: Logging entfernt ("Super.view")
  meinsendung' <- liftIO $ sequence $ fmap (readFile . T.toString) input
  defaultLayout $
    $(widgetFile "einsendung")

bewertungBearbeitenForm :: Maybe Html -> Form (Maybe Html, Maybe T.Wert)
bewertungBearbeitenForm mbewertung =
    identifyForm "bewertung" $ renderBootstrap3 BootstrapBasicForm $ (,)
      <$> aopt htmlField (addAttrs $ bfs MsgBewertungBearbeiten) (Just mbewertung)
      <*> areq (tdRadioField $ optionsPairs optionen) (bfs MsgEinsendungBewertung) {fsAttrs = []} (Just Nothing)
      <* bootstrapSubmit (BootstrapSubmit MsgEinsendungBewerten "btn-success" [])
  where
    addAttrs field = field {
        fsAttrs = ("rows", pack . show $ 2 + maybe 0 (length . lines . toStrict . renderHtml) mbewertung) : fsAttrs field
      }

optionen :: [(AutotoolMessage, Maybe T.Wert)]
optionen = [(MsgBehalten, Nothing), (MsgNein, Just T.No), (MsgAusstehend, Just T.Pending)] ++ fmap (\ i -> (MsgTextToMsg (pack $ show i), Just $ T.Ok i)) [1..10]

fixInput :: Student -> StudentId -> Aufgabe -> AufgabeId -> Einsendung.Stud_Aufg -> Handler (Maybe T.File)
fixInput student studentId aufgabe aufgabeId einsendung = case Einsendung.input einsendung of
  Just file -> return $ Just file
  Nothing -> liftIO $ do
    -- fix location of previous einsendung
    let p = getDefaultParam student studentId aufgabe aufgabeId
        d = Store.location Store.Input
                 p "latest" False
    file <- D.home d
    ex <- doesFileExist file
    if ex
      then do
        -- nur infile-location einschreiben
        let inf = T.fromCGI file
        bepunkteStudentDB
          (P.ident p) (P.anr p)
          Nothing
          Nothing (P.highscore p)
          (Just inf)
          Nothing
        return $ Just inf
      else return Nothing

fixInstant :: Student -> StudentId -> Aufgabe -> AufgabeId -> Einsendung.Stud_Aufg -> Handler (Maybe T.File)
fixInstant student studentId aufgabe aufgabeId einschreibung = case Einsendung.instant einschreibung of
  Just file -> return $ Just file
  Nothing -> do
    -- transitional:
    -- (try to) re-generate previous instance
    aufgabe' <- updateSignatur aufgabe aufgabeId
    (_, _, _, aufgabenstellung) <-
      getAufgabeInstanz
        (aufgabeServer aufgabe')
        (signed_task_config $ entityToAufgabe aufgabeId aufgabe')
        (getCrc (T.VNr $ keyToInt $ aufgabeVorlesungId aufgabe) (Just $ T.ANr $ keyToInt aufgabeId) (T.MNr $ unpack $ studentMatrikelNummer student))
    let p = getDefaultParam student studentId aufgabe' aufgabeId
        d = Store.location Store.Instant
               p "latest" False
    file <- liftIO $ D.schreiben d $ show aufgabenstellung
    let inst = T.fromCGI file
    liftIO $ bepunkteStudentDB
      (P.ident p) (P.anr p)
      (Just inst)
      Nothing (P.highscore p)
      Nothing
      Nothing
    return $ Just inst

updateSignatur :: Aufgabe -> AufgabeId -> Handler Aufgabe
updateSignatur aufgabe aufgabeId =
  if aufgabeSignatur aufgabe == "missing"
  then do
    esigned <-
      checkKonfiguration
        (aufgabeServer aufgabe)
        (aufgabeTyp aufgabe)
        (aufgabeKonfiguration aufgabe)
    case esigned of
      Left fehler -> do
        setMessage fehler
        redirectWith temporaryRedirect307 $ VorlesungR $ aufgabeVorlesungId aufgabe
      Right signed -> do
        aufgabe'' <- runDB $ updateGet aufgabeId [AufgabeSignatur =. pack (signature signed)]
        setMessageI MsgAufgabeSignaturUpdate
        return aufgabe''
  else return aufgabe
