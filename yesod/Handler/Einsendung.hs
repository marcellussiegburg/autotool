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

import qualified Control.Aufgabe.Typ as Aufgabe
import qualified Control.Aufgabe.DB as AufgabeDB
import Control.Punkt (bepunkteStudentDB)
import qualified Control.Stud_Aufg.DB as EinsendungDB
import qualified Control.Stud_Aufg.Typ as Einsendung
import qualified Control.Student.DB as StudentDB
import qualified Control.Student.Type as Student
import qualified Control.Tutor.DB as TutorDB
import qualified Control.Vorlesung.Typ as Vorlesung
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
  meinsendung <- liftIO $ liftM listToMaybe $ EinsendungDB.get_snr_anr (T.SNr studentId) $ T.ANr aufgabeId
  maufgabe <- liftIO $ liftM listToMaybe $ AufgabeDB.get_this $ T.ANr aufgabeId
  mstudent <- liftIO $ liftM listToMaybe $ StudentDB.get_snr $ T.SNr studentId
  (aufgabe, student) <- do
    mval <- runMaybeT $ do
      a <- MaybeT . return $ maufgabe
      s <- MaybeT . return $ mstudent
      return (a, s)
    case mval of
      Nothing -> permissionDeniedI MsgNichtAutorisiert
      Just v -> return v
  tutored <- liftIO $ TutorDB.get_tutored student
  let istTutor = Aufgabe.vnr aufgabe `elem` fmap Vorlesung.vnr tutored
  einsendung <- liftIO $ maybe (EinsendungDB.put_blank (Student.snr student) (Aufgabe.anr aufgabe)) return meinsendung
  mbewertung <- liftIO $ sequence $ fmap (liftM preEscapedToHtml . readFile . T.toString) $ Einsendung.report einsendung
  ((formResult, formWidget), formEnctype) <- runFormPost $ bewertungBearbeitenForm mbewertung
  case formResult of
    FormSuccess (mneueBewertung, mneuerWert) -> do
      let param = getDefaultParam student aufgabe
          continue = case mneuerWert of
            Just T.Pending -> True
            _ -> isJust mneueBewertung
      if continue
         then do
           msg <- lift $ liftIO $ bank param {
               P.report = mneueBewertung,
               P.mresult = maybe (Einsendung.result einsendung) Just mneuerWert -- Warum input und instant nicht setzen?
             }
           setMessage $ toHtml $ pack msg
         else setMessageI $ MsgEinsendungBewertungNotwendig
    _ -> return ()
  input <- fixInput student aufgabe einsendung
  instant <- fixInstant student aufgabe einsendung
  -- INFO: Logging entfernt ("Super.view")
  maufgabenstellung <- liftIO $ sequence $ fmap (liftM preEscapedToHtml . readFile . T.toString) instant
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

fixInput :: Student.Student -> Aufgabe.Aufgabe -> Einsendung.Stud_Aufg -> Handler (Maybe T.File)
fixInput student aufgabe einsendung = case Einsendung.input einsendung of
  Just file -> return $ Just file
  Nothing -> liftIO $ do
    -- fix location of previous einsendung
    let p = getDefaultParam student aufgabe
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
      else return $ Nothing

fixInstant :: Student.Student -> Aufgabe.Aufgabe -> Einsendung.Stud_Aufg -> Handler (Maybe T.File)
fixInstant student aufgabe einschreibung = case Einsendung.instant einschreibung of
  Just file -> return $ Just file
  Nothing -> do
    -- transitional:
    -- (try to) re-generate previous instance
    aufgabe' <- updateSignatur aufgabe
    (_, _, _, aufgabenstellung) <-
      getAufgabeInstanz
        (pack $ T.toString $ Aufgabe.server aufgabe')
        (signed_task_config aufgabe')
        (getCrc (Aufgabe.vnr aufgabe) (Just $ Aufgabe.anr aufgabe) (Student.mnr student))
    let p = getDefaultParam student aufgabe'
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

updateSignatur :: Aufgabe.Aufgabe -> Handler Aufgabe.Aufgabe
updateSignatur aufgabe =
  if T.toString (Aufgabe.signature aufgabe) == "missing"
  then do
    esigned <-
      checkKonfiguration
        (pack $ T.toString $ Aufgabe.server aufgabe)
        (pack $ T.toString $ Aufgabe.typ aufgabe)
        (pack $ T.toString $ Aufgabe.config aufgabe)
    case esigned of
      Left fehler -> do
        setMessage fehler
        let T.VNr vorlesungId = Aufgabe.vnr aufgabe
        redirectWith temporaryRedirect307 $ VorlesungR vorlesungId
      Right signed -> do
        let aufgabe'' = aufgabe { Aufgabe.signature = T.fromCGI $ signature signed }
        liftIO $ AufgabeDB.put_signature (Just $ Aufgabe.anr aufgabe'') aufgabe''
        setMessageI MsgAufgabeSignaturUpdate
        return aufgabe''
  else return aufgabe
