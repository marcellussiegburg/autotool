module Handler.Einsendung where

import Import
import Handler.Aufgabe (getDefaultParam)

import qualified Control.Aufgabe.Typ as Aufgabe
import qualified Control.Aufgabe.DB as AufgabeDB
import qualified Control.Stud_Aufg.DB as EinsendungDB
import qualified Control.Stud_Aufg.Typ as Einsendung
import qualified Control.Student.DB as StudentDB
import qualified Control.Student.Type as Student
import qualified Control.Vorlesung.DB as VorlesungDB
import qualified Control.Types as T

getEinsendungR :: AufgabeId -> StudentId -> Handler Html
getEinsendungR = postEinsendungR

postEinsendungR :: AufgabeId -> StudentId -> Handler Html
postEinsendungR aufgabeId studentId = do
  meinsendung <- liftIO $ liftM listToMaybe $ EinsendungDB.get_snr_anr (T.SNr studentId) $ T.ANr aufgabeId
  maufgabe <- liftIO $ liftM listToMaybe $ AufgabeDB.get_this $ T.ANr aufgabeId
  mstudent <- liftIO $ liftM listToMaybe $ StudentDB.get_snr $ T.SNr studentId
  (einsendung, aufgabe, student) <- do
    mval <- runMaybeT $ do
      e <- MaybeT . return $ meinsendung
      a <- MaybeT . return $ maufgabe
      s <- MaybeT . return $ mstudent
      return (e, a, s)
    case mval of
      Nothing -> permissionDeniedI MsgNichtAutorisiert
      Just v -> return v 
  let einsendung' = "" :: Html
      mbewertung = Nothing :: Maybe Html
      istTutor = False
      aufgabenstellung = "" :: Html
  (formWidget, formEnctype) <- generateFormPost $ einsendungBearbeitenForm
  defaultLayout $
    $(widgetFile "einsendung")

einsendungBearbeitenForm :: Form ()
einsendungBearbeitenForm =
  identifyForm "einsendung" $ renderBootstrap3 BootstrapBasicForm $
    bootstrapSubmit (BootstrapSubmit MsgEinsendungBewerten "btn-success" [])
