module Handler.DirektorErnennen where

import Import
import qualified Control.Direktor.DB as DirektorDB
import qualified Control.Student.DB as StudentDB
import qualified Control.Student.Type as Student
import Control.Student.Type (Student)
import Control.Types

-- | Definiert die Parameter für eine Html-Seite zum Setzen einer Rolle für Studenten. Dient als Hilfsdatenstruktur für 'rolleSetzenListe'.
-- * @nullStudenten@ ist die Nachricht, die ausgegeben werden soll, wenn kein Student zur entsprechenden Rollenänderung zur Verfügung steht.
-- * @submit@ ist der Button, der in den Formularen angezeigt werden soll.
-- * @erfolgMsg@ ist die Nachricht, die dem Nutzer angezeigt werden soll, wenn eine Rollenänderung stattgefunden hat.
-- * @formRoute@ ist die Route, zu der der Nutzer geleitet werden soll, wenn der das Formular absendet.
-- * @getOp@ ist die Operation, mit der man alle für die Rollenänderung relevanten Studenten erhält (je nach dem: alle Studenten der Schule, die diese Rolle für die entsprechende Abteilung bereits haben oder noch nicht haben)
-- * @setOp@ ist die Operation, mit der ein Student zur entsprechenden Rolle ernannt oder von ihr abgesetzt wird.
data StudentenSeite = StudentenSeite {
  nullStudenten :: AutotoolMessage,
  submit :: BootstrapSubmit AutotoolMessage,
  erfolgMsg :: AutotoolMessage,
  formRoute :: Route Autotool,
  getOp :: Handler [Student],
  setOp :: Student -> Handler ()
}

getDirektorErnennenR :: SchuleId -> Handler Html
getDirektorErnennenR = postDirektorErnennenR

postDirektorErnennenR :: SchuleId -> Handler Html
postDirektorErnennenR schuleId = do
  schule <- runDB $ get404 schuleId
  studenten' <- lift $ StudentDB.get_unr $ UNr $ keyToInt schuleId
  let schule' = entityToSchule schuleId schule
      fromSNr (SNr snr) = snr
  let keineDirektoren = do
        direktoren <- DirektorDB.get_directors schule'
        return $ deleteFirstsBy ((==) `on` Student.snr) studenten' direktoren
  let studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineStudentenErnennen,
        submit = BootstrapSubmit MsgDirektorErnennen "btn-success btn-block" [],
        erfolgMsg = MsgDirektorErnannt,
        formRoute = DirektorErnennenR schuleId,
        getOp = lift keineDirektoren,
        setOp = \stud -> runDB $ insert_ $ Direktor (fromSNr $ Student.snr stud) schuleId
      }
  rolleSetzenListe studentenSeite

-- | Gibt eine Html-Seite für das Setzen einer Rolle zurück, @rolleSetzenListe@ beinhaltet auch die Auswertung der in der Seite enthaltenen Formulare.
-- * @eigenschaften@ sind die Eigenschaften und Operationen, die das Aussehen der Html-Seite und die Wirkung der Html-Formulare bestimmen.
rolleSetzenListe :: StudentenSeite -> Handler Html
rolleSetzenListe eigenschaften = do
  studentenForms <- do
    studenten' <- getOp eigenschaften
    _ <- mapM (formAuswerten eigenschaften) studenten'
    studenten'' <- getOp eigenschaften
    mapM (generiereForm (submit eigenschaften)) studenten''
  defaultLayout $ do
    $(widgetFile "studentenFunktion")

-- | Wertet das Formular aus, wobei bei erfolgreichem Submit des Formulares, der Student @student@ die per @setOp@ in @eigenschaften@ festgelegte Rolle für die Schule @schule@ erhält, bzw. verliert (je nach @setOp@).
formAuswerten :: StudentenSeite -> Student -> Handler ()
formAuswerten eigenschaften student = do
  let SNr snr = Student.snr student
  ((ernennenResult, _), _) <- runFormPost $ form (submit eigenschaften) snr
  case ernennenResult of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess _ -> do
      _ <- (setOp eigenschaften) student
      setMessageI $ erfolgMsg eigenschaften

-- | Erzeugt ein Formular mit einem Butten zum Ernennen bzw. Absetzen des Studenten @student@ für eine bestimmte Rolle. @submit@ legt Layout und Text des Submit-Buttons fest.
generiereForm :: BootstrapSubmit AutotoolMessage -> Student -> Handler (Student, (Widget, Enctype))
generiereForm submit' student = do
  let SNr snr = Student.snr student
  form' <- generateFormPost $ form submit' snr
  return (student, form')

form :: BootstrapSubmit AutotoolMessage -> StudentId -> Form StudentId
form submit' student = identifyForm (pack $ show student ++ "-1") $ renderDivs $
  areq hiddenField "" (Just student)
  <* bootstrapSubmit submit'

