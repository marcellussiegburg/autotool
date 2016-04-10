module Handler.DirektorErnennen where

import Import

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
  getOp :: Handler [Entity Student],
  setOp :: Entity Student -> Handler ()
}

getDirektorErnennenR :: SchuleId -> Handler Html
getDirektorErnennenR = postDirektorErnennenR

postDirektorErnennenR :: SchuleId -> Handler Html
postDirektorErnennenR schuleId = do
  _ <- runDB $ get404 schuleId
  let keineDirektoren = do
        studenten <- selectList [StudentSchuleId ==. schuleId] []
        direktoren <- selectList [DirektorSchuleId ==. schuleId] []
        return $ filter ((`notElem` fmap (direktorStudentId . entityVal) direktoren) . entityKey) studenten
  let studentenSeite = StudentenSeite {
        nullStudenten = MsgKeineStudentenErnennen,
        submit = BootstrapSubmit MsgDirektorErnennen "btn-success btn-block" [],
        erfolgMsg = MsgDirektorErnannt,
        formRoute = DirektorErnennenR schuleId,
        getOp = runDB keineDirektoren,
        setOp = \stud -> runDB $ insert_ $ Direktor (entityKey stud) schuleId
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
  defaultLayout $
    $(widgetFile "studentenFunktion")

-- | Wertet das Formular aus, wobei bei erfolgreichem Submit des Formulares, der Student @student@ die per @setOp@ in @eigenschaften@ festgelegte Rolle für die Schule @schule@ erhält, bzw. verliert (je nach @setOp@).
formAuswerten :: StudentenSeite -> Entity Student -> Handler ()
formAuswerten eigenschaften student = do
  ((ernennenResult, _), _) <- runFormPost $ form (submit eigenschaften) $ entityKey student
  case ernennenResult of
    FormMissing -> return ()
    FormFailure _ -> return ()
    FormSuccess _ -> do
      _ <- setOp eigenschaften student
      setMessageI $ erfolgMsg eigenschaften

-- | Erzeugt ein Formular mit einem Butten zum Ernennen bzw. Absetzen des Studenten @student@ für eine bestimmte Rolle. @submit@ legt Layout und Text des Submit-Buttons fest.
generiereForm :: BootstrapSubmit AutotoolMessage -> Entity Student -> Handler (Entity Student, (Widget, Enctype))
generiereForm submit' student = do
  form' <- generateFormPost $ form submit' $ entityKey student
  return (student, form')

form :: BootstrapSubmit AutotoolMessage -> StudentId -> Form StudentId
form submit' student = identifyForm (pack $ show student ++ "-1") $ renderDivs $
  areq hiddenField "" (Just student)
  <* bootstrapSubmit submit'
