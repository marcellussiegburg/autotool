module Handler.Statistik where

import Import
import Handler.EinsendungAnlegen (getDefaultParam)

import qualified Control.Stud_Aufg.DB as EinsendungDB
import qualified Control.Stud_Aufg.Typ as Einsendung
import qualified Control.Student.Type as Student
import qualified Control.Vorlesung.DB as VorlesungDB
import qualified Control.Types as T
import Operate.Bank (bank)
import qualified Operate.Param as P
import qualified Util.Datei as D

import Control.Exception (SomeException (SomeException), catch)
import Control.Monad (unless)
import Data.Set (member, fromList)
import Data.List (head, find)

data ErgebnisEintrag = ErgebnisEintrag {
    studentId :: StudentId,
    matrikel :: Text,
    vorname :: Text,
    nachname :: Text,
    okays :: Maybe Int,
    neins :: Maybe Int,
    mergebnis :: Maybe T.Wert,
    form :: (Widget, Enctype)
  }


data Ergebnis = Okay {punkte :: Int, größe :: Int} | Nein | Ausstehend

aktion :: Text
aktion = "aktion"

neuBewertenLabel :: Int -> Text
neuBewertenLabel number = mappend "neuBewerten" $ pack $ show number

data Aktion = Bearbeiten | CacheLeeren Text deriving (Show, Read)

getStatistikR :: AufgabeId -> Handler Html
getStatistikR = postStatistikR

postStatistikR :: AufgabeId -> Handler Html
postStatistikR aufgabeId = do
  maufgabe <- runDB $ get aufgabeId
  aufgabe <- case maufgabe of
    Nothing -> permissionDeniedI MsgNichtAutorisiert
    Just a -> return a
  studenten' <- liftIO $ VorlesungDB.steilnehmer $ T.VNr $ keyToInt $ aufgabeVorlesungId aufgabe
  studenten'' <- runDB $ mapM (\(T.SNr s) -> selectList [StudentId ==. intToKey s] []) $ fmap Student.snr studenten'
  let studenten = concat studenten''
  _ <- runMaybeT $ do
    maktion <- lookupPostParam aktion
    aktion' <- MaybeT . return $ maktion
    lift $ case read $ unpack aktion' of
      Bearbeiten -> bewertungenSchreiben studenten aufgabe aufgabeId
      CacheLeeren m -> cacheLeeren m (aufgabeVorlesungId aufgabe) aufgabeId
  einsendungen <- liftIO $ EinsendungDB.get_anr $ T.ANr $ keyToInt aufgabeId
  ergebnisse <- getErgebnisListe einsendungen studenten
  optionen' <- optionsPairs optionen
  defaultLayout $
    $(widgetFile "statistik")

cacheLeeren :: Text -> VorlesungId -> AufgabeId -> Handler ()
cacheLeeren matrikel' vorlesungId aufgabeId = do
  let d =  D.Datei {
      D.pfad = [ "autotool", "cache"
               , show vorlesungId
               , show aufgabeId
               ]
    , D.name = unpack matrikel'
    , D.extension = "cache"
    }
  lift $ D.loeschen d `catch` \ (SomeException _) -> return ()
  setMessageI $ MsgCacheGeleert $ pack $ show d

bewertungenSchreiben :: [Entity Student] -> Aufgabe -> AufgabeId -> Handler ()
bewertungenSchreiben studenten aufgabe aufgabeId = do
  let getResult s = lookupPostParam $ neuBewertenLabel $ keyToInt $ entityKey s
  mapM_ (\s -> do r <- getResult s
                  let i = maybe 1 (read . unpack) r - 1
                  bewertungSchreiben s aufgabe aufgabeId (snd $ optionen !! i)) studenten

bewertungSchreiben :: Entity Student -> Aufgabe -> AufgabeId -> Maybe T.Wert -> Handler Text
bewertungSchreiben _ _ _ Nothing = return ""
bewertungSchreiben student aufgabe aufgabeId (Just bewertung) = do
  let message = "Bewertung durch Tutor" :: Text -- TODO: Übersetzung?
  lift $ pack <$> bank (getDefaultParam (entityVal student) (entityKey student) aufgabe aufgabeId) {
      P.report = Just $ preEscapedToHtml message,
      P.mresult = Just bewertung
    }

getErgebnisListe :: [Einsendung.Stud_Aufg] -> [Entity Student] -> Handler [ErgebnisEintrag]
getErgebnisListe einsendungen studenten =
  let einsender = fromList $ fmap Einsendung.snr einsendungen
      getStudent e = find (\ s -> T.SNr (keyToInt $ entityKey s) == Einsendung.snr e) studenten
      getEintrag e =
        let s = getStudent e
        in maybeToList $ fmap (\ s' -> getErgebnisEintrag s' $ Just e) s
      einsendungen' = concat $ fmap getEintrag einsendungen
      keineEinsendungen = concat $ fmap (\s -> if T.SNr (keyToInt $ entityKey s) `member` einsender then [] else [getErgebnisEintrag s Nothing]) studenten
  in sequence $ einsendungen' ++ keineEinsendungen

getErgebnisEintrag :: Entity Student -> Maybe Einsendung.Stud_Aufg -> Handler ErgebnisEintrag
getErgebnisEintrag student meinsendung = do
  let fromOks oks = let T.Oks i = oks in i
      fromNos nos = let T.Nos i = nos in i
  form' <- generateFormPost $ neuBewertenForm $ keyToInt $ entityKey student
  return ErgebnisEintrag {
    studentId = entityKey student,
    matrikel = studentMatrikelNummer $ entityVal student,
    vorname = studentVorname $ entityVal student,
    nachname = studentName $ entityVal student,
    okays = fmap (fromOks . Einsendung.ok) meinsendung,
    neins = fmap (fromNos . Einsendung.no) meinsendung,
    mergebnis = join $ fmap Einsendung.result meinsendung,
    form = form'
  }

optionen :: [(AutotoolMessage, Maybe T.Wert)]
optionen = [(MsgBehalten, Nothing), (MsgNein, Just T.No)] ++ fmap (\ i -> (MsgTextToMsg (pack $ show i), Just $ T.Ok i)) [1..5]

neuBewertenForm :: Int -> Form (Maybe T.Wert)
neuBewertenForm student = identifyForm (neuBewertenLabel student) $ renderRaw $
  areq (tdRadioFieldNoLabel (neuBewertenLabel student) . optionsPairs $ optionen) "" $ Just Nothing

renderRaw :: Monad m => FormRender m a
renderRaw aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
  ^{fvInput view}
|]
    return (res, widget)

renderTableHead :: OptionList a -> WidgetT site IO ()
renderTableHead options = [whamlet|
$newline never
$forall option <- olOptions options
  <th .text-center>
    #{optionDisplay option}
|]

-- | Das Auswahlfeld wird als Tabelle gerendert.
tdRadioField :: (Eq a, RenderMessage site FormMessage)
             => HandlerT site IO (OptionList a)
             -> Field (HandlerT site IO) a
tdRadioField options = selectFieldHelper
    (\theId _name attrs inside -> do
      options' <- handlerToWidget options
      [whamlet|
$newline never
<table .table ##{theId} *{attrs}>
  <thead>^{renderTableHead options'}
  <tbody>^{inside}
|])
    (\theId name isSel -> [whamlet|
$newline never
<td .text-center>
    <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
|])
    (\theId name attrs value isSel _text -> [whamlet|
$newline never
<td .text-center>
    <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}>
|]) options

-- | Es werden so viele Tabellenzellen erzeugt, wie es Auswahlmöglichkeiten gibt. Für eventuell benötigte Tabellenüberschriften und umliegende Tabellenzeilen, sowie die Tabelle selbst muss man sich selbst kümmern (ggf. @renderTableHead@ verwenden)
tdRadioFieldNoLabel :: (Eq a, RenderMessage site FormMessage)
                    => Text -> HandlerT site IO (OptionList a)
                    -> Field (HandlerT site IO) a
tdRadioFieldNoLabel name = selectFieldHelper
    (\_theId _name _attrs inside -> [whamlet|
$newline never
^{inside}
|])
    (\theId _name isSel -> [whamlet|
$newline never
<td .text-center>
    <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked>
|])
    (\theId _name attrs value isSel _text -> [whamlet|
$newline never
<td .text-center>
    <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs}>
|])

{- |
kopiert von 'http://hackage.haskell.org/package/yesod-form-1.4.4/docs/src/Yesod-Form-Fields.html#selectFieldHelper'

LICENSE for this method:
Copyright (c) 2012 Michael Snoyman, http://www.yesodweb.com/

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}
selectFieldHelper
        :: (Eq a, RenderMessage site FormMessage)
        => (Text -> Text -> [(Text, Text)] -> WidgetT site IO () -> WidgetT site IO ())
        -> (Text -> Text -> Bool -> WidgetT site IO ())
        -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> WidgetT site IO ())
        -> HandlerT site IO (OptionList a)
        -> Field (HandlerT site IO) a
selectFieldHelper outside onOpt inside opts' = Field
    { fieldParse = \x _ -> do
        opts <- opts'
        return $ selectParser opts x
    , fieldView = \theId name attrs val isReq -> do
        opts <- olOptions <$> handlerToWidget opts'
        outside theId name attrs $ do
            unless isReq $ onOpt theId name $ render opts val `notElem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                ((if isReq then (("required", "required"):) else id) attrs)
                (optionExternalValue opt)
                (render opts val == optionExternalValue opt)
                (optionDisplay opt)
    , fieldEnctype = UrlEncoded
    }
  where
    render _ (Left _) = ""
    render opts (Right a) = maybe "" optionExternalValue $ listToMaybe $ filter ((== a) . optionInternalValue) opts
    selectParser _ [] = Right Nothing
    selectParser opts (s:_) = case s of
            "" -> Right Nothing
            "none" -> Right Nothing
            x -> case olReadExternal opts x of
                    Nothing -> Left $ SomeMessage $ MsgInvalidEntry x
                    Just y -> Right $ Just y
