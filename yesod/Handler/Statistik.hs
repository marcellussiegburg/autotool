module Handler.Statistik where

import Import
import Handler.Aufgabe (getDefaultParam)

import qualified Control.Aufgabe.Typ as Aufgabe
import qualified Control.Aufgabe.DB as AufgabeDB
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

data Aktion = Bearbeiten | CacheLeeren Text deriving (Show, Read)

getStatistikR :: AufgabeId -> Handler Html
getStatistikR = postStatistikR

postStatistikR :: AufgabeId -> Handler Html
postStatistikR aufgabeId = do
  maufgabe <- liftIO $ liftM listToMaybe $ AufgabeDB.get_this $ T.ANr aufgabeId
  aufgabe <- case maufgabe of
    Nothing -> do -- sollte nie passieren
      setMessageI MsgFehler
      redirect SchulenR
    Just a -> return a
  studenten <- liftIO $ VorlesungDB.steilnehmer $ Aufgabe.vnr aufgabe
  _ <- runMaybeT $ do
    maktion <- lookupPostParam aktion
    aktion' <- MaybeT . return $ maktion
    lift $ case read $ unpack aktion' of
      Bearbeiten -> bewertungenSchreiben studenten aufgabe
      CacheLeeren m ->
        let T.VNr vorlesungId = Aufgabe.vnr aufgabe
        in cacheLeeren m vorlesungId aufgabeId
  einsendungen <- liftIO $ EinsendungDB.get_anr $ T.ANr aufgabeId
  ergebnisse <- getErgebnisListe einsendungen studenten
  optionen' <- optionsPairs optionen
  defaultLayout $ do
    $(widgetFile "statistik")

cacheLeeren :: Text -> VorlesungId -> AufgabeId -> Handler ()
cacheLeeren matrikel' vorlesungId aufgabeId = do
  let d =  D.Datei {
      D.pfad = [ "autotool", "cache"
               , show vorlesungId
               , show aufgabeId
               ]
    , D.name = unpack $ matrikel'
    , D.extension = "cache"
    }
  lift $ D.loeschen d `catch` \ (SomeException _) -> return ()
  setMessageI $ MsgCacheGeleert $ pack $ show d

bewertungenSchreiben :: [Student.Student] -> Aufgabe.Aufgabe -> Handler ()
bewertungenSchreiben studenten aufgabe = do
  let fromSNr snr = let T.SNr s = snr in s
      getResult s = liftM (fst . fst) $ runFormPost $ neuBewertenForm $ fromSNr $ Student.snr s
      erfolgreich (FormSuccess r, s) = Just (r, s)
      erfolgreich _ = Nothing
  results <- sequence $ map (\s -> do r <- getResult s
                                      return (r, s)) studenten
  let relevant = concat $ map (maybeToList . erfolgreich) results
  sequence_ $ map (\ (r, s) -> bewertungSchreiben s aufgabe r) relevant

bewertungSchreiben :: Student.Student -> Aufgabe.Aufgabe -> Maybe T.Wert -> Handler Text
bewertungSchreiben _ _ Nothing = return ""
bewertungSchreiben student aufgabe (Just bewertung) = do
  let message = "Bewertung durch Tutor" :: Text -- ^ TODO: Übersetzung?
  lift $ liftM pack $ bank (getDefaultParam student aufgabe) {
      P.report = Just $ preEscapedToHtml message,
      P.mresult = Just bewertung
    }

getErgebnisListe :: [Einsendung.Stud_Aufg] -> [Student.Student] -> Handler [ErgebnisEintrag]
getErgebnisListe einsendungen studenten =
  let einsender = fromList $ fmap Einsendung.snr einsendungen
      getStudent e = find (\ s -> Student.snr s == Einsendung.snr e) studenten
      getEintrag e =
        let s = getStudent e
        in maybeToList $ fmap (\ s' -> getErgebnisEintrag s' $ Just e) s
      einsendungen' = concat $ fmap getEintrag einsendungen
      keineEinsendungen = concat $ fmap (\s -> if Student.snr s `member` einsender then [] else [getErgebnisEintrag s Nothing]) studenten
  in sequence $ einsendungen' ++ keineEinsendungen

getErgebnisEintrag :: Student.Student -> Maybe Einsendung.Stud_Aufg -> Handler ErgebnisEintrag
getErgebnisEintrag student meinsendung = do
  let fromOks oks = let T.Oks i = oks in i
      fromNos nos = let T.Nos i = nos in i
      fromSNr snr = let T.SNr s = snr in s
  form' <- generateFormPost $ neuBewertenForm $ fromSNr $ Student.snr student
  return $ ErgebnisEintrag {
    matrikel = pack $ T.toString $ Student.mnr student,
    vorname = pack $ T.toString $ Student.vorname student,
    nachname = pack $ T.toString $ Student.name student,
    okays = fmap (fromOks . Einsendung.ok) meinsendung,
    neins = fmap (fromNos . Einsendung.no) meinsendung,
    mergebnis = join $ fmap Einsendung.result meinsendung,
    form = form'
  }

optionen :: [(AutotoolMessage, Maybe T.Wert)]
optionen = [(MsgBehalten, Nothing), (MsgNein, Just T.No), (MsgTextToMsg "1", Just $ T.Ok 1), (MsgTextToMsg "2", Just $ T.Ok 2), (MsgTextToMsg "3", Just $ T.Ok 3), (MsgTextToMsg "4", Just $ T.Ok 4), (MsgTextToMsg "5", Just $ T.Ok 5)]

neuBewertenForm :: Int -> Form (Maybe T.Wert)
neuBewertenForm student = identifyForm (pack $ "neuBewerten" ++ show student) $ renderRaw $
  areq (tdRadioFieldNoLabel . optionsPairs $ optionen) "" $ Just Nothing

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

renderTableHead :: OptionList a -> Widget
renderTableHead options =
  [whamlet|
$newline never
$forall option <- olOptions options
  <th .text-center>
    #{optionDisplay option}
|]

-- | Es werden so viele Tabellenzellen erzeugt, wie es Auswahlmöglichkeiten gibt. Für eventuell benötigte Tabellenüberschriften und umliegende Tabellenzeilen, sowie die Tabelle selbst muss man sich selbst kümmern (ggf. @renderTableHead@ verwenden)
tdRadioFieldNoLabel :: (Eq a, RenderMessage site FormMessage)
                    => HandlerT site IO (OptionList a)
                    -> Field (HandlerT site IO) a
tdRadioFieldNoLabel = selectFieldHelper
    (\_theId _name _attrs inside -> [whamlet|
$newline never
^{inside}
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
        opts <- fmap olOptions $ handlerToWidget opts'
        outside theId name attrs $ do
            unless isReq $ onOpt theId name $ not $ render opts val `elem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                ((if isReq then (("required", "required"):) else id) attrs)
                (optionExternalValue opt)
                ((render opts val) == optionExternalValue opt)
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
