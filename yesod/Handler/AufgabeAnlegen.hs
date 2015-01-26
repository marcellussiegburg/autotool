module Handler.AufgabeAnlegen where

import Import

import Data.List (head, tail)
import Data.Tuple6

import Handler.Aufgabe.Forms
import Handler.AufgabeEinstellungen (AufgabeFormDaten)

data AutotoolForm = AutotoolForm {
  titel :: AutotoolMessage,
  name :: Text,
  enctype :: Enctype,
  route :: Route Autotool,
  widget :: Widget,
  attributes :: [(Text, Text)]
}

getAufgabeAnlegenR :: VorlesungId -> Handler Html
getAufgabeAnlegenR = postAufgabeAnlegenR

postAufgabeAnlegenR :: VorlesungId -> Handler Html
postAufgabeAnlegenR vorlesung = do
  aufgabeTemplate (Left vorlesung) Nothing

aufgabeTemplate :: Either VorlesungId AufgabeId -> Maybe (Maybe ServerUrl, Maybe AufgabeTyp, Maybe AufgabeFormDaten, Maybe (Maybe VorlageName), Maybe AufgabeKonfiguration) -> Handler Html
aufgabeTemplate eid maufgabe = do
  let ziel = case eid of
               Left gruppe -> AufgabeAnlegenR gruppe
               Right aufgabe -> AufgabeBearbeitenR aufgabe
      maufgabenstellung = Nothing :: Maybe Text
      mhinweis = Nothing :: Maybe Text
      mbewertung = Nothing :: Maybe Text
      atyp = preEscapedToHtml ("<td><a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Raw.html#t:Term\"><tt>Term</tt></a>(<a href=\"http://hackage.haskell.org/package/ghc-prim/docs/GHC-Tuple.html#t:()\")><tt>()</tt></a>)(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>)</td>" :: Text)
      ktyp = preEscapedToHtml ("[(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autotool-collection/Baum-Order.html#t:Order\"><tt>Order</tt></a>, [<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>])]" :: Text)
      aufgabenTypen =
       [Node "Terme, Ersetzungssysteme"
        [Node "Unifikation"
         [Node "Unify-Direct-1" []]
        ,Node "Termersetzung"
         [Node "Derive-For_TRS-Direct-1" []
         ,Node "Termination"
          [Node "Matrix-Interpretationen"
           [Node "Rewriting_Termination-Direct" []]
          ,Node "Polynom-Interpretationen"
           [Node "Rewriting_Termination-Direct-1" []]]
         ,Node "Completion-Direct-1" []]]
       ,Node "Mengen und Relationen"
        [Node "Algebraic_Set-Direct-1" []]] :: Forest Text
      vorlagen = ["ctnfa", "alextest", "NFA: ab", "DFA_vollst: ab", "NFA: =<3b", "NFA: |w|>3", "NFA: 3 teilt |w|_a", "NFA-ohne-aba", "NFA-mod3/2", "NFA: mod", "NFA: wx", "NFA:wx2", "NFA: |w|_a=|w|_b mod 3", "EXP2MinNFA", "EXP2MinNFAhelp", "Aufgabe 1.1", "Aufgabe 1.2.a", "Aufgabe 1.2.b", "Aufgabe 2.1.a", "Aufgabe 2.3.a", "Aufgabe 2.3.b", "Aufgabe 2.6", "Aufgabe 2.1.c", "Aufgabe 2.1.b", "NFA ohne ba*b", "Aufgabe 3.3.b", "Aufgabe 3.5", "Aufgabe 4.2.a", "Aufgabe 4.2.b", "Aufgabe 4.5", "Aufgabe 4.6", "2", "NFA:demo", "NFA:ab+ba", "NFA: ohne bab", "NFA: |w|_a=|w|_b mod 3", "NFA: wx", "NFA: =<3b", "NFA:ab...nichtdet", "NFA: ab...det", "RA2MinDFA", "NFA: wx2", "FA-nicht3", "NFA-Ostern", "DFA-Ostern", "NFA-u0g1", "NFA-aibjckdla7", "NFA-Xa7X", "NFA-suffixababcd", "DFA-suffixababcd", "DFA-csuffixababcd", "NFAtoDFA1", "NFA-prefixbc-infixaca", "DFA-prefixbc-infixaca", "NFAa++", "DFA-Suffix-bb", "Minimal-DFA-1", "EA_Anzahl_ab", "NFA-Xa4Xb4", "DEA_Auto_matte", "EA_Auto_matte", "NFA-a4bicjdka4", "NFA-suffixbcbcdab", "DFA-suffixbcbcdab", "NFA-praefixabc-infixabd", "DFA-praefixabc-infixabd", "NFA-nicht3", "NFA1", "NFA2", "NFA3", "NFA-Bin3", "NFA4", "NFA5", "minDEA_Ende10", "MinimDEA1", "MinimDEA2", "NFA1", "DFA1", "DFA2", "NFA6", "MinimDEA3", "DFA-Mini1", "DFA-Mini2", "DFA4", "RE -> NFA", "GR -> NFA", "NFA-Rechtslinear", "NFA-Ostern", "DFA-Ostern", "EA_Anzahl_ab", "NFA-Infix-ab", "NFA: |w|>3", "NFA-HS: wx", "NFA-HS:ohne-Infix", "NFA:Postfix", "NFA:auto", "NFA-DFA2", "DFA-ostern", "DFA-HS1", "HA3.3", "HA3.6b", "HA3.6e", "NFA-suffixbcbcdab", "NFA-nicht3", "Potenzmengenk", "HA5.3.a", "HA5.3.b", "A01 DFA ohne bab", "A02 DFA ab+ba", "RLG->NFA", "RE->NFA", "HA6.3a", "HA6.6a", "A05 wxy minimieren", "A05z wxy als NEA", "A06 eNEA -> NEA", "A07 Produktautomat", "A18 regG -> minDEA-118", "A00a DFA w01 ", "A00b NFA awc", "A01 DFA ohne bab", "A02 NFA ubv2c0c", "A04 wxy minimieren", "A04z wxy als NEA", "A05 eNEA -> NEA", "A06 Produktautomat", "EA_Praefix_Infix", "EA_auto_matte", "Potenzmengenk", "NFA1", "DFA1", "DFA2", "NFA3", "NFA4", "NFA5", "NFA6", "DFA6", "MinAut_Aufg.3.4b", "DFA10", "Typ3-GR->NFA (korrigiert)", "Typ3-Gr->NFA2", "Convert_To_NFA-Direct-142", "ttt", "NFA: ab", "less_than_4_b", "NFA: <4b", "DFA: endet_auf_bb", "NFA: rest=2", "NFA: wx", "NFA:ab", "student_enten", "NFA-Demo", "NFA1", "NFA2", "NFA-HS1", "DFA-ostern", "NFA4", "NFA-HS2", "NFA5", "Eps-NFA1", "EA_Auto_matte", "DEA_Auto_matte", "Ostern", "Potenzmengenk", "NFA-Demo", "NFA1", "NFA2", "NFA2a", "NFA2b", "DFA1", "DFA6.1.a", "NFA6.4", "DFA6.4", "MDFA7.4", "NFA3", "DFA2", "regAusdruck->EA", "GR->NFA", "NFA-Demo", "NFA1", "NFA2", "NFA3", "16.1-DFA-B", "NFA5  (statt DFA4)", "DFA1 (korrigiert)", "EA_auto_matte", "NFA-Ostern", "DFA-Ostern"] :: [Text]
  ((serverResult, _), _) <- runFormPost $ serverForm Nothing
  ((typResult, _), _) <- runFormPost $ typForm aufgabenTypen "" Nothing
  ((einstellungenResult, _), _) <- runFormPost $ aufgabeForm "" "" Nothing
  ((vorlageResult, _), _) <- runFormPost $ vorlagenForm vorlagen "" "" undefined Nothing
  ((konfigurationResult, _), _) <- runFormPost $ konfigurationForm (undefined :: Text) eid "" "" undefined undefined Nothing
  ((hochladenResult, _), _) <- runFormPost $ hochladenForm "" "" undefined undefined undefined
  ((testenResult, _), _) <- runFormPost $ testenForm (undefined :: Text) "" "" undefined undefined "" Nothing

  hochladen <- auswerten hochladenResult Tuple6_6 [ServerForm, AufgabeTypForm, AufgabeForm, VorlagenForm, KonfigurationForm]
  eingaben <- do
      a <- auswertenTyped serverResult Tuple6_1 []
      b <- auswertenTyped typResult Tuple6_2 [ServerForm]
      c <- auswertenTyped einstellungenResult Tuple6_3 [ServerForm, AufgabeTypForm]
      d <- auswertenTyped vorlageResult Tuple6_4 [ServerForm, AufgabeTypForm, AufgabeForm]
      e <- auswertenTyped konfigurationResult Tuple6_5 [ServerForm, AufgabeTypForm, AufgabeForm, VorlagenForm]
      f <- auswerten testenResult Tuple6_6 [ServerForm, AufgabeTypForm, AufgabeForm, VorlagenForm, KonfigurationForm]
      return $ do {_ <- a; _ <- b; _ <- c; _ <- d; _ <- e; f}
  let (mserver', mtyp', meinstellungen', mvorlage', mkonfiguration', mtesten) = fromTuple6 $ either id id eingaben
      (mserver'', mtyp'', meinstellungen'', mvorlage'', mkonfiguration'', mhochladen) = fromTuple6 $ either id id hochladen
      (mserver, mtyp, meinstellungen, mvorlage, mkonfiguration) = case maufgabe of
        Just aufgabe -> aufgabe
        Nothing -> case mserver'' of
          Nothing -> (mserver', mtyp', meinstellungen', mvorlage', mkonfiguration')
          Just _ -> (mserver'', mtyp'', meinstellungen'', mvorlage'', mkonfiguration'')
  let getServerForm ms = getForm ServerForm ziel [] $ serverForm ms
      getTypForm s mt = getForm AufgabeTypForm ziel [] $ typForm aufgabenTypen s mt
      getAufgabeForm s t ma = getForm AufgabeForm ziel [] $ aufgabeForm s t ma
      getVorlagenForm s t a mv = getForm VorlagenForm ziel [("class", "form-inline")] $ vorlagenForm vorlagen s t a mv
      getKonfigurationForm s t a v mk = getForm KonfigurationForm ziel [] $ konfigurationForm ktyp eid s t a v mk
      getHochladenForm s t a v k = getForm HochladenForm ziel [] $ hochladenForm s t a v k
      getTestenForm s t a v k ml = getForm TestenForm ziel [] $ testenForm atyp s t a v k ml
  forms <- sequence $ concat $ fmap maybeToList
             [getServerForm <$> Just mserver,
              getTypForm <$> mserver <*> Just mtyp,
              getAufgabeForm <$> mserver <*> mtyp <*> Just meinstellungen,
              getVorlagenForm <$> mserver <*> mtyp <*> meinstellungen <*> Just mvorlage,
              getKonfigurationForm <$> mserver <*> mtyp <*> meinstellungen <*> mvorlage <*> Just mkonfiguration,
              getHochladenForm <$> mserver <*> mtyp <*> meinstellungen <*> mvorlage <*> mkonfiguration,
              getTestenForm <$> mserver <*> mtyp <*> meinstellungen <*> mvorlage <*> mkonfiguration <*> Just mtesten]
  defaultLayout $ do
    addStylesheet $ StaticR css_tree_css
    $(widgetFile "aufgabeAnlegen")

auswertenTyped ::
  (MonadHandler m, RenderMessage (HandlerSite m) FormMessage, a ~ ServerUrl, b ~ AufgabeTyp, c ~ AufgabeFormDaten,
   d ~ (Maybe VorlageName), e ~ AufgabeKonfiguration, f ~ Text) =>
  FormResult t -> (t -> Tuple6 a b c d e f) -> [AufgabeForm]
  -> m (Either (Tuple6 a b c d e f) (Tuple6 a b c d e f))
auswertenTyped = auswerten

auswerten ::
  (MonadHandler m, PathPiece a, PathPiece b, PathPiece c, PathPiece d, PathPiece e,
   RenderMessage (HandlerSite m) FormMessage) =>
  FormResult t -> (t -> Tuple6 a b c d e f) -> [AufgabeForm]
  -> m (Either (Tuple6 a b c d e f) (Tuple6 a b c d e f))
auswerten result t forms =
  case result of
    FormSuccess s -> return $ Left $ t s
    FormFailure _ -> do v <- parameterAbrufen forms
                        return $ Left v
    FormMissing -> return $ Right Tuple6_0

parameterAbrufen :: (MonadHandler m, PathPiece a, PathPiece b, PathPiece c, PathPiece d,
            PathPiece e, RenderMessage (HandlerSite m) FormMessage) =>
           [AufgabeForm] -> m (Tuple6 a b c d e f)
parameterAbrufen forms = do
    let (form1, form2, form3, form4, form5, _) = fromTuple6 $ listToTuple6 forms
        fv = (\ f -> runInputPost $ ireq hiddenField (getId f))
    form1' <- mapM fv form1
    form2' <- mapM fv form2
    form3' <- mapM fv form3
    form4' <- mapM fv form4
    form5' <- mapM fv form5
    return $ toTuple6 (form1', form2', form3', form4', form5', Nothing)

getForm :: AufgabeForm -> Route Autotool -> [(Text, Text)] -> Form a -> Handler AutotoolForm
getForm art ziel attrs form = do
  ((_, widget'), enctype') <- runFormPost form
  return $ AutotoolForm (getTitel art) (getId art) enctype' ziel widget' attrs

tabWidget :: [AutotoolForm] -> [(Text, AutotoolMessage)] -> WidgetT Autotool IO ()
tabWidget forms names = do
  let nameUndTitel ::  AutotoolForm -> (Text, AutotoolMessage)
      nameUndTitel a = (name a, titel a)
  [whamlet|
  $if null forms
    <ul .nav .nav-tabs>
      $forall name' <- names
        <li>
          <a href=##{fst name'}>_{snd name'}
  $else
    $with form <- head forms
      <div ##{name form}>
        ^{tabWidget (tail forms) $ nameUndTitel form : names}
        <div .content>
          <h2>_{titel form}
          <form role="form" action=@{route form} method="post" enctype=#{enctype form} *{attributes form}>
            ^{widget form}
  |]
