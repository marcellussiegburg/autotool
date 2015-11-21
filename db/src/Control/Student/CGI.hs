module Control.Student.CGI where

import Control.Types
import Gateway.CGI
import Operate.Crypt
import Control.Schule as U
import Control.Student.Type as T
import Control.Student.DB
import qualified Control.Schule

import Debug ( debug )

import Control.Monad
import Data.List ( partition, isSuffixOf, isPrefixOf )
import Data.Char ( isAlphaNum, toUpper )
import Data.Maybe ( isNothing , isJust , fromJust )

import Autolib.Util.Zufall
import qualified Debug
import qualified Local

import qualified Autolib.Multilingual as M
import qualified Autolib.Multilingual.Html as H
import qualified Autolib.Output as O

{- | Benutzeridentifikation:

alt: über Schule und Matrikelnummer, die der Benutzer
     selbst eintippt.

zwischen: über Schule (vorgegeben) und Matrikelnummer (vom Shibboleth-IdP) 

          Probleme dabei:
          * nicht jeder hat eine Matrikelnr (z.B. Mitarbeiter haben keine)
          * Matrikelnummern ändern sich im Studentenleben (Wechsel von Bachelor zu Master)
          
neu: über EduPersonPrincipalName , 
     das sollte global eindeutig sein, 
     OPAL verwendet das aus diesem Grund auch.
     
     Probleme dabei: 
     * gleitender Wechsel vom alten System (für HTWK-Studenten)
     * "alt" soll für Externe weiterhin möglich sein (?)

-}


login :: Maybe Schule -> Form IO Student
login mschool = do
    lang <- get_preferred_language 

    -- click    <- submit    "Login:"

    open btable
    
    u <- case mschool of
      Just u -> do 
        -- open row ; plain "Schule" ; plain $ toString $ U.name u ; close -- row
        return u
      Nothing -> do
        us <- io $ Control.Schule.get 
        click_choice "Schule" $ do
            u <- us
	    return ( toString $ Control.Schule.name u , u )
        
    if U.use_shibboleth u 
       then login_via_shibboleth_eppn u
       else login_via_stored_password u

login_via_stored_password u = do
    lang <- get_preferred_language 

    h3 "Login"
    mnr <- defaulted_textfield "Matrikel" ""
    pwd <- defaulted_password  "Passwort" ""

    change <- click_choice "Aktion"
           [ ("Login", False)
           , (   M.specialize lang
               $ M.make [(M.DE, "persönliche Daten ändern")
                        ,(M.UK, "update account information")
                        ]
             , True) 
           ]

    close -- btable

    studs <- io $ Control.Student.DB.get_unr_mnr 
		        ( U.unr u , fromCGI mnr )
    -- close -- row

    stud <- case studs of
         [ stud ] ->
             if Operate.Crypt.compare ( passwort stud ) pwd
                then use_first_passwort stud
                else if Operate.Crypt.compare ( next_passwort stud ) pwd
                then use_next_passwort stud
                else wrong_password stud

         [ ] -> do
             plain $ M.specialize lang
                   $ M.make [ (M.DE,"Account existiert nicht.")
                            , (M.UK,"no such account")
                            ]
             mzero

         xs -> do
             plain "Mehrere Studenten mit dieser Matrikelnummer?"
             plain $ show $ map T.snr xs
             mzero

    when change $ do
        Control.Student.CGI.edit stud
    return stud

-- | spec: cut_at c w = xss  <=>  intercalate [c] xss == w
cut_at :: Eq a => a -> [a] -> [[a]]
cut_at c w = 
      let (pre, post) = span (/= c) w
      in  pre : if null post then [] else cut_at c $ tail post

analyze_puc = do
    mpuc <- look_var "puc"
    -- "urn:mace:terena.org:schac:personalUniqueCode:de:htwk-leipzig.de:Matrikelnummer:"
    case mpuc of
      Nothing -> do 
        return Nothing
      Just puc -> do
        let pucs = cut_at ':' puc
        case pucs of
          [ "urn", "mace", "terena.org", "schac", "personalUniqueCode", "de"
            , school, "Matrikelnummer", mat ] -> return $ Just (school, mat)
          _ -> return Nothing                                          

login_via_shibboleth_mnr u = do
  res <- analyze_puc
  case res of
    Nothing -> do 
      plain "missing or malformed PersonalUniqueCode"
      mzero
    Just (school,mat) -> do
      login_via_shibboleth_cont u school mat

-- | das soll eigentlich der DB-Schlüssel sein: 
-- edu-person-principal-name
login_via_shibboleth_eppn u = do
  show_session_info
  meppn <- look_var "eppn"
  case meppn of
    Nothing -> do
      plain "missing shibboleth attribute: eduPersonPrincipalName"
      mzero
    Just eppn -> do
      when (not $ isSuffixOf eppn $ toString $ U.mail_suffix u) $ do
        plain "eduPersonPrincipalName does not end with required suffix"
        mzero
      close -- btable
      Just sn <- look_var "sn" ; Just gn <- look_var "givenName"
      mpuc <- analyze_puc
      let mnr = case mpuc of Nothing -> "?" ; Just (school,mnr) -> mnr
      use_or_make_account (U.unr u) (fromCGI sn) (fromCGI gn) (fromCGI mnr) eppn
      
login_via_shibboleth_cont u school mnr = do
    when (not $ isSuffixOf school $ toString $ U.mail_suffix u) $ do
        plain "puc school attribute and mail_suffix differ"
        plain $ show (school, U.mail_suffix u)
        mzero
    Just sn <- look_var "sn" ; Just gn <- look_var "givenName"
    Just eppn <- look_var "eppn"
    when (null mnr) $ do
        maff <- look_var "affiliation"
        case maff of
          Nothing -> do 
            plain "missing affiliation attribute"
            mzero
          Just aff -> do
            let parts = cut_at ';' aff
            if any ( \ part -> isPrefixOf "staff@" part) parts
               then return ()
               else do
                 plain "no Matrikelnummer and no staff@"
                 mzero
                 
    show_session_info
    close -- btable    
    use_or_make_account (U.unr u) (fromCGI sn) (fromCGI gn) (fromCGI mnr) eppn

show_session_info = do
    open table 
    open row 
    plain $ "Ihre Shibboleth-Session:"  
    html $ M.specialize M.DE $ ( O.render $ O.Link "/Shibboleth.sso/Session" :: H.Html )
    close -- row
    close -- table
  

-- | Account in DB suchen oder einfügen, 
-- in jedem Fall den Account zurückgeben.
-- das wird nur bei shibboleth-auth aufgerufen, 
-- also haben wir eppn
use_or_make_account unr sn gn mnr eppn = do
    let inputs = unwords 
         [ show  unr, show sn, show gn, show mnr, show eppn ]
    studs <- io $ Control.Student.DB.get_unr_sn_gn_mnr_meppn
             ( unr , sn, gn, mnr, Just eppn )
    studs <- 
        if (null studs) then do
             let msg = "Account existiert nicht => wird angelegt: " ++ inputs
             plain msg
             io $ debug msg
             let stud = Student { T.snr = error "noch nicht"
                                     , T.unr = unr
                                     , T.mnr =  mnr
                                     , T.name =  sn
                                     , T.vorname =  gn
                                     , T.email = fromCGI eppn
                                     , T.passwort = Crypt "use shibboleth"
                                     , T.next_passwort = Crypt "use shibboleth"
                                     }
             io $ Control.Student.DB.put Nothing stud 
             io $ Control.Student.DB.get_unr_sn_gn_mnr_meppn ( unr , sn, gn, mnr, Just eppn )
        else return studs     
             
    stud <- case studs of
         [ stud ] -> return stud
         [ ] -> do
             plain "kein Account (Anlegen fehlgeschlagen)"
             mzero
         xs -> do
           let msg = unwords 
                 [ "Mehrere Accounts mit diesen Merkmalen"
                 , inputs, show $ map T.snr xs
                 ] 
           plain msg ; io $ debug msg
           mzero

    if (T.email stud == fromCGI "use shibboleth"  ) 
       then do
         let msg = "EPPN wird erstmalig zugeordnet " ++ inputs
         io $ debug msg

         let stud' = stud { T.email = fromCGI eppn }
         io $ Control.Student.DB.put (Just $ T.snr stud) stud'
         return stud'
       else return stud 

use_first_passwort stud = 
    if ( Operate.Crypt.is_empty $ next_passwort stud ) 
    then return stud -- ändert sich nichts
    else do
        lang <- get_preferred_language 
        plain $ M.specialize lang
              $ M.make 
              [ (M.DE, unlines [ "Sie hatten eine Email mit einem neuen Passwort erhalten,"
                               , "aber Sie haben jetzt Ihr altes Passwort benutzt."
                               , "Das Passwort aus der Email wird dadurch ungültig,"
                               , "Ihr bestehendes (jetzt benutztes) Passwort bleibt gültig."
                               ] )
              , (M.UK, unlines [ "You have received an email with a fresh password"
                               , "but you used your previous password."
                               , "Now the password from the email is invalid"
                               , "and your previous password (that you just used) remains valid."
                               ] )
              ]
        let neu = stud { T.next_passwort = Operate.Crypt.empty }
        io $ Control.Student.DB.put ( Just $ T.snr stud ) neu
        return neu

use_next_passwort alt = do
    lang <- get_preferred_language 
    plain $ M.specialize lang
          $ M.make [ (M.DE, "Sie haben Ihr neues Passwort verwendet.")
                   , (M.UK, "You used your new password.")
                   ]
    let neu = alt { T.passwort = T.next_passwort alt
                  , T.next_passwort = Operate.Crypt.empty
                  }
    io $ Control.Student.DB.put ( Just $ T.snr alt ) neu
    plain $ M.specialize lang
          $ M.make [(M.DE, "Das vorherige ist damit ungültig.")
                   ,(M.UK, "Your previous password is now invalid.")
                   ]
    return neu

wrong_password stud = do
    lang <- get_preferred_language 
    plain $ M.specialize lang
          $ M.make [ (M.DE, "Passwort falsch."), (M.UK, "wrong password")]
    par 
    if ( Operate.Crypt.is_empty $ next_passwort stud )
       then ask_pwmail stud
       else plain $ M.specialize lang
                  $ M.make [ (M.DE, unlines
                                    [ "Sie haben eine Email mit einem neuen Passwort erhalten,"
                                    , "aber Sie haben dieses neue Passwort noch nicht benutzt."
                                    ])
                           , (M.UK, unlines
                                    [ "You received an email with a fresh password"
                                    , "but you never used it."
                                    ]) 
                           ]
    mzero

-----------------------------------------------------------------------

edit :: Student -> Form IO ()
edit s = do
    edit_create Nothing $ Just s
    return ()

is_a_word :: Monad m => String -> String -> Form m ()
is_a_word label cs = do
    when ( null cs ) $ complain [ label , "leere Eingabe ist nicht erlaubt." ]
    let ( good, bad ) = partition isAlphaNum cs
    when ( not $ null bad ) $ do
	 complain [ label , "diese Zeichen sind nicht erlaubt:", show bad ]

is_an_email :: Monad m => String -> String -> Form m ()
is_an_email label cs = do
    when ( null cs ) $ do
         complain [ label , "leere Eingabe ist nicht erlaubt." ]
    let ( ats, rest ) = partition ( == '@' ) cs
    when ( 1 /= length ats ) $ do
	complain [ label , "Adresse soll genau ein '@' enthalten" ]
    let ok c = isAlphaNum c || c `elem` ".-_"
    let ( good, bad ) = partition ok rest
    when ( not $ null bad ) $ do
	 complain [ label , "diese Zeichen sind nicht erlaubt:", show bad ]

is_an_email_for_school :: Monad m => U.Schule -> String -> String -> Form m ()
is_an_email_for_school u label cs = do
    is_an_email label cs
    let suf = toString $ U.mail_suffix u
    when ( not $ isSuffixOf suf cs ) $ do
        complain [ label, "es sind nur Mailadressen gestattet, die auf"
                 , suf, "enden."
                 ]

-----------------------------------------------------------------------

complain css = do
    open row
    sequence_ $ do
        cs <- css
	return $ do
	    plain cs
    close -- row
    open row
    blank
    submit "submit"
    close -- row
    mzero    

-- | falls 'Just Student', dann editieren
-- falls 'Nothing', dann anlegen
edit_create :: Maybe Schule -> Maybe Student 
            -> Form IO ()
edit_create (Just u) ms = do
    edit_create_continue u ms

edit_create Nothing ms = do
    
    us <- io $ Control.Schule.get 

    u <- case ms of
        -- Student darf Schule nicht ändern
        Just s -> return $ head $ do
            u <- us
            guard $ U.unr u == T.unr s 
            return u
        Nothing -> click_choice "Schule" $ do
            u <- us
	    return ( toString $ Control.Schule.name u , u )

    edit_create_continue u ms


edit_create_continue u ms = do
    lang <- get_preferred_language 

    open btable

    plain $ toString $ U.name u

    let dtf label select = 
           defaulted_textfield label $ case ms of
                Just s -> toString $ select s ; Nothing -> ""

    mnr <- dtf "Matrikelnummer" T.mnr
    vorname <- dtf "Vorname" T.vorname
    name <- dtf "Nachname" T.name
    email <- dtf "Email" T.email 

    when (isJust ms) (do open row
                         plain "Interne Kennung"
                         plain $ toString $ T.snr $ fromJust ms
		         close -- row
                      )

    is_a_word "Matrikelnummer" mnr    
    is_a_word "Vorname" vorname
    is_a_word "Nachname" name
    is_an_email_for_school u "Email" email

    schon <- io $ get_unr_mnr ( U.unr u , fromCGI mnr )
    let others = case ms of
            Just s -> filter ( \ s' -> T.snr s' /= T.snr s ) schon
            Nothing -> schon
    when ( not $ null others ) $ do
        open row
	plain "diese Matrikelnummer ist bereits in Benutzung"
        close
	mzero

    let stud0 = case ms of
          Just s -> s
          Nothing -> T.Student -- cannot log in
                     { T.passwort = Operate.Crypt.empty
                     , T.next_passwort = Operate.Crypt.empty
                     } 
    let stud = stud0
             { T.mnr = fromCGI mnr
             , T.unr = case ms of
                     Just s -> T.unr s
                     Nothing -> U.unr u
	     , T.vorname = fromCGI vorname
	     , T.name = fromCGI name
	     , T.email = fromCGI email
             -- NOTE: passwords are not set here
             }

    -- password handling
    case ms of
        Nothing -> do -- neuer Account: passwort würfeln und mailen,
            open row
            submit "Account ..."
            click <- submit "... create?"
            close -- row
            close -- table
            when click $ do
                io $ Control.Student.DB.put Nothing stud
                [ stud ] <- io $ Control.Student.DB.get_unr_mnr 
                            ( T.unr stud , T.mnr stud )
                pwmail $ stud
            mzero -- never returns

        Just s -> do -- bestehender Account: passwort ändern 
           pw <- defaulted_textfield "password" ""
           c <- if null pw 
	        then do 
                    plain "(keine Eingabe: Passwort wird nicht geändert)"
                    return $ T.passwort s
	        else do
	            is_a_word "Passwort" pw
	            io $ encrypt pw
           open row
           up <- submit "update"
           close -- row
           when up $ do
                io $ Control.Student.DB.put ( Just $ T.snr s )
                   $ stud { T.passwort = c
                          , T.next_passwort = Operate.Crypt.empty
                          }
                plain "update done"

    close -- btable

-------------------------------------------------------------------------

-- | generate, display and encode "random" passwort
generator s = do
    mc <- generator0 False
    case mc of
        Nothing -> return ()
        Just c  -> 
           io $ Control.Student.DB.put (Just $ T.snr s)
	      $ s { T.passwort = c }

generator0 must = do
    open row
    gen <- if must 
           then do submit "new password:" ; return True
           else do submit "generate new password?"
    if gen 
       then do
           p <- io $ pass
           pre p -- raw form anzeigen
           c <- io $ encrypt p
           close -- row
           return $ Just c
       else do
           close -- row
           return Nothing

-- | ask if user wants new password,
-- if yes, then generate and mail,
-- then stop
ask_pwmail stud = do
    lang <- get_preferred_language 
    open row
    plain $ M.specialize lang
          $ M.make [(M.DE, unlines
                           [ "Ein neues Passwort erzeugen und per email zustellen?"
                           -- , toString $ email stud
                           ])
                   ,(M.UK, unlines
                           [ "generate new password and send to known email address?"
                           ])
                   ]
    click <- submit $ M.specialize lang $ M.make [(M.DE, "Ja."), (M.UK, "Yes")]
    close -- row
    when click $ pwmail stud
    mzero

-- | generate new password,
-- put ciphertext in db
-- send plaintext to known email address
-- be very very careful not to allow shell code injection
pwmail stud = do
    lang <- get_preferred_language 
    let e = toString $ email stud
    is_an_email "Email" e
    let m = toString $ mnr stud
    is_a_word "Matrikel" m    

    p <- io $ pass
    is_a_word "Passwort" p    
    c <- io $ encrypt p
    io $ Control.Student.DB.put ( Just $ T.snr stud )
       $ stud { T.next_passwort = c }

    let echo = texter $ M.specialize lang $ M.make
          [(M.DE, 
            [ "Sie haben ein neues Passwort"
            -- , "für das E-Learning-System autotool angefordert."
            , "fuer das E-Learning-System autotool angefordert."
            , unwords [ "Es lautet:", "Matrikelnummer:", m, "Passwort:", p ]
            , "Es wird durch seine erste Benutzung aktiviert,"
            -- , "Sie können es danach ändern."
            , "Sie koennen es danach aendern."
            -- , "Sie können aber auch Ihr bisheriges Passwort weiter benutzen"
            , "Sie koennen aber auch Ihr bisheriges Passwort weiter benutzen"
            , "und diese Mail ignorieren."
            ])
          ,(M.UK,
            [ "You, or someone acting as you, have requested a fresh password"
            , "for the autotool E-Learning system."
            , unwords [ "account:", m, "password:", p ]
            , "It will be activated by its first use."
            , "You can change it afterwards."
            , "Alternatively, if you already had a password, you may continue to use it"
            , "and ignore this email."
            ])
          ]

    let cmd = unwords
           [ echo
           , "|"
           , "/usr/bin/mail"
           , "-s", show $ M.specialize lang $ M.make [ (M.DE, "neues autotool-passwort"), (M.UK, "new autotool password")]
           , "-a", show "From: autotool"
           , e
           , ""
           ]           
    io $ Debug.debug $ "running: " ++ cmd
    
    res <- io $ Debug.system cmd
    io $ appendFile "/tmp/mail.log" cmd
    
    io $ Debug.debug $ "Exit code: " ++ show res

    pre $ M.specialize lang $ M.make
        [ (M.DE, unwords
                [ "Ein neues Passwort wurde an Ihre Mail-Adresse"
                -- , e
                , "gesendet."
                ])
        , (M.DE, unwords
                [ "A new password has been sent to your email address."
                -- , e
                ])
        ]
    return ()

-- | writes text using echo shell commands (ugly ugly)
texter :: [ String ] -> String
texter lines = 
    let parens cs = "( " ++ cs ++ " )"
        handle line = " echo " ++ line ++ " ; "
    in  parens $ unlines $ map handle lines

----------------------------------------------------------------
 
vokale = "aeiouy"
konsonanten = "bcdfghjklmnpqrstvwxz"

drei = do
   k <- eins konsonanten
   v <- eins vokale
   j <- eins konsonanten
   return [k,v,j]


zwei = do
   k <- eins konsonanten
   v <- eins vokale
   return [k,v]

pass = do
   ws <- sequence [ drei, zwei, zwei, drei ]
   return $ concat ws


