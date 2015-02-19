-- | edit the problem configuration
-- and check a sample solution

-- TODO: hack this module into pieces

{-# LANGUAGE PatternSignatures, DeriveDataTypeable #-}

module Main where

import Prelude hiding ( readFile, writeFile, appendFile )
import System.IO ( readFile, writeFile, appendFile )

import Gateway.CGI

import Inter.Evaluate
import Operate.Make 
import Operate.Motd
import Operate.Bank
import Operate.Store 
import Operate.Login
import Operate.Logged
import qualified Operate.Param as P
import qualified Operate.Statistik

import Gateway.Help

import Autolib.Set
import qualified Autolib.Output

import qualified Autolib.Multilingual as M

import Control.Types 
    ( toString, fromCGI, Name, Typ , Remark, HiLo (..), Status (..)
    , Oks (..), Nos (..), Time , Wert (..), MNr, SNr, VNr, ANr, UNr
    , TimeStatus (..)
    )

import qualified Control.Types   



import Challenger.Partial
-- import Inter.Types
import Operate.Types
import Operate.Common

import qualified Default

import Control.Student.CGI
import Control.Vorlesung.DB
import qualified Control.Student.DB
import qualified Control.Punkt
import qualified Control.Stud_Aufg.DB

import qualified Control.Aufgabe as A
import qualified Control.Stud_Aufg as SA
import qualified Control.Student as S
import qualified Control.Vorlesung as V
import qualified Control.Gruppe as G
import qualified Control.Stud_Grp as SG
import qualified Control.Schule as U

import Autolib.Reporter.Type hiding ( wrap, initial )
import Autolib.ToDoc
import qualified Autolib.Output as O
import Autolib.Reader
import Autolib.Util.Sort
import Autolib.FiniteMap

import qualified Autolib.Multilingual as M
import qualified Autolib.Multilingual.Html as H

import qualified Util.Datei as D
import Debug

import System.Random
import qualified System.Directory
import Data.Typeable
import Data.Maybe
import Data.Tree
import Data.List ( partition )
import Data.Char ( isAlpha, toLower )
import Control.Monad

import qualified Control.Exception as CE

import qualified Gateway.Html as Html

import Operate.DateTime ( defaults )
import Operate.Tutor
import Operate.Student

import qualified Debug 


main :: IO ()
main = do
   Debug.debug "Super_Debug:main"
   let my_name = Default.super_cgi_name
   ( Gateway.CGI.execute ( my_name ) $ do
       
       -- vs <- get_vars ; plain $ show vs
       -- is <- get_env  ; plain $ show is
      
       wrap $ preface Default.server
       
       scores <- scores_link
       footer scores ) `CE.catch` \ ( e :: CE.SomeException ) -> do
         debug $ "caught: " ++ show e
         print e

preface server = do
    mschool <- look "school"
    school <- bestmatch $ Control.Types.Name $ case mschool of
        Nothing -> "HTWK Leipzig"
        Just school -> school
    btabled $ select_preferred_language_with_default $ U.preferred_language school
    iface (Just school) server

bestmatch name = do
    us <- io $ U.get
    let essence = map toLower . filter isAlpha . toString
        eq s t = essence s == essence t
    [ u ] <- return $ filter ( eq name . U.name ) us
    return u

-- iface :: Tree ( Either String Make ) -> Form IO ()
iface mschool server = do
    lang <- get_preferred_language   
    case mschool of
      Just u | U.use_shibboleth u -> use_account mschool server
      _ -> do

        new <- click_choice_with_default 0 "Aktion" 
            [ ( M.specialize lang $ M.make [(M.DE, "Account benutzen"),(M.UK, "use account")] , False ) 
            , ( M.specialize lang $ M.make [(M.DE, "Account anlegen") ,(M.UK, "create account")] , True )
            ]
        
        if new 
           then edit_create mschool Nothing
           else use_account mschool server

data Code = Stat | Auf | Einsch
   deriving ( Show, Eq, Typeable )


use_account mschool server = do
    lang <- get_preferred_language   

    h3 "Login"
    -- für Student und Tutor gleicher Start

    svt @ ( stud, vor, status0, attends0 ) <- Operate.Login.form mschool

    ( status, attends ) <- 
        if status0 == Tutor
        then do
            h3 "Sie sind Tutor für diese Vorlesung."
            open btable
            result <- click_choice_with_default 0 "Arbeiten als..."
                  [ ("Tutor", ( Tutor, attends0 ) ) 
                  , ("Student", ( Student, True ) )
                  ]
            close
            return result
        else return ( Student, attends0 )
    let tutor = ( status >= Tutor )

    open btable
    aktion <- click_choice_with_default 0 "Aktion" $
           [ (multi lang [(M.DE, "Aufgaben"),(M.UK, "Exercises")],  aufgaben server ( stud, V.vnr vor, tutor )  ) 
                 | attends || tutor ]
	++ [ (multi lang [(M.DE, "Einschreibung"),(M.UK, "Registration")], veranstaltungen ( stud, vor, tutor )  ) ]
        ++ [ (multi lang [(M.DE, "Statistiken"),(M.UK, "Statistics")],   Operate.Statistik.main svt  ) | tutor ]
        ++ [ (multi lang [(M.DE, "Waisenkinder"),(M.UK, "repair registration")], waisenkinder $ S.unr stud ) | tutor ]
    close -- btable 
    aktion

multi lang opts = M.specialize lang $ M.make opts

-- | Studenten behandeln, die in keiner Übungsgruppe sind
waisenkinder :: UNr -> Form IO ()
waisenkinder u = do
    lang <- get_preferred_language   
    h3 $ multi lang [(M.DE, "Waisenkinder"), (M.UK, "repair registrations")]
    plain $  multi lang [ (M.DE, "Studenten Ihrer Schule, die keine Übungsgruppe gewählt haben")
                   , (M.UK, "students of your school that did not choose a class")
                   ]
    studs <- io $ S.orphans $ u
    open btable
    Operate.Statistik.edit_studenten studs

-- | alle Übungen,
-- markiere besuchte Übungen
-- one-click für verlassen\/besuchen 
veranstaltungen :: ( S.Student , V.Vorlesung , Bool ) -> Form IO ()
veranstaltungen ( stud , vor, False ) = do
    lang <- get_preferred_language 

    h3 $ multi lang [ (M.DE, "Einschreibung"), (M.UK, "Enrollment")]

    plain $ multi lang [(M.DE, unlines [ "Um die Aufgaben für eine Vorlesung zu bearbeiten,"
                                       , "müssen Sie sich in einer Übungsgruppe dieser Vorlesung einschreiben."
                                       ])
                       ,(M.UK, unlines [ "To work on the exercises for a lecture"
                                       , "you need to enroll for a class associated to the lecture."
                                       ])
                       ]

    -- dieser student für diese Vorlesung
    ags <- io $ G.get_attended ( V.vnr vor ) ( S.snr stud )

    case ags of
        [] -> plain $ M.specialize lang
                    $ M.make [ (M.DE, "Sie sind in keine Übungsgruppe eingeschrieben.")
                             , (M.UK, "You are not enrolled in a class.")
                             ]
        _  -> show_gruppen (M.specialize lang $ M.make
               [(M.DE,"Sie sind eingeschrieben in Übungsgruppe:")
              , (M.UK,"You are enrolled in class") ] )  ags

    -- alle Gruppen für diese Vorlesung
    gs <- io $ G.get_this $ V.vnr vor
    show_gruppen (M.specialize lang $ M.make [ (M.DE,"alle Übungsgruppen zu dieser Vorlesung:")
                                             , (M.UK,"all classes for this lecture") ]) gs

    when ( Control.Types.Current /= V.einschreib vor ) $ do
         br
	 plain $ M.specialize lang
               $ M.make [ (M.DE, unlines [ "Das Ein/Ausschreiben ist"
			 , "nur von " ++ show ( V.einschreibVon vor )
			 , "bis " ++ show ( V.einschreibBis vor )
			 , "möglich."
			 ] )
                        , (M.UK, unlines [ "You can only enroll"
			 , "from " ++ show ( V.einschreibVon vor )
			 , "to " ++ show ( V.einschreibBis vor )
			 ] ) ]
	 mzero
    opts <- sequence $ do
        g <- gs
	return $ do
	    att <- io $ SG.attendance ( G.gnr g )
	    let here = G.gnr g `elem` map G.gnr ags
		msg  = toString ( G.name g )
                     ++ " "
		     ++ M.specialize lang
                     ( M.make [(M.DE, if here then "verlassen" else "besuchen")
                              ,(M.UK, if here then "leave" else "enroll")])
	    return ( msg , ( G.gnr g, here, att >= G.maxStudents g ) )
    open btable 
    ( g, here, full ) <- click_choice 
        (M.specialize lang $ M.make [(M.DE, "Gruppe"),(M.UK,"class")] ) $ opts
    close -- btable
    if here 
       then do 
           par
	   plain $ M.specialize lang 
                 $ M.make [(M.DE, "click auf besuchte Gruppe: abmelden")
                          ,(M.UK, "you were enrolled in this class and now you left it")]
           io $ SG.delete ( S.snr stud ) ( g )
       else do
           par
           plain $ M.specialize lang
                 $ M.make [(M.DE, "click auf nicht besuchte Gruppe: anmelden")
                          ,(M.UK, "you are now enrolled in this class")]
	   when full $ do
	        plain $ M.specialize lang
                      $ M.make [(M.DE, "Diese Gruppe ist voll."),(M.UK, "there are no more seats in this class")]
		mzero
           io $ SG.insert ( S.snr stud ) ( g )
           sequence_ $ do
		a <- ags
		return $ io $ SG.delete ( S.snr stud ) ( G.gnr a )
    return ()

veranstaltungen ( stud , vor, True ) = do
    h3 "Daten der Vorlesung"
    [ u ] <- io $ U.get_unr ( V.unr vor )
    V.edit u ( Just vor )
    
    h3 "Übungsgruppen zu dieser Vorlesung:"
    gs <- io $ G.get_this $ V.vnr vor

    open btable
    act <- click_choice "Aktion:"
        [ ("anzeigen", View )
        , ("erzeugen", Add )
	, ("bearbeiten", Edit )
	, ("löschen", Delete )
	]
    close -- btable
    case act of
	 View -> do
             show_gruppen "Übungsgruppen zu dieser Vorlesung:" gs
	 Add -> do
	     G.edit ( V.vnr vor ) Nothing
	 Edit -> do
             open btable
             g <- click_choice "Gruppe" $ do
	         g <- gs
		 return ( toString $ G.name g , g )
             close
	     G.edit ( V.vnr vor ) ( Just g )
	 Delete -> do
             open btable
             g <- click_choice "Gruppe" $ do
	         g <- gs
		 return ( toString $ G.name g , g )
             open row
	     click <- submit "wirklich löschen?"
             close
             close
	     io $ G.delete $ G.gnr g

show_gruppen header gs = do
     plain $ header
     open btable
     open row
     plain "Name" ; plain "Referent"
     plain "Studenten (jetzt)"
     plain "Studenten (maximal)"
     close
     sequence_ $ do
         g <- gs
         return $ do
             open row
             plain $ toString $ G.name g
             plain $ toString $ G.referent g
             c <- io $ SG.attendance $ G.gnr g
             plain $ show c
             plain $ toString $ G.maxStudents g
             close -- row
     close -- btable
    


----------------------------------------------------------------------------

aufgaben server ( stud, vnr, tutor ) = do
    lang <- get_preferred_language 

    h3 $ multi lang [(M.DE, "Aufgaben"), (M.UK, "Exercises")]

    -- let mks = do Right mk <- flatten tmk ; return mk

    let snr = S.snr stud

    -- das sind alle aufgaben 
    aufs <- io $ A.get $ Just vnr

    let opts = do
             auf <- aufs
             return ( toString $ A.name auf , Just $ auf )

    ( mauf , action ) <- 
        if tutor
	   then do 
                open btable 
	        mauf <- click_choice "Aufgabe"
                         $ ( "(neue Aufgabe)", Nothing ) : opts
                action <- click_choice "Action"
			  $ do act <- [ Statistics, Config, Delete ] 
			       return ( show act, act )
		close -- btable
                return ( mauf, action )
           else do 
		( action, auf ) <- statistik False stud aufs
                return ( Just auf, action )

    when ( tutor && Statistics == action ) $ do
         Just auf <- return mauf 
	 ( act, sauf, stud ) <- tutor_statistik vnr auf

	 case act of 
	     Rescore rsc -> do
		  sequence_ $ do
		      ( w, sauf, stud ) <- rsc
		      return $ do
		          Operate.Common.punkte tutor stud auf
			      ( Nothing, Nothing, Just w
			      , Just $ Html.primHtml 
					    "Bewertung durch Tutor"  
			      )

             Clear_Cache -> do
                  let d =  D.Datei { D.pfad = [ "autotool", "cache"
				   , toString vnr
				   , toString $ SA.anr sauf
				   ]
			  , D.name = toString $ S.mnr stud 
			  , D.extension = "cache"
			  } 
		  io $ D.loeschen d `CE.catch` \ ( any :: CE.SomeException ) -> return ()
		  plain $ "geloescht: " ++ show d

             _ -> do
                  mtriple <- show_previous ( Edit == act ) 
                                 vnr server stud auf sauf
		  case mtriple of
		      Nothing -> return ()
		      Just ( inst, inp, res, com ) -> do
                           [ stud ] <- io $ S.get_snr $ SA.snr sauf
			   -- das muß auch nach Einsendeschluß gehen,
			   -- weil es der Tutor ausführt
			   Operate.Common.punkte tutor stud auf ( inst, inp, res, com )
	 mzero

    let manr = fmap A.anr mauf
    
    when ( Delete == action ) $ do
        Just anr <- return manr
	wirk <- submit "wirklich löschen?"
	when wirk $ do
            io $ A.delete anr
            plain $ unwords [ "Aufgabe", show anr, "gelöscht." ]
	mzero

    -- here, action == Config (since other options ended with mzero)

    io $ debug $ unwords [ "Config" ,  server ]

    ( mk, type_click ) <- find_mk server tutor mauf

    auf' <- if tutor 
            then do
		 auf' <- edit_aufgabe server mk mauf vnr manr type_click
	         up <- submit "update data base: aufgabe"
                 when up $ io $ A.put manr auf'
                 return auf'

	    else -- kein tutor 
                case mauf of
		  Nothing -> do 
                      -- kommt eigentlich nicht vor?
		      plain "keine Aufgabe gewählt"
		      mzero
		  Just auf -> do
                      return auf
    stud' <- get_stud tutor stud

    case action of
        Config -> do
            ( minst :: Maybe O.Output, cs, res, com :: Maybe H.Html ) 
	        <- solution vnr manr stud' auf' 
	    return ()
        Solve -> do
            ( minst, cs, res, com ) 
                <- solution vnr manr stud' auf'
	    Operate.Common.punkte False stud' auf' 
                     ( fmap ( M.specialize M.DE .
                         O.render ) minst 
                     , cs
                     , Just res
                     , fmap (M.specialize M.DE)  com 
                     )
	Edit | tutor -> do
	    find_previous True  vnr server stud' auf'
            return ()
	View -> do
	    find_previous False vnr server stud' auf'
            return ()

    hr
    con <- io $ Operate.Motd.contents
    html con

    return ()

-------------------------------------------------------------------------


find_previous edit vnr mks stud auf = do

    -- kann sein, daß S.anr  error  ergibt (für tutor)
    sas <- io $ SA.get_snr_anr (S.snr stud) (A.anr auf) 
                   `CE.catch` \ (CE.SomeException any) -> return []
    case sas of
        [ sa ] -> do
	    show_previous edit vnr mks stud auf sa 
        _ -> return Nothing

fix_input vnr mks stud auf sa = case  SA.input sa of
   Just file -> return $ Just file
   Nothing -> io $ do
       -- fix location of previous einsendung
       let p = mkpar stud auf
           d = Operate.Store.location Operate.Store.Input 
                    p "latest" False
       file <- D.home d
       ex <- System.Directory.doesFileExist file
       let inf = fromCGI file
       if ex 
           then do
                -- nur infile-location einschreiben
                Control.Punkt.bepunkteStudentDB 
                         (P.ident p) (P.anr p) 
                         Nothing
                         Nothing (P.highscore p) 
                         ( Just inf )
                         Nothing
                return $ Just inf
           else return $ Nothing

fix_instant vnr mks stud auf sa = case SA.instant sa of
   Just file -> return $ Just file
   Nothing -> do
                lang <- get_preferred_language 
       -- transitional:
       -- (try to) re-generate previous instance
                ( _, _, com ) <- make_instant 
		    vnr ( Just $ A.anr auf ) stud auf
                let p = mkpar stud auf
                    d = Operate.Store.location Operate.Store.Instant
                           p "latest" False
                file <- io $ D.schreiben d 
                        $ show $ M.specialize M.DE 
                        $ ( O.render com :: Doc )
                let inst = fromCGI file
                io $ Control.Punkt.bepunkteStudentDB 
                         (P.ident p) (P.anr p) 
                         ( Just inst )
                         Nothing (P.highscore p) 
                         Nothing 
                         Nothing
                return $ Just inst

-- | TODO: possibly with edit (for tutor)
show_previous edit vnr mks stud auf sa0 = do
    lang <- get_preferred_language 
    inf <- fix_input vnr mks stud auf sa0
    ins <- fix_instant vnr mks stud auf sa0
    let sa = sa0 { SA.input = inf, SA.instant = ins }

    hr ;  h3 $ M.specialize lang 
             $ M.make [(M.DE,"Vorige Einsendung und Bewertung zu dieser Aufgabe")
                      ,(M.UK,"Previous submission and evaluation for this problem")]
    -- pre $ show sa
    br ; plain $ M.specialize lang $ M.make [(M.DE,"Aufgabenstellung:"),(M.UK,"Problem")]
    case SA.instant sa of
        Just file -> do
            cs <- io $ logged "Super.view" 
    	         $ readFile $ toString file
    	    html $ Html.primHtml cs
        Nothing -> do
	    plain "(keine Aufgabe)"
    br ; plain $ M.specialize lang $ M.make [(M.DE,"Einsendung:"),(M.UK,"Submission:")]
    case SA.input sa of
        Just file -> do
            cs <- io $ logged "Super.view" 
    	         $ readFile $ toString file
    	    pre cs
        Nothing -> do
	    plain "(keine Einsendung)"
    br ; plain "Bewertung:"
    h <- case SA.report sa of
        Just file -> do
            -- alte bewertung ist schon da
	    io $ readFile $ toString file
        Nothing -> case edit of
            -- alte bewertung nicht da
	    False -> return "(keine Bewertung)"
            True -> case SA.input sa of
                 -- stattdessen alte eingabe lesen
                 Just file -> io $ readFile $ toString file
		 Nothing -> return "(keine Eingabe)"
    case edit of
	 False -> do
             html $ Html.primHtml h
             blank -- ??
	     return Nothing
         True  -> do
               mcom <- textarea h
               let com = fromMaybe h mcom   
               open table
               mgrade <- click_choice0  "Grade" $
			[ ("(none)", Nothing )
			, ("Pending", Just Pending)
			, ("No", Just No) 
                        ] ++ do
                           p <- [ 1 .. 10 ]
                           let w = Control.Types.ok p
			   return (show w, Just $ w)
			
               close -- table
	       return $ Just ( Nothing
			     , Nothing 
			     , mgrade
			     , case mgrade of 
			           Just x | x /= Pending -> 
			                 Just $ Html.primHtml com 
			           _ -> Nothing
			     )



------------------------------------------------------------------------

data Action = Solve  -- ^ neue Lösung bearbeiten
	    | View -- ^ alte Lösung + Bewertung ansehen
	    | Edit -- ^ alte Lösung + Bewertung ändern
	    | Rescore [ ( Wert, SA.Stud_Aufg, S.Student ) ] 
            | Clear_Cache
	    | Statistics 
	    | Config
            | Delete 
            | Add
     deriving ( Show, Eq, Typeable )

-- data Display = Current | Old 
--     deriving ( Show, Eq, Typeable )

-- | für Student: statistik aller seiner Aufgaben anzeigen, 
-- für Tutor: kann Aufgabenlösung sehen und (nach-)korrigieren
-- mit aufgabenauswahl
statistik tutor stud aufs = do
    lang <- get_preferred_language 
    hr 
    h3 $ M.specialize lang
       $ M.make [(M.DE, "Punktestand und Aufgaben-Auswahl") ,(M.UK, "Exercises and Scores") ]
    -- daten holen
    score <- io $ sequence $ do
        auf <- aufs
	return $ do
            sas <- SA.get_snr_anr (S.snr stud) (A.anr auf) 
            let okno = case sas of
		     [    ] ->  ( Oks 0, Nos 0 , Nothing )
		     [ sa ] ->  ( SA.ok sa, SA.no sa, SA.result sa )
	    let stat = if A.current auf then Current else Late
	    return ( auf, stat, okno )

    open btable
    disp <- click_choice_with_default 0 
         ( M.specialize lang
         $ M.make [(M.DE,"Aufgaben anzeigen:") ,(M.UK,"show exercises:") ] )
	      [ ( M.specialize lang     
                $ M.make [(M.DE,"nur aktuelle"),(M.UK,"current only")]
                , [ Current ] )
	      , ( M.specialize lang
                $ M.make [(M.DE,"alle"),(M.UK,"all")]
                , [ Late, Current , Early ] )
	      ]
    close -- btable
    br

    -- daten anzeigen
    let dat = do
	    begin -- mutex
            open btable
            open row
            M.specialize lang 
                $ M.make [(M.DE, do plain "Aufgabe" ; plain "Status" ; plain "Highscore"
                                    plain "Bearbeitungszeit"
                                    plain "vorige Bewertung" ; plain "Gesamt-Wertungen" )
                         ,(M.UK, do plain "problem" ; plain "status"; plain "highscore"
                                    plain "available" ; plain "previous evaluation" ; plain "total evaluations" )
                         ]
            close -- row
            sequence_ $ do 
                ( auf, _ , ( ok, no, mres ) ) <- score
		guard $ A.timeStatus auf `elem` disp
                let name = toString $ A.name  auf
                return $ do
            	    open row
                    let col = if A.current auf
			         && Mandatory == A.status auf
			      then if ok > Oks 0
				   then "green"
				   else "red"
			      else "black"
                    farbe col $ toString $ A.name auf
                    farbe col $ toString $ A.status auf
                    farbe col $ toString $ A.highscore auf
                    farbe col $ M.specialize lang $ M.make
                        [(M.DE, case A.timeStatus auf of
                            Early -> "erst ab " ++ toString ( A.von auf )
                            Current -> "noch bis " ++ toString ( A.bis auf )
                            Late -> "vorbei seit " ++  toString ( A.bis auf ) )
                        ,(M.UK, case A.timeStatus auf of
                            Early -> "from " ++ toString ( A.von auf )
                            Current -> "until " ++ toString ( A.bis auf )
                            Late -> "expired " ++  toString ( A.bis auf ) )
                        ]
                    farbe col $ case mres of
				  Just res -> show res
				  Nothing  -> ""
            	    farbe col $ show ( ok , no )
                    sequence_ $ do
		        ch <- case A.timeStatus auf of
			        _ | tutor -> [ View, Edit ]
			        Early     -> []
				_         -> [ Solve, View ]
                        return $ click ( show ch , ( ch, auf ))
            	    close -- row
            close -- table
            end -- mutex
    -- auswerten
    let goal = sum $ do 
            ( auf, _ , okno ) <- score
	    guard $ A.status auf == Mandatory
	    return ( 1 :: Int )
	done = sum $ do 
            ( auf, _ , (ok, no, mres) ) <- score
	    guard $ A.status auf == Mandatory
	    guard $ ok > Oks 0
	    return ( 1 :: Int )
        percent = ( 100 * done ) `div` goal
        -- anzeigen
        aus = when ( goal > 0 ) $ do
	    plain $ M.specialize lang $ M.make
                [ (M.DE, unwords 
                  [ "Von", show goal, "Pflicht-Aufgaben" 
		  , "haben Sie bis jetzt", show done, "erledigt."
		  , "Das sind", show percent, "Prozent." 
		  ] )
                , (M.UK, unwords 
                  [ "of", show goal, "mandatory exercises" 
		  , "you have already solved", show done, "."
		  , "this is", show percent, "percent." 
		  ] )
                ]

    ( ch, auf ) <- dat ; br ; aus
    return ( ch, auf )

--------------------------------------------------------------------------

data Entry = Entry
	   { snr :: SNr
	   , mnr :: MNr
	   , vorname :: Name
	   , name :: Name
	   , oks :: Oks
	   , nos :: Nos
	   }


---------------------------------------------------------------------------


-- | will return Maybe Stud_Aufg for re-grading
tutor_statistik :: VNr -> A.Aufgabe 
		-> Form IO ( Action, SA.Stud_Aufg, S.Student )
tutor_statistik vnr auf = do
    hr
    saufs <- io $ Control.Stud_Aufg.DB.get_anr $ A.anr auf

    h3 "Statistik für diese Aufgabe"


    open_btable_with_sorter 
	      [ "Matrikel", "Vorname" , "Name" , "Oks" , "Nos" , "Result" ]

    begin -- mutex


    -- erstmal die, die wirklich was eingesandt haben
    rscores1 <- sequence $ do
        sauf <- saufs
        return $ do
            [ stud ] <- io $ Control.Student.DB.get_snr $ SA.snr sauf
	    open row
	    plain $ toString $ S.mnr stud
	    plain $ toString $ S.vorname stud
	    plain $ toString $ S.name stud
            plain $ toString $ SA.ok sauf
	    plain $ toString $ SA.no sauf
	    plain $ show $ SA.result sauf
	    click ( "View", ( View, sauf, stud ))
	    click ( "Edit",  ( Edit, sauf, stud ))
	    click ( "Clear_Cache",  ( Clear_Cache, sauf, stud ))
            rs <- radio_score sauf stud
            close -- row
	    return rs    

    -- dann die, die noch gar nichts geschickt haben
    all_studs <- io $ Control.Vorlesung.DB.steilnehmer vnr
    let done = mkSet $ map SA.snr saufs
    rscores2 <- sequence $ do
          stud <- all_studs
          guard $ not $ S.snr stud `elementOf` done
          return $ do
              open row
	      plain $ toString $ S.mnr stud
	      plain $ toString $ S.vorname stud
	      plain $ toString $ S.name stud
              plain $ "--"
	      plain $ "--"
	      plain $ "--"
              sauf <- io $ Control.Stud_Aufg.DB.put_blank 
                              (S.snr stud) (A.anr auf)
	      click ( "Edit",  ( Edit, sauf, stud ))
              rs <- radio_score sauf stud
              close -- row
	      return rs

    close -- btable
    click ( "Edits ausführen"
	  , ( Rescore ( concat $ rscores1 ++ rscores2 ) 
	    , error "sa" :: SA.Stud_Aufg
	    , error "s" :: S.Student
	    )
	  )
    end -- mutex

-- | input widget for a score
radio_score sauf stud = do
    p <- radiogroup "keep" 
            $ ( "keep", [] )
	    : ( "No"  , [ ( No, sauf, stud ) ] )
	    : do p <- [ 1 .. 5 ] 
		 return ( show p
			, [ ( Control.Types.ok p
			    , sauf, stud
			    )
			  ] 
			)
    return $ concat $ maybeToList p

btabled :: Monad m => Form m a -> Form m a
btabled = bracketed btable

bracketed b action = do open b; x <- action ; close ; return x
