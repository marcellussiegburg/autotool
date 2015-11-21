module Control.Student.DB where

import Control.SQL
import Control.Types
import Operate.Crypt
import qualified Control.Student.Type as CST
import qualified Data.Set as S
import Data.List ( sortBy )
import Data.Function (on )

import Prelude hiding ( all )

-- | get alle passenden studenten aus DB
-- TODO: implementiere filter
get_unr_mnr  :: ( UNr , MNr ) -> IO [ CST.Student ]
get_unr_mnr ( unr , mnr ) = 
    get_where $ ands
	      [ equals ( reed "student.UNr" ) ( toEx unr )
	      , equals ( reed "student.MNr" ) ( toEx mnr )
	      ]

get_unr_name :: (UNr, Name, Name) -> IO [ CST.Student ]
get_unr_name (unr, sn, gn) = do
  get_where $ ands 
              [ equals ( reed "student.UNr" ) ( toEx unr )
              , equals ( reed "student.Name") (toEx sn)
              , equals ( reed "student.Vorname") (toEx gn) 
              ]

get_email :: Email -> IO [ CST.Student ]
get_email em = do
  get_where $ ands 
              [ equals ( reed "student.Email" ) ( toEx em )
              ]

-- | studenten mit übereinstimmender eppn.
-- wenn kein solcher gespeicher ist ("use shibboleth"),
-- dann studenten mit übereinstimmenden Schule, Vornamen, Namen
-- (Matrikelnr. wird nicht benutzt, weil es mglw. keine gibt oder diese
-- gewechselt hat (bachelor/master))
get_unr_sn_gn_mnr_meppn ( unr , sn, gn, mnr, meppn ) = do
  eppn_matchs <- case meppn of
    Nothing -> return []
    Just eppn -> get_where $ ands [ equals ( read "student.EMail" ) (toEx eppn) ]
  if not $ null eppn_matches
    then eppn_matches
    else get_where $ ands 
        [ equals ( reed "student.UNr" ) ( toEx unr )
        , equals ( reed "student.Name") (toEx sn)
        , equals ( reed "student.Vorname") (toEx gn)
        , equals ( reed "student.Email") (toEx "use shibboleth")
        ]
  
-- | wenn mnr = "", dann wird diese nicht geprueft,
-- das ist fuer tutoren, die bisher mnr hatten, 
-- aber ueber shibboleth keine bekommen.
--
-- extra basteleien wegen
-- http://nfa.imn.htwk-leipzig.de/bugzilla/show_bug.cgi?id=360
get_unr_sn_gn_mnr ( unr , sn, gn, mnr ) = do
    let mnrs = digest mnr
    candidates <- get_where $ ands $
              [ equals ( reed "student.UNr" ) ( toEx unr )
              , equals ( reed "student.Name") (toEx sn)
              , equals ( reed "student.Vorname") (toEx gn) 
              ]
    let compat c = compatible mnrs $ digest $ CST.mnr c
        studs = filter compat candidates
        -- HACK: mehrere bestehende Accounts -> neuester
        recent = take 1 
               $ reverse 
               $ sortBy (Prelude.compare `on` CST.snr )
               $ studs
    return recent

digest :: MNr -> S.Set String
digest = S.fromList . uncomma . toString

compatible :: S.Set String -> S.Set String -> Bool
compatible sent stored = 
    if S.null sent 
    then -- für Mitarbeiter (haben keine MNr)
         S.null stored
    else -- für Studenten (haben wenigstens eine)
         not $ S.null $ S.intersection sent stored

uncomma :: String -> [String]
uncomma s = if null s then [] else
    let (pre, post) = span (/= ',') s
    in  pre : case post of [] -> [] ; _ : t -> uncomma t
    

get_snr  :: SNr -> IO [ CST.Student ]
get_snr snr = get_where $ equals ( reed "student.SNr" ) ( toEx snr )

get_unr  :: UNr -> IO [ CST.Student ]
get_unr unr = get_where $ equals ( reed "student.UNr" ) ( toEx unr )

snr_by_unr_mnr :: ( UNr, MNr ) -> IO [ SNr ]
snr_by_unr_mnr um = get_unr_mnr um >>= return . map CST.snr

-- so findet man die Waisenkinder:
-- select student.* -- oder sogar *
-- from student left join stud_grp ON student.snr=stud_grp.snr 
-- where stud_grp.gnr is null;  

-- | alle, die in keiner Übungsgruppe sind
orphans :: UNr -> IO [ CST.Student ]
orphans unr = 
    let from = reed "student LEFT JOIN stud_grp ON student.snr = stud_grp.snr"
    in  get_from_where [ from ] $ ands
            [ equals ( reed "student.UNr" ) ( toEx unr )
	    , reed "stud_grp.gnr IS NULL"
	    ]

get_from_where from ex = do
    conn <- myconnect
    stat <- squery conn $ Query
        ( Select $ map reed [ "student.SNr", "student.UNr", "student.MNr", "student.Name", "student.Vorname" 
			    , "student.Email", "student.Passwort", "student.Next_Passwort" 
			    ] )
        [ From $ from
        , Where $ ex
	]
    inh  <- collectRows (\ state -> do
        s_snr <- getFieldValue state "SNr"
    	s_unr <- getFieldValue state "UNr"
    	s_mnr <- getFieldValue state "MNr"
        s_name <- getFieldValue state "Name"
        s_vorname <- getFieldValue state "Vorname"
        s_email <- getFieldValue state "Email"
        s_passwort <- getFieldValue state "Passwort"
        s_next_passwort <- getFieldValue state "Next_Passwort"
        return $ CST.Student { CST.snr = s_snr
    			     , CST.unr = s_unr
    			     , CST.mnr = s_mnr
			     , CST.name = s_name
			     , CST.vorname = s_vorname
			     , CST.email = s_email
			     , CST.passwort = s_passwort
			     , CST.next_passwort = s_next_passwort
    			     }
                    ) stat
    disconnect conn
    return inh

get_where :: Expression -> IO [ CST.Student ]
get_where ex = get_from_where ( map reed [ "student" ] ) ex

-- | put into table:
-- do not evaluate Student.snr (it may be undefined!)
-- instead use first argument: Just snr -> update, Nothing -> insert
put :: Maybe SNr 
    -> CST.Student
    -> IO ()
put msnr stud = do
    conn <- myconnect 
    let common = [ ( reed "UNr", toEx $ CST.unr stud )
		 , ( reed "Name", toEx $ CST.name stud )
		 , ( reed "Vorname", toEx $ CST.vorname stud )
		 , ( reed "Email", toEx $ CST.email stud )
		 , ( reed "Passwort", toEx $ CST.passwort stud )
		 , ( reed "Next_Passwort", toEx $ CST.next_passwort stud )
		 ]
    -- stat <- case msnr of
    case msnr of
	 Nothing -> squery conn $ Query
            ( Insert (reed "student") 
	      $ ( reed "MNr" , toEx $ CST.mnr stud ) : common 
            ) 
	    [ ]
         Just snr -> squery conn $ Query
            ( Update (reed "student") common ) 
	    [ Where $ equals ( reed "student.SNr" ) ( toEx snr ) ]
    disconnect conn

-- not used
switch_passwort :: CST.Student -> IO ()
switch_passwort stud = do
    conn <- myconnect
    squery conn $ Query
           ( Update ( reed "student" )
                    [ ( reed "Passwort", reed "Next_Passwort" ) ] )
           [ Where $ equals ( reed "student.SNr" ) ( toEx $ CST.snr stud ) ]
    squery conn $ Query
           ( Update ( reed "student" )
                    [ ( reed "Passwort", toEx $ Crypt "" ) ] )
           [ Where $ equals ( reed "student.SNr" ) ( toEx $ CST.snr stud ) ]
    disconnect conn



