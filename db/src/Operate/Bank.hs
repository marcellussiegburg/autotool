module Operate.Bank where

import Control.Punkt
import Control.Types
import qualified System.Posix

import System.Time
import qualified Operate.Param as P

import qualified Operate.Store
import Util.Datei

bank :: P.Type -> IO String
bank p = do

    ( pid , minfile  ) <- Operate.Store.store Operate.Store.Input p 
    ( _ , minstfile  ) <- Operate.Store.store Operate.Store.Instant p 
    
    mrepfile <- case P.mresult p of
        Just x | x /= Pending -> do 
            ( pid , mrepfile ) <- Operate.Store.store Operate.Store.Report p 
	    return $ mrepfile
        _ -> return Nothing

    bepunkteStudentDB (P.ident p) (P.anr p) 
         ( minstfile )
         (P.mresult p) (P.highscore p)
         ( minfile ) ( mrepfile )

    case P.mresult p of
        Nothing  -> do
	    return "(kein Resultat => kein Eintrag)"
	Just res -> do
	    time <- zeit
	    let msg = logline  time pid p res

            d <- datum
	    let logcgi = Datei	{ pfad = [ "autotool", "log" ] ++ d
			, name = "CGI"
			, extension = ""
			}
	    anhaengen logcgi msg
	    return msg


logline time pid p res = unwords [ time
		      , "(",  pid, ")"
		      , "cgi-" ++ P.smatrikel p
		     , "(", P.smatrikel p, ")"
		      , P.subject p , ":"
		     , result_string res
		     , "\n"
		     ]

result_string :: Wert -> String
result_string mres = case mres of
    Pending -> "Pending"
    No -> "NO"
    Okay {} -> "OK # Size: " ++ show (size mres)
                 ++ " Punkte: " ++ show (punkte mres)

datum :: IO [ String ]
datum = do
    clock <- getClockTime
    cal <- toCalendarTime clock    
    return [ show $ ctYear cal
	   , show $ ctMonth cal
	   , show $ ctDay cal
	   ]

zeit :: IO String
zeit = do
    clock <- getClockTime
    cal <- toCalendarTime clock    
    return $ calendarTimeToString cal

