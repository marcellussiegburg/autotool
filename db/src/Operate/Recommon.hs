{-# language PatternSignatures #-}

module Operate.Recommon where

import qualified Control.Stud_Aufg as SA
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import Control.Types

import Operate.Types

-- import Inter.Collector
-- import Inter.Evaluate
import Operate.Common
-- import Inter.Types
import qualified Operate.Param as P
import qualified Operate.Bank ( logline ) 

import qualified Util.Datei
import Challenger.Partial

import qualified Gateway.CGI as G

import Autolib.Reporter hiding ( wrap )
import Autolib.ToDoc
import Autolib.Reader

import Control.Exception ( catch )
import System.IO
import qualified System.Time
import qualified System.Directory

verbose :: Bool
verbose = False -- True

--  recompute_for_einsendung  :: Make -> A.Aufgabe -> SA.Stud_Aufg -> IO ()
recompute_for_einsendung  auf eins = 
    {- wrap ("for einsendung " ++ show eins ) $ -} do
        
        studs <- G.io $ S.get_snr $ SA.snr eins
        mapM ( \ s -> recompute_for_student auf eins s 
		--  `Control.Exception.catch` \ any -> do
	        --      hPutStrLn stderr $ "err: " ++ show any
	     ) studs
        return ()

recompute_for_student auf eins stud = do
        ( sti, sol, doc ) <- 
            Operate.Common.make_instant_common (A.vnr auf) (Just $ A.anr auf) stud auf 
                --   ( fun $ read $ toString $ A.config auf ) 
        input   <- G.io $ read_from_file $ SA.input eins
        ( res, doc  ) <- G.io $ Operate.Types.evaluate auf sti input
        let old_result = SA.result  eins
        G.io $ if ( compatible old_result res )
           then do
              hPutStr stderr "."
           else do
              hPutStrLn stderr $ show $ vcat
                   [ text "aufgabe:" <+> toDoc auf
                   , text "einsendung:" <+> toDoc eins
                   -- , text "instant:" <+> toDoc instant
                   , text "input:" <+> text input
                   -- , text "comment:" <+> doc
                   , text "computed result:" <+> toDoc res
                   , text "stored result:" <+> toDoc old_result
                   ]
              hFlush stderr
        let Just inf = SA.input   eins
        clock <- G.io $ System.Directory.getModificationTime $ toString inf

        -- cal <- G.io $ System.Time.toCalendarTime clock    
        -- let time = System.Time.calendarTimeToString cal
        let time = show clock
        let param = P.Param { P.mmatrikel = Just $ S.mnr stud 
                            , P.vnr = A.vnr auf
                            , P.anr = A.anr auf
                            }
        case res of
            Just res -> do
                G.io $ putStrLn $ Operate.Bank.logline time "771" param res
            Nothing -> return ()


compatible ( Just Pending ) _ = True
compatible ( Just No ) Nothing = True
compatible x y = x == y

read_from_file :: Maybe File -> IO String
read_from_file ( Just fname ) = do
    this <- readFile $ toString fname
    return this

