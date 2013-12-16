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
import Data.Time.LocalTime
import System.Locale
import Data.Time.Format

verbose :: Bool
verbose = False -- True

recompute_for_student fname auf stud = do
    ( sti, sol, doc ) <- 
            Operate.Common.make_instant_common (A.vnr auf) (Just $ A.anr auf) stud auf 
    input <- G.io $ readFile fname
    ( mres, doc  ) <- G.io $ Operate.Types.evaluate auf sti input
    G.io $ hPutStrLn stderr $ show $ vcat
                   [ text "aufgabe:" <+> toDoc auf
                   , text "input:" <+> text input
                   -- , text "comment:" <+> doc
                   , text "computed result:" <+> toDoc mres
                   ]
    G.io $ hFlush stderr
        
    clock <- G.io $ System.Directory.getModificationTime fname
    lt <- G.io $ utcToLocalZonedTime clock        
    let time = formatTime defaultTimeLocale "%c" lt
            
    let param = P.Param { P.mmatrikel = Just $ S.mnr stud 
                            , P.vnr = A.vnr auf
                            , P.anr = A.anr auf
                            }
    case mres of 
        Just res -> G.io $ putStrLn $ Operate.Bank.logline time "771" param res
        Nothing -> return ()






