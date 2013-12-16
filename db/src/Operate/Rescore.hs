-- | input: on stdin, a list of file names, e.g.
-- /space/autotool/done/196/2020/8644/OK/5837.input
-- /space/autotool/done/196/2020/8644/OK/latest.input


{-# language PatternSignatures #-}

import Scorer.Einsendung 

import Operate.Recommon

import Operate.Store ( location, load, store )
import Operate.Bank ( logline )
-- import Inter.Boiler ( boiler )

-- import Inter.Collector
import Operate.Common
import Operate.Types
-- import Inter.Types
-- import Inter.Evaluate
-- import qualified Inter.Param as P

import qualified Control.Stud_Aufg as SA
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import qualified Control.Schule as U
import qualified Control.Vorlesung as V
import Control.Types

import qualified Gateway.CGI as G

import Util.Datei

import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Timer 

import Control.Monad ( guard, forM, void )
import Control.Exception
import Data.Maybe ( isJust )
import System.Environment (getArgs)
import System.IO ( hPutStrLn, stderr )

patience :: Int
patience = 60 -- seconds

main :: IO ()
main = do
    input <- getContents
    forM_ (lines input) rescore_for_line

rescore_for_line fname = do
    let ws = words $ map ( \ c -> if c == '/' then ' ' else c) fname
    hPutStrLn stderr $ show ws
    case ws of 
        [ "space", "autotool", "done", vnr, anr, mnr, okno, file ] -> do
           [ vor ] <- V.get_this $ VNr $ read vnr
           [ auf ] <- A.get_this $ ANr $ read anr
           [ stud ] <- S.get_unr_mnr ( V.unr vor , MNr mnr )
           void $ G.runForm 
             $ Operate.Recommon.recompute_for_student fname auf stud
        _ -> hPutStrLn stderr $ "cannot handle " ++ fname


        
