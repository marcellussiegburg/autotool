-- module main where

{-

input: log file name with lines like:

Fri Nov 28 18:33:49 CET 2003 ( 2425 ) cgi-318 ( 318 ) VNr-ANr : OK # Size: 7 
Fri Nov 28 18:33:49 CET 2003 ( 2425 ) cgi-318 ( 318 ) VNr-ANr : NO 


action: read the corresponding input file and re-do the evaluation

output: ?

-}

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

patience :: Int
patience = 60 -- seconds

main :: IO ()
main = do
    args <- getArgs
    contents <- mapM readFile args

    forM_ ( slurp $ concat contents ) $ \ ein -> do
        rescore ein `Control.Exception.catch` \ e -> return ()

    
rescore :: Einsendung 
	-> IO ()
rescore e = do    
    let mat = internal $ matrikel e 
    let infile = Datei { pfad  = [ "autotool", "done"
                                 , toString ( vor e ) , toString ( auf e )
                                 , toString mat
                                 , if isJust ( msize e ) then "OK" else "NO"
                                 ]
                      , name = pid e
                      , extension = "input"
                      }
    input <- Util.Datei.lesen infile
    [ aufgabe ] <- A.get_this $ Scorer.Einsendung.auf e

    void $ G.runForm $ Operate.Recommon.recompute_for_einsendung aufgabe e

