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
    putStrLn $ "rescore for Aufgaben " ++ show args
    forM_ args rescore_for_aufgabe

rescore_for_aufgabe arg = do
    let anr = ANr $ read arg
    [ auf ] <- A.get_this anr
    sas <- SA.get_anr anr
    forM sas $ \ sa -> do
        void $ G.runForm 
             $ Operate.Recommon.recompute_for_einsendung auf sa

