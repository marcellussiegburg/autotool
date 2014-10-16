-- module Main where
module Main where

import qualified Scorer.Aufgabe
import Scorer.Compute
import Scorer.Util

import qualified Control.Schule as U
import qualified Control.Vorlesung as V
import qualified Control.Semester as E
import qualified Control.Aufgabe as A

import Autolib.ToDoc
import Autolib.Output ( Output )
import qualified Autolib.Output as O
import qualified Autolib.Multilingual
import qualified Text.Blaze.Html

import Autolib.FiniteMap
import Control.Monad ( guard, when, forM )
import Data.Maybe
import qualified Data.Text as T

import System.Environment

-- | usage: ScorerDB [--cleartext] file1 file2 ...

main :: IO ()
main = do
    t <- zeit    
    let header = O.Text $ T.pack $ "autotool -- Top Ten Scores, Stand von: " ++ t        
    schulen <- U.get
    outss <- forM schulen $ \ schule -> do
            sems <- E.get_at_school $ U.unr schule 
            let current_sems = filter ( \ e -> E.status e == Current) sems
            table <- forM current_sems $ \ sem -> do
                 vors <- V.get_at_school_sem schule sem
                 info <- forM vors $ \ vor -> do
                     aufs <- A.get $ Just $ V.vnr vor
                     let def = listToFM 
                             $ map ( \  a -> (A.anr a, a ) ) 
                             $ filter ( \ a -> A.highscore a /= Keine )
                             $ aufs
                     return ( vor, def )
                 return info
            forM (filter ( \ (vor,def) -> not $ isEmptyFM def ) $ concat table) $ compute schule
    let out = O.lead header 
            $ O.Itemize $ concat outss >>= maybeToList
    putStrLn "<head><meta charset=\"UTF-8\"></head>"
    print $ Autolib.Multilingual.specialize Autolib.Multilingual.DE
          $ ( O.render out :: Autolib.Multilingual.Type Text.Blaze.Html.Html )

