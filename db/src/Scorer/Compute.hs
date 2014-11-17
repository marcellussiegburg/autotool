{-# language OverloadedStrings #-}

module Scorer.Compute where

import Scorer.Aufgabe
import Scorer.Einsendung
import Scorer.Config
import Scorer.Emit

import Control.Types hiding ( size )
import Control.Aufgabe.Typ

import qualified Control.Vorlesung as V
import qualified Control.Schule as U

import Autolib.ToDoc
import Autolib.Output ( Output )
import qualified Autolib.Output as O

import Autolib.Util.Sort
import Autolib.FiniteMap
import Control.Monad ( guard )
import System.Environment ( getArgs )

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import Control.Applicative ((<$>),(<*))
import Control.Monad ( forM)

import System.IO (hPutStrLn, stderr)

-- | in fm steht abbildung von aufgabe(name) auf inhalt (z. b. direction)
compute :: U.Schule -> ( V.Vorlesung, ScoreDefFM ) -> IO ( Maybe Output )
compute u ( vor, aufs ) = do
  
    hPutStrLn stderr $ unwords [ "compute", show $ toDoc u, show vor, show $ toDoc aufs ]

    -- wir lesen die logfiles für jede vorlesung komplett neu ein,
    -- damit wir die entries, die wir nicht brauchen, 
    -- gleich wieder weghauen können

    args <- getArgs

    let (decorate,fileargs) = if null args then (False,[])
			      else ( case head args of
                                        "DECORATE" -> (True,tail args)
                                        "--cleartext" -> (False,tail args)
			                _ -> (True,args)
				   )

    contents <- forM fileargs $ \ f -> do
         s <- BS.readFile f
         case A.parseOnly ( slurp_deco decorate  ) s of
             Right es -> return $ filter Scorer.Einsendung.okay es
             Left err -> do
               hPutStrLn stderr err
               return []

    let einsendungen = concat contents

    let total = foldl ( update aufs ) emptyFM einsendungen
    -- pforsicht: enthalt alle Einsendungen zu dieser Aufgabe,
    -- (auch die vom Tutor, der normalerweise nicht eingeschrieben ist)
    -- Damit wir "best known" anzeigen können
    -- vor der bepunktung müssen die aber raus

    emit decorate u vor total

update :: ScoreDefFM -> DataFM -> Einsendung -> DataFM
update aufs mappe e = 
    case lookupFM aufs (auf e) of
        Nothing -> mappe -- aufgabe unbekannt
	Just a -> case highscore a of
	    Keine -> mappe -- keine wertung
	    dir  -> addToFM_C ( collide dir ) mappe (auf e) [ e ]

collide :: HiLo
	-> [ Einsendung ] -> [ Einsendung ] 
	-> [ Einsendung ]
collide dir schon neus = 
    let fun = case dir of
            Low -> id ; High -> negate 
    in    take ( scoreItems + 5 ) -- platz lassen für admins (s. o.)
	$ nubBy matrikel -- nur eine lösung je student
        $ mergeBy ( \ e -> ( fun ( size e ) -- erst nach größe
			    , date e )        -- dann nach zeit
		   ) neus schon

mergeBy fun xs ys = sortBy fun (xs ++ ys) -- FIXME


