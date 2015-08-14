{-# language TupleSections #-}

-- module Control.Student.Zulassungen where

import Control.Types

import qualified Control.Student as S
import qualified Control.Aufgabe as A
import qualified Control.Vorlesung as V
import qualified Control.Stud_Aufg as SA

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( forM, forM_ )
import Data.List
import Control.Applicative

main = do
  zs <- M.unions <$> forM [ VNr 152, VNr 175, VNr 187, VNr 198 ]  zugelassen
  studs <- forM (M.toList zs) $ \ (s,v) -> (,v) <$> S.get_snr s 
  let info = sort $ map (\ ([s],v) -> (S.name s, S.vorname s, S.mnr s, V.name v)) studs 
  forM_ info $ \ ( Name f, Name v, MNr m, Name vor ) -> putStrLn $ concat $ intersperse "," [ f, v, m, vor ]

-- | Fuer die Vorlesung: alle Studenten, die >= 50 prozent pflichtaufgaben haben
zugelassen :: VNr -> IO ( M.Map SNr V.Vorlesung )
zugelassen v = do
  [vor] <- V.get_this v
  print ( v, V.name vor )
  aufs <- A.get (Just v)
  let pflicht = filter (\ a -> A.status a == Mandatory)  aufs
  -- putStrLn $ unwords $ "Pflichtaufgaben" : map (show . A.anr) pflicht
  studs <- forM pflicht $ \ a -> do
     sas <- SA.get_anr $ A.anr a
     return $ S.fromList $ map SA.snr $ filter ( \ sa -> let Oks n = SA.ok sa in n > 0 ) sas
  -- putStrLn $ unwords $ "Studenten" : map show studs
  let zug :: [ SNr ]
      zug = M.keys
          $ M.filter ( \ v -> 2*v >= length pflicht)
          $ M.fromListWith (+) $ do sas <- studs ; s <- S.toList sas ; return (s,1)
  -- putStrLn $ unwords $ "zugelassen" : map show zug
  return $ M.fromList $ do z <- zug ; return (z, vor)

