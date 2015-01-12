-- is a main module

{-# language TupleSections #-}
{-# language PatternSignatures #-}
{-# language OverloadedStrings #-}

import Geo.Program.Type

import qualified Geo.Program.Run

import Autolib.Reader
import Autolib.ToDoc
import Autolib.TES.Identifier
import Autolib.Reporter
import System.Environment
import Control.Applicative
import System.Random
import System.IO

main = do
  args <- getArgs
  (f, s) <- case args of
    [] -> ("stdin",) <$> getContents
    [f] -> (f,) <$> readFile f
  let p = parse (reader :: Parser (Exp Identifier)) f s
  case p of
       Left e -> error $ show e
       Right x -> handle x

handle p = do
  putStrLn "input program"
  print $ toDoc p
  
  putStrLn "concrete evaluation (with random rational values)"
  g <- newStdGen
  let (out, msg :: Doc) =
        export $ Geo.Program.Run.concrete g p
  print msg ; hFlush stdout
  case out of
    Just (val, dgc) -> do
      print $ vcat [ text "value" <+> toDoc val
               , text "degeneracy conditions" <+> toDoc dgc
               ]  

  putStrLn "symbolic evaluation (polynomials)"
  let (out, msg :: Doc) =
        export $ Geo.Program.Run.symbolic p
  print msg ; hFlush stdout
  case out of
    Just (val, dgc) -> do
      print $ vcat [ text "value" <+> toDoc val
               , text "degeneracy conditions" <+> toDoc dgc
               ]  

instance ToDoc StdGen 
