module Geo.Pappus where

import Geo.Program.Type

import Autolib.Reader
import Autolib.ToDoc
import Autolib.TES.Identifier

main = do
  p <- parseFromFile (reader :: Parser (Exp Identifier)) "Geo/pappus.gp"
  case p of
       Left e -> error $ show e
       Right x -> print $ toDoc x
  
