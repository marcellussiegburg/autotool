-- parsing related utility functions

module Util.Parse (
    parseHelper
) where

import Util.Description

import Types.Description

import Autolib.Reader
import Autolib.Output
import Gateway.Errmsg

import Control.Monad.Error

import Autolib.Multilingual as M

parseHelper
    :: Reader b
    => M.Language -> SourceName -> [Char] -> ErrorT Description IO b
parseHelper lang what input = case parse (parse_complete reader) what input of
    Right res ->
        return res
    Left err -> do
        output <- liftIO $ fromOutput lang . Pre $ errmsg 80 err input
        throwError output
