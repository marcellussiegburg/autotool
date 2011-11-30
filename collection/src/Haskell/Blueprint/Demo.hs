{-# language PatternSignatures #-}

import qualified Mueval.Interpreter
import qualified Mueval.ArgsParse as M
import qualified Language.Haskell.Interpreter as I
import qualified Control.Exception

main = do
    r <- ( I.runInterpreter $ Mueval.Interpreter.interpreter $ M.Options
                    { M.timeLimit = 1
                    , M.modules = Just [ "Prelude" ]
                    , M.expression = "undefined"
                    , M.loadFile = "" -- meaning "no file" ??
                    , M.user = "" -- WHO ??
                    , M.printType = False -- printed to where?
                    , M.extensions = False
                    , M.namedExtensions = []
                    , M.noImports = False
                    , M.rLimits = True
                    } ) `Control.Exception.catch` \ ( e :: Control.Exception.SomeException ) -> do
                        return $ Left $ I.UnknownError ( show e )
    ( print r ) `Control.Exception.catch` \ ( e :: Control.Exception.SomeException ) -> do
        putStrLn $ "caught " ++ show e
        
