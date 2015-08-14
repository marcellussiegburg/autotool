{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
{-# language MultiParamTypeClasses #-}
{-# language PatternSignatures #-}
{-# language OverlappingInstances #-}

module Haskell.Blueprint.Central where

import Debug ( debug )

import Haskell.Blueprint.Data
import Haskell.Blueprint.Match 

import qualified Language.Haskell.Exts as E
import qualified Language.Haskell.Exts.Parser as P
-- import qualified Language.Haskell.Syntax as S
import qualified Language.Haskell.Exts.SrcLoc as S
import Data.Generics.Schemes (gtypecount)

import qualified Mueval.ArgsParse as M
import qualified Mueval.Interpreter
import qualified Language.Haskell.Interpreter as I
import qualified Mueval.Context

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Reporter.IO.Type
import qualified Autolib.Reporter as R
import Inter.Types
import Inter.Quiz
import Data.Typeable

import qualified Control.Exception
import Control.Monad.IO.Class
import Test.SmallCheck
import System.IO.Temp
-- import System.IO.UTF8
import System.Random ( randomRIO )
-- import qualified System.IO.Strict
import qualified System.IO
import qualified System.Directory
import qualified System.Posix.Directory as SPD

data Haskell_Blueprint = Haskell_Blueprint deriving Typeable

derives [makeReader, makeToDoc] [''Haskell_Blueprint]

instance Show Haskell_Blueprint where show = render . toDoc

instance OrderScore Haskell_Blueprint where
    scoringOrder h = Increasing

instance Verify Haskell_Blueprint Code where
    verify _ ( Code i ) = do
        Haskell.Blueprint.Central.parse i
        return ()

-- we measure the difference in size
instance Measure Haskell_Blueprint Code Code where
    measure p i b = fromIntegral $ size b - size i



instance Partial Haskell_Blueprint Code Code where
    describe p i = vcat
        [ text "Vervollständigen Sie das Haskell-Programm."
        , text "Ersetzen Sie jedes 'undefined',"
        , text "so daß der Ausdruck \'test\' den Wert True hat."
        , nest 4 $ toDoc i
        ]
    initial p i = i

    partial p ( Code i ) ( Code b ) = do
        mi <- Haskell.Blueprint.Central.parse i
        mb <- Haskell.Blueprint.Central.parse b
        R.inform $ text "paßt Ihr Quelltext zum Muster?"
        case Haskell.Blueprint.Match.test mi mb of
            Fail loc ->  reject_parse b loc "Nein"
            Haskell.Blueprint.Match.Ok _ -> R.inform $ text "Ja."

    totalIO p (Code i) (Code b) = do
        r <- liftIO $ run_total i b
        -- liftIO $ debug "outside runInterpreter"
        case r of
            Left err -> reject $ text $ show err
            Right ( e, et, val ) -> do
                inform $ vcat
                       [ text "expression" </> text e
                       , text "type" </> text et
                       ]  
                assert ( et == "Bool" ) $ text "richtiger Typ?"
              
                v <- liftIO  ( ( do Control.Exception.evaluate val ; return ( Right val ) )
                  `Control.Exception.catch` \ ( e :: Control.Exception.SomeException ) -> do
                        return $ Left ( show e ) )
                
                case v of
                     Right val -> assert ( val == "True" ) $ text "richtiger Wert?"
                     Left ex -> reject $ text "Exception" </> text ex

run_total_opts f = 
            let Right opts0 = M.interpreterOpts []
            in  opts0
                    { M.timeLimit = 10 -- seconds?
                    , M.modules = Just [ ]
		    , M.loadFile = f 
                    , M.expression = "Blueprint.test"
-- http://httpd.apache.org/docs/1.3/misc/FAQ-F.html#premature-script-headers 
-- Another cause for the "premature end of script headers" message 
-- are the RLimitCPU and RLimitMEM directives. 
-- You may get the message if the CGI script was killed due to a resource limit.
                    , M.rLimits = False
                    } 

run_total i b = 
  withTempDirectory "/tmp" "Blue" $ \ d -> 
      ( do
            let f = d ++ "/" ++ "Blueprint.hs"
            debug $ unwords 
                  [ "Blueprint tmpfile is", f ]
            System.IO.writeFile f b 

            keepCurrentDir $ do
                System.Directory.setCurrentDirectory d
                I.runInterpreter $ Mueval.Interpreter.interpreter ( run_total_opts f )
      ) `Control.Exception.catch` \ ( e :: Control.Exception.SomeException ) -> do
                        debug $ "interpreter got exception " ++ show e
                        return $ Left $ I.UnknownError ( show e ) 
            -- debug $ "after runInterpreter"
            -- length ( show r ) `seq` 
            -- return r
            -- System.Directory.removeFile f
            -- return r


-- | this is necessary because mueval 
-- changes currentDir without resetting
keepCurrentDir action = Control.Exception.bracket
    System.Directory.getCurrentDirectory
    System.Directory.setCurrentDirectory
    $ \ d -> action

make_fixed = direct Haskell_Blueprint code_example

parse m = case E.readExtensions m of
    Nothing -> R.reject $ text "cannot parse LANGUAGE pragmas at top of file"
    Just (lang, exts) -> 
        let pamo = P.defaultParseMode 
                   { P.extensions = exts }
        in  case P.parseModuleWithMode pamo m of
            P.ParseOk a -> return a
            P.ParseFailed loc msg -> 
                reject_parse m loc msg

reject_parse m loc msg =
    let ( lpre, lpost ) = 
            splitAt ( S.srcLine loc  ) $ lines m
        lpre' = reverse $ take 3 $ reverse lpre
        tag = replicate ( S.srcColumn loc - 1 ) '.' ++ "^"
    in  R.reject $ vcat ( map text lpre' ++ [ text tag, text msg ] )

-- this is the size of the syntax tree.
-- TODO: count just the nodes that are visible.
-- otherwise, it's not understandable for the student.
instance Size Code where
    size ( Code cs ) = case R.result $ Haskell.Blueprint.Central.parse cs of
        Just m -> gtypecount ( undefined :: E.Exp ) m
        _ -> 0

