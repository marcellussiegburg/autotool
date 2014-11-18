{-# language PatternSignatures #-}
{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

import Control.Types
import qualified Control.Aufgabe as A
import qualified Control.Aufgabe.DB

import Autolib.ToDoc

import Network.CGI
import Text.XML
import Text.Hamlet.XML
import qualified Data.Map as M
-- import Text.XML.Cursor
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BS
import Data.String

main :: IO ()
main = runCGI $ do
    Just (a :: Int) <- readInput "task-config"
    -- [ auf ] <- liftIO $ Control.Aufgabe.DB.get_this $ ANr a

    let auf = take 10 [ 1 :: Int .. ]
        foo = "foo"
        s = show $ toDoc auf
    
    let root = Element "autotoolnode" M.empty [xml|
    <task type_name="#{foo}" type_scoring="i">
    <tasconfiguration is_altered="false">
    <auth_comment>
    <conf_text>#{fromString s}
    <doc_text>what
    <signature>#{"12334"}
    <description>what
    <configuration>
        <scorepoints>20,15,10,7,6,5,4,3,2,1
    <server_conn name="what" version="0.8.0">
|]

    let doc = Document (Prologue [] Nothing []) root []
        ent = Zip.toEntry ("task-config/" ++ show a) 0 $ renderLBS def doc
        arch = Zip.addEntryToArchive ent $ Zip.emptyArchive

    setHeader "Content-type" "application/x-zip-compressed"
    outputFPS $ Zip.fromArchive arch
