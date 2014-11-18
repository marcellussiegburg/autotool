{-# language PatternSignatures #-}
{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

import Control.Types
import qualified Control.Aufgabe as A
import qualified Control.Aufgabe.DB

import Autolib.ToDoc

import Network.URL
import System.FilePath
import Network.CGI
import Text.XML
import Text.Hamlet.XML
import qualified Data.Map as M
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy as BS
import Data.String
import Control.Monad ( when )

main :: IO ()
main = runCGI $ do
    Just (a :: Int) <- readInput "problem"
    [ auf ] <- liftIO $ Control.Aufgabe.DB.get_this $ ANr a
    when ( A.timeStatus auf == Early ) $ error "too early"
    
    -- default output method for all components, exceptions see below
    let out x = fromString $ toString x
                   
    let Just u = importURL $ toString $ A.server auf
        p = url_path u
        (dir,file) = splitFileName p
        (name,ext) = splitExtension file
        version = reverse $ takeWhile ( /= '-' ) $ reverse name
    
    let scoring = case A.highscore auf of
            Keine -> "?" ; High -> "i" ; Low -> "d"
    
    let root = Element "autotoolnode" M.empty [xml|
    <tasktype type_name="#{out $ A.typ auf}" type_scoring="#{scoring}">
    <taskconfiguration is_altered="false">
    <auth_comment>
    <conf_text>#{out $ A.config auf}
    <doc_text>(missing)
    <signature>#{out $ A.signature auf}
    <description>#{out $ A.remark auf}
    <configuration>
        <scorepoints>20,15,10,7,6,5,4,3,2,1
    <server_conn name="Autotool Server HTWK Leipzig" version="#{fromString version}" url="#{out $ A.server auf}">
|]

    -- FIXME: server_conn should have this url attribute (but currently hasn't)

    let doc = Document (Prologue [] Nothing []) root []
        ent = Zip.toEntry ("task-config/" ++ show a) 0 $ renderLBS def doc
        arch = Zip.addEntryToArchive ent $ Zip.emptyArchive

    setHeader "Content-type" "application/x-zip-compressed"
    outputFPS $ Zip.fromArchive arch
