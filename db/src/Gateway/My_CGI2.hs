-- | wrapper for the system library

module Gateway.My_CGI2

( wrapper
)

--  $Id$

where

import Network.CGI hiding ( wrapper )
import Control.Monad.Trans

import qualified Text.Blaze.Html
import qualified Text.Blaze.Html.Renderer.Utf8
import System.IO

wrapper :: ([(String,String)] -> [(String,String)] -> IO Text.Blaze.Html.Html) -> IO ()
wrapper f = runCGI $ do
    setHeader "Content-Type" "text/html; charset=UTF-8"
    vs <- getVars
    is <- getInputs
    a <- lift $ f vs is
    outputFPS $ Text.Blaze.Html.Renderer.Utf8.renderHtml a

