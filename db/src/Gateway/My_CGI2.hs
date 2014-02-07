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


-- | same type as official function
wrapper :: ([(String,String)] -> IO Text.Blaze.Html.Html) -> IO ()
wrapper f = runCGI $ do
    e <- getInputs
    a <- lift $ f $ e
    outputFPS $ Text.Blaze.Html.Renderer.Utf8.renderHtml a

