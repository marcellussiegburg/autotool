module Operate.Motd where

--   $Id$

-- import Text.XHtml hiding ( text  )
import Text.Blaze.Html 
import Text.Blaze.Html5
import qualified Control.Exception as CE

contents :: IO Html
contents = motd location


location :: FilePath
location = "/home/autotool/autotool/config/motd.html"

motd :: FilePath -> IO Html
motd file = do
    contents <- readFile file `CE.catch` \ (CE.SomeException err) -> return ""
    return $ Text.Blaze.Html5.p $ toHtml contents

