module Yesod.Form.Fields.PreField (preField) where

import Control.Monad (Monad, return)
import Data.Either (Either (Right))
import Data.Function (($))
import Data.Maybe (Maybe (Just))
import Text.Blaze.Html (ToMarkup)
import Text.Shakespeare.I18N (RenderMessage)
import Yesod.Core (HandlerSite)
import Yesod.Core.Widget (whamlet)
import Yesod.Form.Fields (FormMessage)
import Yesod.Form.Types (Field (Field, fieldParse, fieldView, fieldEnctype), Enctype (UrlEncoded))

-- | Creates an HTML-pre-element which simply prints the value assigned to it.
-- 
-- Note: This field is no classical form element. It is only used to display information.
preField :: Monad m => RenderMessage (HandlerSite m) FormMessage => ToMarkup t => t -> Field m ()
preField content = Field {
    fieldParse = \ _ _ -> return $ Right $ Just (),
    fieldView = \idAttr nameAttr otherAttrs _ _ -> do
      [whamlet|
      <pre ##{idAttr} name=#{nameAttr} *{otherAttrs}>
        #{content}
      |],
    fieldEnctype = UrlEncoded
}
