module Handler.Sprache where

import Import

getSpracheR :: Text -> Text -> Handler Html
getSpracheR lang route = do
  setLanguage lang
  y <- getYesod
  setMessage $ toHtml $ renderMessage y [lang] MsgNeueSprache
  redirect (read $ unpack route :: Route Autotool)
