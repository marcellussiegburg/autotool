{-# LANGUAGE OverloadedStrings #-}
module Handler.AufgabeKonfiguration where

import Import
import Yesod.Form.Fields.PreField (preField)

type Konfiguration = Text

getAufgabeKonfigurationR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeKonfigurationR = postAufgabeKonfigurationR

postAufgabeKonfigurationR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeKonfigurationR server aufgabeTyp konfiguration = do
  let typ = preEscapedToHtml ("[(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autotool-collection/Baum-Order.html#t:Order\"><tt>Order</tt></a>, [<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>])]" :: Text)
  ((result, formWidget), formEnctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ konfigurationForm typ $ Just konfiguration
  defaultLayout $ do
    $(widgetFile "aufgabeKonfiguration")

konfigurationForm :: ToMarkup t => t -> Maybe AufgabeKonfiguration -> AForm Handler AufgabeKonfiguration
konfigurationForm typ mkonfiguration = do
    aopt (preField typ) (bfs MsgAufgabeKonfigurationTyp) {fsAttrs = []} Nothing
    *> (unTextarea <$> areq textareaField (bfs MsgAufgabeKonfiguration) (fmap Textarea mkonfiguration))
    <* bootstrapSubmit (BootstrapSubmit MsgAufgabeKonfigurieren "btn-success" [])
