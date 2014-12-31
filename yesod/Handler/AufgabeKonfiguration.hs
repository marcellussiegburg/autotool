{-# LANGUAGE OverloadedStrings #-}
module Handler.AufgabeKonfiguration where

import Import

type Konfiguration = Text

getAufgabeKonfigurationR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeKonfigurationR server aufgabeTyp konfiguration = do
  let konfiguration' = Just $ Textarea konfiguration
      typ = preEscapedToHtml ("[(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autotool-collection/Baum-Order.html#t:Order\"><tt>Order</tt></a>, [<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>])]" :: Text)
  (formWidget, formEnctype) <- generateFormPost $ konfigurationForm konfiguration'
  defaultLayout $ do
    $(widgetFile "aufgabeKonfiguration")

postAufgabeKonfigurationR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeKonfigurationR server aufgabeTyp konfiguration = do
  let konfiguration' = Just $ Textarea konfiguration
      typ = preEscapedToHtml ("[(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autotool-collection/Baum-Order.html#t:Order\"><tt>Order</tt></a>, [<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>])]" :: Text)
  ((result, formWidget), formEnctype) <- runFormPost $ konfigurationForm konfiguration'
  defaultLayout $ do
    $(widgetFile "aufgabeKonfiguration")

konfigurationForm :: Maybe Textarea -> Form Textarea
konfigurationForm mkonfiguration = do
  renderBootstrap3 BootstrapBasicForm $
    areq textareaField (bfs MsgAufgabeKonfiguration) mkonfiguration
    <* bootstrapSubmit (BootstrapSubmit MsgAufgabeKonfigurieren "btn-success" [])
