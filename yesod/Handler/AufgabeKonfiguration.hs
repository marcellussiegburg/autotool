{-# LANGUAGE OverloadedStrings #-}
module Handler.AufgabeKonfiguration where

import Import
import Text.Blaze.Html

type Konfiguration = Text

getAufgabeKonfigurationR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
getAufgabeKonfigurationR server aufgabeTyp konfiguration = do
  let konfiguration' = Just $ Textarea "[ ( Pre\n  , [ e , j , b , i , f , m , l\n    , k , d , g , c , a , h ] )\n, ( In\n  , [ b , j , i , e , k , l , d\n    , m , g , f , a , c , h ] ) ]"
      typ = preEscapedToHtml ("[(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autotool-collection/Baum-Order.html#t:Order\"><tt>Order</tt></a>, [<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>])]" :: Text)
  (formWidget, formEnctype) <- generateFormPost $ konfigurationForm konfiguration'
  defaultLayout $ do
    $(widgetFile "aufgabeKonfiguration")

postAufgabeKonfigurationR :: ServerUrl -> AufgabeTyp -> AufgabeKonfiguration -> Handler Html
postAufgabeKonfigurationR server aufgabeTyp konfiguration = do
  let konfiguration' = Just $ Textarea "[ ( Pre\n  , [ e , j , b , i , f , m , l\n    , k , d , g , c , a , h ] )\n, ( In\n  , [ b , j , i , e , k , l , d\n    , m , g , f , a , c , h ] ) ]"
      typ = preEscapedToHtml ("[(<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autotool-collection/Baum-Order.html#t:Order\"><tt>Order</tt></a>, [<a href=\"http://autotool.imn.htwk-leipzig.de/docs/autolib-rewriting/Autolib-TES-Identifier.html#t:Identifier\"><tt>Identifier</tt></a>])]" :: Text)
  ((result, formWidget), formEnctype) <- runFormPost $ konfigurationForm konfiguration'
  defaultLayout $ do
    $(widgetFile "aufgabeKonfiguration")

konfigurationForm :: Maybe Textarea -> Form Textarea
konfigurationForm mkonfiguration = do
  renderBootstrap3 BootstrapBasicForm $
    areq textareaField (bfs MsgAufgabeKonfiguration) mkonfiguration
    <* bootstrapSubmit (BootstrapSubmit MsgAufgabeKonfigurieren "btn-success" [])
