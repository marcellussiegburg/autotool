{-# LANGUAGE OverloadedStrings #-}
module Handler.Server where

import Import

getServerR :: ServerUrl -> Handler Html
getServerR server = do
  let aufgabenTypen =
        [Node "Terme, Ersetzungssysteme"
         [Node "Unifikation"
          [Node "Unify-Direct-1" []]
         ,Node "Termersetzung"
          [Node "Derive-For_TRS-Direct-1" []
          ,Node "Termination"
           [Node "Matrix-Interpretationen"
            [Node "Rewriting_Termination-Direct" []]
           ,Node "Polynom-Interpretationen"
            [Node "Rewriting_Termination-Direct-1" []]]
          ,Node "Completion-Direct-1" []]]
        ,Node "Mengen und Relationen"
         [Node "Algebraic_Set-Direct-1" []]] :: Forest Text
  defaultLayout $ do
    addStylesheet $ StaticR css_tree_css
    $(widgetFile "server")

unterbaum :: ServerUrl -> Tree Text -> Maybe Text -> Widget
unterbaum server baum mName = do
  inputId <- newIdent
  $(widgetFile "baum")
