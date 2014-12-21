{-# LANGUAGE OverloadedStrings #-}
module Handler.Server where

import Import

data Baum a = Zweig {
  wurzel :: a,
  unterbäume :: [Baum a]
}

getServerR :: Serverurl -> Handler Html
getServerR server = do
  let aufgabenTypen =
        [Zweig "Terme, Ersetzungssysteme"
         [Zweig "Unifikation"
          [Zweig "Unify-Direct-1" []]
         ,Zweig "Termersetzung"
          [Zweig "Derive-For_TRS-Direct-1" []
          ,Zweig "Termination"
           [Zweig "Matrix-Interpretationen"
            [Zweig "Rewriting_Termination-Direct" []]
           ,Zweig "Polynom-Interpretationen"
            [Zweig "Rewriting_Termination-Direct-1" []]]
          ,Zweig "Completion-Direct-1" []]]
        ,Zweig "Mengen und Relationen"
         [Zweig "Algebraic_Set-Direct-1" []]] :: [Baum Text]
  defaultLayout $ do
    addStylesheet $ StaticR css_tree_css
    $(widgetFile "server")

unterbaum :: Baum Text -> Widget
unterbaum baum = do
  inputId <- newIdent
  $(widgetFile "baum")
