module Handler.SchuleAnlegen where

import Import
import Handler.Schule (schuleForm)

getSchuleAnlegenR :: Handler Html
getSchuleAnlegenR = do
  (formWidget, formEnctype) <- generateFormPost $ schuleForm Nothing
  defaultLayout $ do
    $(widgetFile "schuleAnlegen")

postSchuleAnlegenR :: Handler Html
postSchuleAnlegenR = do
  ((result, formWidget), formEnctype) <- runFormPost $ schuleForm Nothing
  defaultLayout $ do
    $(widgetFile "schuleAnlegen")
