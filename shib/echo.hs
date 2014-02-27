{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language TypeFamilies #-}

import Yesod

import Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze
import Text.Blaze.Html4.Strict hiding (map)

import Network.Wai

data This = This

mkYesod "This" [parseRoutes|
/         TopR GET
|]

instance Yesod This

getTopR :: Handler Html
getTopR = do
    req <- waiRequest
    defaultLayout [whamlet| 
    requestMethod = #{show $ requestMethod req} 
    requestHeaders = #{show $ requestHeaders req} 
    |]

main = warpDebug 3000 This


