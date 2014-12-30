{-# LANGUAGE OverloadedStrings #-}
module Handler.AufgabeVorlage where

import Import

getAufgabeVorlageR :: ServerUrl -> AufgabeTyp -> VorlageName -> Handler Html
getAufgabeVorlageR server aufgabeTyp vorlageName = do
  let konfiguration = "[ ( Pre\n  , [ e , j , b , i , f , m , l\n    , k , d , g , c , a , h ] )\n, ( In\n  , [ b , j , i , e , k , l , d\n    , m , g , f , a , c , h ] ) ]"
  redirect $ AufgabeKonfigurationR server aufgabeTyp konfiguration
