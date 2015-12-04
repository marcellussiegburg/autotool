module Handler.VorlesungenSortiert where

import Import

import Control.Monad (when)
import Control.Types (TimeStatus (Early, Late, Current))
import Data.List (nub)
import Data.Time (getCurrentTime)

import Handler.Vorlesungen (autorisiertVorlesung, newLink, statusClass)

headersAsc :: [SelectOpt Vorlesung]
headersAsc = [Asc VorlesungName, Asc VorlesungVon, Asc VorlesungBis, Asc VorlesungNachricht]

headersDesc :: [SelectOpt Vorlesung]
headersDesc = [Desc VorlesungName, Desc VorlesungVon, Desc VorlesungBis, Desc VorlesungNachricht]

getVorlesungenSortiertR :: SemesterId -> [Int] -> Handler Html
getVorlesungenSortiertR semester ints = do
  let abss = map abs ints
  when (length ints /= length (nub abss)) notFound
  when (minimum abss < 1 || maximum abss > length headersAsc) notFound
  let sort = map (\i -> if i > 0 then headersAsc !! (abs i - 1) else headersDesc !! (abs i - 1)) ints
  vorlesungen <- runDB $ selectList [VorlesungSemesterId ==. semester] sort
  mid <- maybeAuthId
  vorlesungenAutorisiert' <- mapM (autorisiertVorlesung mid) vorlesungen
  let vorlesungenAutorisiert = concat vorlesungenAutorisiert'
  zeit <- liftIO getCurrentTime
  defaultLayout $
    $(widgetFile "vorlesungen")
