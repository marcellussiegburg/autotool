module Handler.Vorlesungen where

import Import

import Handler.Semesters (listGroupItemClass)

import Control.SQL (equals, reed, toEx)
import qualified Control.Vorlesung as Vorlesung
import Control.Types
import Autolib.Util.Sort (sortBy)

getVorlesungenR :: SemesterId -> Handler Html
getVorlesungenR semester = do
  vorlesungen <- lift $ get_at_semester $ ENr semester
  let vorlesungenSortiert = reverse $ sortBy Vorlesung.einschreibVon vorlesungen
  mid <- maybeAuthId
  vorlesungenAutorisiert' <- lift $ mapM (autorisiertVorlesung mid) vorlesungenSortiert
  let vorlesungenAutorisiert = concat vorlesungenAutorisiert'
      vname v = let Name n = Vorlesung.name v
                in n
      vmotd v = let Name m = Vorlesung.motd v
                in m
  defaultLayout $ do
    $(widgetFile "vorlesungen")

autorisiertVorlesung :: Maybe (AuthId Autotool) -> Vorlesung.Vorlesung -> IO [(Vorlesung.Vorlesung, Maybe (Route Autotool), Maybe (Route Autotool))]
autorisiertVorlesung mid vorlesung = do
  let VNr v = Vorlesung.vnr vorlesung
      vorlesungRoute = GruppenR v
      bearbeitenRoute = VorlesungR v
      ist (Just True) route = Just route
      ist _ _ = Nothing
  autorisiertS <- istAutorisiert mid vorlesungRoute
  autorisiertB <- istAutorisiert mid bearbeitenRoute
  if autorisiertS == Just True || autorisiertB == Just True
     then return [(vorlesung
                  ,ist autorisiertS vorlesungRoute
                  ,ist autorisiertB bearbeitenRoute)]
     else return []

get_at_semester :: ENr -> IO [ Vorlesung.Vorlesung ]
get_at_semester enr =
    Vorlesung.get_from_where (map reed ["vorlesung"])
           (equals (reed "vorlesung.ENr") (toEx enr))
