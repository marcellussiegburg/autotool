module Graph.Iso.Central where

import Graph.Util
import Graph.Iso

import Autolib.Graph.Basic
import Autolib.Graph.Ops ( gmap )
import Autolib.Dot.Dotty 
import Autolib.Graph.Kneser ( petersen )

import Inter.Types
import Autolib.ToDoc
import Autolib.Size
import Autolib.Informed

import qualified Challenger as C

import qualified Data.Set as S
import qualified Data.Map as M

import Data.Typeable

data Isomorphie = Isomorphie deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Isomorphie where
    scoringOrder _ = Increasing

instance C.Partial Isomorphie ( Graph Int, Graph Int ) ( FiniteMap Int Int ) where

    report p (g,h) = do
        inform $ text "Gesucht ist eine Isomorphie zwischen diesen Graphen:"
        inform $ text "G" <+> equals <+> toDoc g ; peng_using Dot g
        inform $ text "H" <+> equals <+> toDoc h ; peng_using Dot h

    initial p (g,h) = listToFM $ zip ( lknoten g ) ( lknoten h )

    partial p (g,h) f = return ()
    
    total p (g,h) f = do
	check_iso f (informed (text "G") g) (informed (text "H") h)

permute :: Graph Int -> Graph Int
permute g = 
  let v = S.toList $ knoten g
      (p,q) = splitAt ( length v `div` 2 ) v
      m = M.fromList $ zip ( p ++ q) ( q ++ p )
  in  gmap ( m M.! ) g

make :: Make
make = direct Isomorphie ( petersen, permute petersen )




