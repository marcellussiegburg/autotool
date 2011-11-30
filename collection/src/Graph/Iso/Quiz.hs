{-# language TemplateHaskell, DeriveDataTypeable #-}

module Graph.Iso.Quiz where

import Graph.Iso.Central
import Graph.Degree ( roll )

import Autolib.Graph.Type
import Autolib.Graph.Ops ( gmap )
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Util.Zufall ( permutation )

import qualified Data.Map as M
import qualified Data.Set as S

import Inter.Quiz
import Inter.Types
import Data.Typeable

data Config = Config
    { nodes :: Int
    , degree :: Int  
    }  
    deriving ( Typeable )
             
$(derives [makeReader, makeToDoc] [''Config])             

conf :: Config
conf = Config { nodes = 8, degree = 3 }

instance Generator Isomorphie Config ( Graph Int, Graph Int ) where 
    generator p conf key = do
        g <- Graph.Degree.roll $ zip [ 1 .. nodes conf ] $ repeat ( degree conf )
        let vs = S.toList $ knoten g
        ws <- permutation vs
        let m = M.fromList $ zip vs ws
        let h = gmap (m M.!) g    
        return ( g, h )    
        
instance Project Isomorphie ( Graph Int, Graph Int ) ( Graph Int, Graph Int ) where        
      project p (g,h) = (g,h)
      
make :: Make
make = quiz Isomorphie conf
