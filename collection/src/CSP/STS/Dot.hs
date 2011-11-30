module CSP.STS.Dot

( module Autolib.Dot.Dot )

where

import CSP.STS.Type

import Autolib.Dot.Dot
import qualified Autolib.Dot.Graph as G
import qualified Autolib.Dot.Node  as N
import qualified Autolib.Dot.Edge  as E

import qualified Data.Map as M
import qualified Data.Set as S

instance ( Show s, Show t, Ord s, Ord t ) 
         => ToDot ( STS s t ) where
    toDot n = helper n
    toDotProgram a = Dot
    toDotOptions a = "-Grankdir=LR"

helper s = 
    let ss = S.toList $ states s
        placemap = M.fromList 
            $ zip ( Nothing : map Just ss ) 
            $ map show [ 0 .. ]
        oneline = unwords . words
        
        tricky cs = 
                if take 1 cs `elem` [ "\"", "'" ]  
                   -- dann ist es Show String|Char
                then tail ( init cs )     
                     -- und eine "-klammer kann weg
                else cs
        quoted cs = "\"" ++ cs ++ "\""
        
        nodes = do
            ( p, q ) <- M.toList placemap       
            return $ case p of 
                Just p -> N.blank
                   { N.ident = q
                   , N.label = Just $ oneline $ show p 
                   , N.shape = Just "ellipse"
                   }
                Nothing -> N.blank   
                   { N.ident = q
                   , N.node_style = Just "invis"
                   }
        real_edges = do
            ( vor, t, nach ) <- visible s
            return $ E.blank
                   { E.from = placemap M.! Just vor
                   , E.to = placemap M.! Just nach
                   , E.taillabel = 
                       Just $ quoted $ tricky $ show t
                   }
        tau_edges = do
            ( vor, nach ) <- hidden s
            return $ E.blank
                   { E.from = placemap M.! Just vor
                   , E.to = placemap M.! Just nach
                   }
        start_edge = E.blank           
                   { E.from = placemap M.! Nothing
                   , E.to = placemap M.! Just (start s)
                   }         
    in  G.Type
        { G.directed = True
        , G.name = "STS"
        , G.nodes = nodes 
        , G.edges = start_edge : real_edges ++ tau_edges
        , G.attributes = []
        }