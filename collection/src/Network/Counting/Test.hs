-- | Thm 7.2: A balancing network with n balancers
-- is a counting network if it counts correctly 
-- in all sequential executions for \le 2^n  tokens.

module Network.Counting.Test where

import Network.Counting.Data
import Network.Counting.Semantics

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Hash

import System.Random
import qualified Data.Map as M
import Control.Monad ( forM )
import Control.Monad.State.Strict
import Data.Ix (range )

test :: Int -> Int 
     -> (Wire, Wire)
      -> Network 
      -> Reporter ()
test num top bnd @ (lo,hi) ( net @ (Network bs) ) = do
    let ms = evalState 
               ( replicateM num $ roll bnd top )
          $ mkStdGen $ fromIntegral 
          $ hash (num, top, net)
    let handle [] = inform $ vcat 
            [ toDoc num <+> text "Tests mit Histogramm-Elementen"
            , text "bis zur Größe" <+> toDoc top 
              <+> text "erfolgreich absolviert."
            ]
        handle (m:ms) = do
            let m' = bulk net m
                s = map ( \ w -> M.findWithDefault 0 w m' ) ( range bnd)
            when ( not $ step_property s ) $ do
               inform $ vcat 
                      [ text "zu dem Eingabe-Histogramm"
                      , nest 4 $ toDoc m
                      , text "gehört das Ausgabe-Histogramm"
                      , nest 4 $ toDoc m'
                      , text "Das ist keine Schrittfolge."
                      ]
               inform $ vcat 
                      [ text "Die folgende Rechnung des Netzes zeigt eine falsche Ausgabe:"
                      ]
               singles bnd net $ tokens m
               reject empty
            handle ms
    handle ms

tokens m = do
    (k,v) <- M.toList m
    replicate v k

step_property :: [ Int ] -> Bool
step_property xs = null xs ||
    and ( zipWith (>= ) xs ( tail xs ++ [head xs - 1]))

roll :: (Wire,Wire) -> Int 
     -> State StdGen ( M.Map Wire Int )
roll bnd top = do
    ns <- forM ( range bnd ) $ \ w -> randS (0, top) 
    return $ M.fromList $ zip ( range bnd ) ns

randS bnd = do
    g <- get
    let (x,g') = randomR bnd g
    put g'
    return x
