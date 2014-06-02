module SOS.Evo where

import SOS.State
import SOS.Plain

import qualified Autolib.Genetic as G
import Autolib.Reporter
import Autolib.ToDoc

import qualified Data.Map as M
import qualified Data.Set as S

import System.Random

import Control.Parallel.Strategies 
       (parMap, rseq, parListChunk, withStrategy)

gee n =  G.evolve $ conf n

-- | number of states that are handled in the wrong way
-- (target evaluation is 0 = no errors)
evaluate :: Int -> Program -> Int
evaluate n p = sum $ forP (states n) $ \  s ->
    maybe 1 (const 0) ( result $ work step p S.empty s ) 

crosso :: Program -> Program -> IO Program
crosso (Program p) (Program q) = do
    let xs = M.toList p
        ys = M.toList q
    i <- randomRIO (0, length xs - 1)
    return $ Program $ M.fromList $ take i xs ++ drop i ys

muto :: Program -> IO Program
muto (Program p) = do
    let xs = M.toList p
    i <- randomRIO (0, length xs - 1)
    let (k, Move (f,t)) = xs !! i
    f' <- randomRIO (0,2)
    t' <- randomRIO (0,2)
    return $ Program $ M.insert k (Move (f',t')) p

for = flip map
forP xs f = withStrategy ( parListChunk 8 rseq ) $ map f xs


conf :: Int -> G.Config Program Int 
conf n = G.Config 
     { G.fitness = negate . evaluate n
          , G.threshold = 0
          , G.present = print . toDoc
          , G.trace = print . toDoc . map fst . take 20
          , G.size = 100
          , G.generate = some n
          , G.combine = crosso
          , G.num_combine = 100
          , G.mutate = muto >=> muto
          , G.num_mutate = 100
          , G.num_compact = 100
          , G.num_steps= Nothing
          , G.num_parallel = 1
          , G.scheme = G.Global
          }
