module Graph.Degree where

import Autolib.Graph.Graph
import Autolib.Graph.Ops ( links )
import Autolib.Graph.Basic ( independent )
import Autolib.Graph.Display
import Autolib.Dot.Dot
import Autolib.Size

import Autolib.Util.Zufall
import Autolib.Util.Splits

import Data.List ( sort )
import Control.Monad ( guard )

import Data.Array

import qualified OBDD as O
import qualified Data.Map as M

-- | create a graph that realizes the given degree sequence.
-- WARNING: this method of generation is feasible
-- for up to 10 nodes roughly.
roll :: GraphC v => [(v,Int)] -> IO ( Graph v ) 
roll xs = do
    Just m <- realize xs 
    let g = mkGraph ( mkSet $ map fst xs ) 
           $ mkSet $ do (k,True) <- M.toList m ; return k
    return $ g { layout_program = Dot }                    

realize :: GraphC v => [ (v,Int) ] -> IO (Maybe (M.Map (Kante v) Bool))
realize xs = O.some_model $ O.and $ do
    ( pre, (x,d) : post ) <- splits xs
    return $ exactly d $ do
        (y,e) <- pre ++ post
        return $ kante x y
    
exactly :: Ord v => Int -> [v] -> O.OBDD v
exactly c vs =
  let bnd = ((-1,0), (c,length vs))
      -- a!(c,i) <=> exactly c of (take i vs) are true
      a = array bnd  $ do 
          (c,i) <- range bnd
          return ((c,i), if c < 0 then O.constant False else
             if i == 0 then O.constant ( c == 0 )
             else O.or [ O.unit (vs!!(i-1)) True  O.&& ( a!(c-1,i-1)  )
                            , O.unit (vs!!(i-1)) False O.&& ( a!(c  ,i-1) )
                            ]  )
  in  a!(c, length vs) 

{-
d1 , . . . , dn is the degree sequence of a simple graph on n vertices 
if and only if  their sum is even and for each 1 ≤ k ≤ n,
k(k − 1) + sum_{k+1}^n min{d_i,k} − sum_1^k d_i \ge 0
-}

erdosgallai :: [ Int ] -> Bool
erdosgallai xs = even ( sum xs )
        && all ( \ k ->
              let ( pre, post ) = splitAt k $ reverse $ sort xs
              in  k * (k-1) + sum ( map (min k) post ) - sum pre >= 0
          ) [ 1 .. length xs ]
                 
      