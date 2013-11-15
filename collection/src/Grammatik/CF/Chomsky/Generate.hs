module Grammatik.CF.Chomsky.Generate where

import Grammatik.CF.Chomsky.Type

import Autolib.FiniteMap

import qualified Data.Map as M

import Control.Monad ( guard )
import Data.List ( nub )
import Data.Maybe (maybeToList)

main :: Ord a
     => Chomsky a -> [[ String ]]
main ch = let Just ws = lookupFM ( creation ch ) ( start ch ) in ws

-- | compute terminal words derivable from symbol
-- collect words of equal length into groups:
-- lookupFM ( creation ch ) v !! k = list of all words derivable
-- from variable v of length k
creation :: Ord a
         => Chomsky a -> FiniteMap a [[String]]
creation ch = 
    let sigma = nub $ map fst $ rules ch
        vrules = M.fromListWith (++) $ do (l,r) <- rules ch ; return (l, [r])
        fm = listToFM $ do
               v <- sigma
               let words k = nub $ 
                     if k <= 0 then [ "" | v == start ch && eps ch ]
                     else if k == 1 then do
                         Left c <- M.findWithDefault [] v vrules
                         return [ c ]
                     else do
                         Right (x,y) <- M.findWithDefault [] v vrules
                         l <- [ 1 .. k -1 ]
                         let r = k - l
                         let Just wls = lookupFM fm x
                             Just wrs = lookupFM fm y
                         guard $ not (null $ wls !! l) && not (null $ wrs !! r)
                         wl <- wls !! l
                         wr <- wrs !! r
                         return $ wl ++ wr
               return ( v, map words [ 0 .. ] ) 
    in  fm
