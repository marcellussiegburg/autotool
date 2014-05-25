module BDD.Semantics where

import BDD.Data

import qualified OBDD as O

import Control.Monad.State.Strict as S

import qualified Data.Map.Strict as M

value :: Ord v => BDD v -> O.OBDD v
value bdd = flip S.execState M.empty ( evaluate bdd ) M.! Address 0

evaluate (BDD table) = forM_ (reverse table) node

node (a, n) = do
    fm <- S.get
    let v = case n of
            Leaf b -> O.constant b
            Branch l v r -> (O.unit v False O.&& ( fm M.! l ))
                       O.|| (O.unit v True  O.&& ( fm M.! r ))
    S.put $ M.insert a v fm

