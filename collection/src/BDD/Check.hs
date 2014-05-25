module BDD.Check where

import BDD.Data

import Autolib.ToDoc
import Autolib.Reporter

import qualified Data.Map.Strict as M
import Control.Monad ( forM_ )

check order bdd = do
    fm <- check_addresses bdd
    check_references fm
    check_acyclic fm
    check_ordered order fm
    check_reduced fm

check_addresses (BDD table) = do
    let wrong = do 
           (i, (a,n)) <- zip [Address 0 ..] table
           guard $ i /= a
           return (i, (a, n))
    when (not $ null wrong ) $ reject 
         $ text "Als Adressen sollen [0, 1 .. ] verwendet werden, Fehler:" </> toDoc wrong
    return $ M.fromList table

check_references fm = forM_ ( M.toList fm ) $ \ (a,n) -> case n of
    Leaf {} -> return ()
    Branch l v r -> forM_ [ l,r ] $ \ c -> case M.lookup c fm of
        Nothing -> reject $ 
            text "Dieser Knoten enthält einen falschen Verweis:" </> toDoc (a,n)
        _ -> return ()

check_acyclic fm = forM_ ( M.toList fm ) $ \ (a,n) -> case n of
    Leaf {} -> return ()
    Branch l v r -> when (not (a < l && a < r)) $ reject
        $ text "Der Graph soll azyklisch sein, Fehler:" </> toDoc (a,n)

check_ordered order fm = do
    let vm = M.fromList $ zip order [1..]
    forM_ ( M.toList fm ) $ \ (a,n) -> case n of
        Leaf {} -> return ()
        Branch l v r -> case M.lookup v vm of
            Nothing -> reject 
                $ text "Knoten benutzt nicht deklarierte Variable" </> toDoc (a,n)
    forM_ ( M.toList fm ) $ \ (a,n) -> case n of
        Leaf {} -> return ()
        Branch l v r -> forM_ [l,r] $ \ a' -> case fm M.! a' of
            Leaf {} -> return ()
            n' @ (Branch _ w _) -> do
                when (not (vm M.! v < vm M.! w)) $ reject $
                    text "Das Diagramm soll geordnet sein, Fehler:" 
                        </> toDoc [ (a,n), (a',n') ]

check_reduced fm = do
    check_no_equal_children fm
    check_no_equal_nodes fm

check_no_equal_children fm = forM_ ( M.toList fm ) $ \ (a,n) -> case n of
    Leaf {} -> return ()
    Branch l v r -> when (l == r) $ reject
        $ text "Das Diagramm soll reduziert sein, aber dieser Knoten hat identische Kinder:" </> toDoc (a,n)

check_no_equal_nodes fm = do
    let wrong = filter ( \ (n,as) -> 1 < length as )
              $ M.toList
              $ M.fromListWith (++) $ do
                   (a,n) <- M.toList fm ; return (n, [a])
    when (not $ null wrong) $ reject 
        $ text "Das Diagramm soll reduziert sein, aber diese Knoten kommen mehrfach vor:" </> vcat ( map  toDoc wrong )
