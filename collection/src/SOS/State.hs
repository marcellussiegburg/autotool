{-# language TemplateHaskell #-}
{-# language DoAndIfThenElse #-}

module SOS.State where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Util.Splits

import qualified Data.Set as S

newtype Observe = Observe [ Maybe Int ]   deriving ( Eq, Ord )

newtype State = State [ [Int] ]  deriving ( Eq, Ord )

derives [makeReader, makeToDoc] [''Observe,''State ]


instance Show Observe where show = render . toDoc
instance Show State where show = render . toDoc

observe :: State -> Observe
observe (State xss) = Observe $ map ( \ xs -> case xs of
    [] -> Nothing ; x : xs' -> Just x ) xss

final (State xss) = case xss of
    xs : xss' -> and (zipWith (==) [1 ..] xs) && all null xss'
    [] -> False 

states :: Int -> [State]
states n = do
    xs <- permutations [1..n]
    xss <- partitions 3 xs
    return $ State xss

permutations [] = [[]]
permutations (x:xs) = do
    p <- permutations xs
    (pre, post) <- splits p
    return (pre ++ x : post)

partitions k xs = 
    if k > 1 then do
        (pre,post) <- splits xs
        ps <- partitions (k-1) post
        return $ pre : ps
    else return [xs]

work :: ( p -> State -> Reporter State )
     -> p -> S.Set State -> State -> Reporter ()
work step p done s = do
    inform $ toDoc s
    if final s then inform $ text "is final"
    else if S.member s done then reject $ 
        text "this starts a loop (over non-final states)"
    else do
        s' <- step p s
        work step p (S.insert s done) s'
