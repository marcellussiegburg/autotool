module Data.Tuple6 where

import Data.List ((!!), length)
import Data.Maybe (Maybe (Just, Nothing))
import Text.Show (Show)

data Tuple6 a b c d e f =
    Tuple6_0 |
    Tuple6_1 a |
    Tuple6_2 (a, b) |
    Tuple6_3 (a, b, c) |
    Tuple6_4 (a, b, c, d) |
    Tuple6_5 (a, b, c, d, e) |
    Tuple6_6 (a, b, c, d, e, f) deriving Show

toTuple6 :: (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f) -> Tuple6 a b c d e f
toTuple6 t = case t of
  (Nothing, _, _, _, _, _) -> Tuple6_0
  (Just a, Nothing, _, _, _, _) -> Tuple6_1 a
  (Just a, Just b, Nothing, _, _, _) -> Tuple6_2 (a, b)
  (Just a, Just b, Just c, Nothing, _, _) -> Tuple6_3 (a, b, c)
  (Just a, Just b, Just c, Just d, Nothing, _) -> Tuple6_4 (a, b, c, d)
  (Just a, Just b, Just c, Just d, Just e, Nothing) -> Tuple6_5 (a, b, c, d, e)
  (Just a, Just b, Just c, Just d, Just e, Just f) -> Tuple6_6 (a, b, c, d, e, f)

fromTuple6 :: Tuple6 a b c d e f  -> (Maybe a, Maybe b, Maybe c, Maybe d, Maybe e, Maybe f)
fromTuple6 t = case t of
  Tuple6_0 -> (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
  Tuple6_1 a -> (Just a, Nothing, Nothing, Nothing, Nothing, Nothing)
  Tuple6_2 (a, b) -> (Just a, Just b, Nothing, Nothing, Nothing, Nothing)
  Tuple6_3 (a, b, c) -> (Just a, Just b, Just c, Nothing, Nothing, Nothing)
  Tuple6_4 (a, b, c, d) -> (Just a, Just b, Just c, Just d, Nothing, Nothing)
  Tuple6_5 (a, b, c, d, e) -> (Just a, Just b, Just c, Just d, Just e, Nothing)
  Tuple6_6 (a, b, c, d, e, f) -> (Just a, Just b, Just c, Just d, Just e, Just f)

listToTuple6 :: [a] -> Tuple6 a a a a a a
listToTuple6 l = case length l of
  0 -> Tuple6_0
  1 -> Tuple6_1 (l !! 0)
  2 -> Tuple6_2 ((l !! 0), (l !! 1))
  3 -> Tuple6_3 ((l !! 0), (l !! 1), (l !! 2))
  4 -> Tuple6_4 ((l !! 0), (l !! 1), (l !! 2), (l !! 3))
  5 -> Tuple6_5 ((l !! 0), (l !! 1), (l !! 2), (l !! 3), (l !! 4))
  _ -> Tuple6_6 ((l !! 0), (l !! 1), (l !! 2), (l !! 3), (l !! 4), (l !! 5))
