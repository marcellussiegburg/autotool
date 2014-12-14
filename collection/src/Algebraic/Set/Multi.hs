{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language TupleSections #-}

module Algebraic.Set.Multi where

import Expression.Op hiding (Identifier)
import qualified Autolib.TES.Binu as B

import Autolib.ToDoc
import Autolib.Reader hiding ((<|>), many)

import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad
import Data.Typeable
import Autolib.Util.Zufall
import Autolib.Pick

newtype Multiset a = Multiset (M.Map a Int)
    deriving Typeable

instance Ops (Multiset Identifier) where
    bops = B.Binu
        { B.nullary = []
        , B.unary = []
        , B.binary = 
          [ Op { name = "+", arity = 2
               , precedence = Just 5, assoc = AssocLeft
               , inter = lift2 union
               }
          , Op { name = "-", arity = 2
               , precedence = Just 5, assoc = AssocLeft
               , inter = lift2 difference
               }
          , Op { name = "&", arity = 2
               , precedence = Just 7, assoc = AssocLeft
               , inter = lift2 intersection
               }
          ]
        }

roll :: Ord a => [a] -> Int -> IO (Multiset a)
roll base max_coeff = multiset <$> ( forM base $ \ b -> do
         action <- pick [ return 0, randomRIO (1, max_coeff)]
         (b,) <$> action
    )

multiset :: Ord a => [(a,Int)] -> Multiset a
multiset kvs = 
    Multiset $ M.filter (/= 0) $ M.fromListWith (+) kvs

instance ToDoc a => ToDoc (Multiset a) where
    toDoc (Multiset m) = braces 
        $ hsep $ punctuate comma
        $ map (\(k,v) -> hcat [ toDoc k, text ":", toDoc v ])
        $ M.toList m

instance (Ord a, Reader a) => Reader (Multiset a) where
    reader = multiset <$> ( my_braces $ my_commaSep $ do
        k <- reader
        my_symbol ":"
        v <- mfilter (> 0) reader <|> fail "must be positive"
        return (k,v)
     )

null :: Ord a => Multiset a -> Bool
null (Multiset a) = M.null a

union :: Ord a => Multiset a -> Multiset a -> Multiset a
union (Multiset a) (Multiset b) = 
    Multiset $ M.unionWith (+) a b

difference :: Ord a => Multiset a -> Multiset a -> Multiset a
difference (Multiset a) (Multiset b) = 
    Multiset $ M.filter (> 0) $ M.unionWith (-) a b

symmetric_difference :: Ord a => Multiset a -> Multiset a -> Multiset a
symmetric_difference (Multiset a) (Multiset b) = 
      Multiset $ M.filter (> 0) 
    $ M.unionWith ( \ x y -> abs (x-y)) a b

intersection :: Ord a => Multiset a -> Multiset a -> Multiset a
intersection (Multiset a) (Multiset b) = 
    Multiset $ M.filter (> 0) $ M.intersectionWith min a b

newtype Identifier = Identifier String
    deriving (Eq, Ord, Typeable)

instance ToDoc Identifier where 
    toDoc (Identifier s) = text s

instance Reader Identifier where
    reader = Identifier 
        <$> ( (:) <$> letter <*> many alphaNum )
        <* my_whiteSpace
