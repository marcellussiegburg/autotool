module Algebraic.Nested.Roll where

import Algebraic.Nested.Type
import Algebraic.Nested.Op

import Autolib.Pick
import Autolib.ToDoc

import qualified Data.Set as S
import System.Random

import Control.Applicative ((<$>))

roll :: Ord a => [a] -> Int -> IO (Type a)
roll base size =
  if size < 1 then do
      action <- pick [ return $ Make $ S.empty , unit <$> pick base ]
      action
  else do
      sl <- randomRIO (0, size-1)
      let sr = size - 1 - sl
      l <- roll base sl
      r <- roll base sr
      return $ union l $ Make $ S.fromList [ Packed r ]

