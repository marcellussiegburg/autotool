{-# language TemplateHaskell #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Markov.Machine where

import Machine.Class
import Machine.History

import Markov.Type

import Autolib.Set
import Autolib.Size
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Util.Splits
import Data.List ( partition )

import Numeric
import Data.Char

newtype Tape = Tape { unTape :: String }
  deriving (Eq, Ord)

instance ToDoc Tape where toDoc t = toDoc $ unTape t

data State =
  State { step :: Int, tape :: Tape, earlier_info :: [ State ] }
  deriving (Eq, Ord)

$(derives [makeToDoc][''State])

instance History State where  history = earlier_info
  

instance Compute Program State where
    depth _ s = step s
    next p s = mkSet $ successors p s
    accepting p s = null $ successors p s

successors (Program rules) s = take 1 $ do
  (l,r) <- rules
  (pre, midpost) <- splits $ unTape $ tape s
  let (mid,post) = splitAt (length l) midpost
  guard $ mid == l
  return $ State { step = succ $ step s
                 , tape = Tape $ pre ++ r ++ post
                 , earlier_info = s : earlier_info s
                 }

instance In Program Tape State where
    input_reporter p s = do
        return $ State { step = 0, tape = s, history = [] }
instance Out Program Tape State where
    output_reporter p s = do
      let t = tape s
          (good,bad) = partition (`elem` "01") $ unTape t
      when (not $ null bad) $ reject $ vcat
         [ text "work tape does contain letters besides 0,1" ]
      return t

instance Encode Tape where
    encode xs = Tape $ do
      x <- xs
      showIntAtBase 2 intToDigit x "#"
    
instance Decode Tape where
    decode (Tape t) =
      foldl ( \ a c -> 2*a + fromIntegral (ord c - ord '0')) 0 t
