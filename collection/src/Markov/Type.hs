{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Markov.Type where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

newtype Program = Program [ Rule ]
  deriving (Eq, Ord, Typeable )

type Rule = ( String,String )

$(derives [makeReader, makeToDoc] [''Program])

example :: Program 
example = Program [ ( "01", "10"), ("#", "") ]

instance Size Program where 
    size (Program rules) = fromIntegral $ length rules

instance Show Program where
  show = render . toDoc



