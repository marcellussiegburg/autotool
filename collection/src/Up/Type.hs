{-# language ScopedTypeVariables #-}

module Up.Type ( module Up.Data ) where

import Up.Data
import Up.Reader
import Up.ToDoc

import Test.SmallCheck
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.SmallCheck

import Autolib.Reader
import Autolib.ToDoc


prop_read_write ( _ :: t) =
  it "read-write" $ property $ \ e -> read_write (e::t)

read_write e =                              
    Just e  ==
    down (parse ( parse_complete reader) "<>"
                ( render $ toDoc e ) )

down = either (const Nothing) Just          
