{-# language ScopedTypeVariables #-}

module Polynomial.Unary.Test where

import Polynomial.Unary

import Polynomial.Class
import Polynomial.Patch
import Polynomial.Pattern

import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.SmallCheck
import Test.SmallCheck.Series

import Autolib.ToDoc
import Autolib.Reader

import qualified Prelude
import Prelude (Bool(..), (==), ($), Either(..))

rw_spec ( _ :: a ) = describe "Print/Parse properties" $ do
    it "read (show x) == x" $ property $ \ a ->  
        case runParser reader () "test" (render $ toDoc a) of
             Right (b :: a) -> a == b
             _ -> False
