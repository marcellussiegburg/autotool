{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
{-# language OverlappingInstances #-}

module Haskell.Blueprint.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable
import Data.Data
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax

import Test.QuickCheck ( Args (..))
import System.Random ( StdGen )

-- | use self-delimiting read/show
-- but nicely formatted reader/todoc
data Code = Code String deriving ( Eq, Ord, Typeable, Read, Show )


-- this is highly problematic because it eats all of the input
-- corollary: it cannot be composed 
-- (we cannot parse Code as part of a larger structure)
-- corollary: all properties of the problem statement
-- must be in the code (e.g., as annotations/pragmas)
instance Reader Code where 
    reader = do cs <- getInput ; setInput "" ; return $ Code cs

instance ToDoc Code where 
    toDoc ( Code cs ) = vcat $ map text $ lines cs

-- this is the size of the syntax tree.
-- TODO: count just the nodes that are visible.
-- otherwise, it's not understandable for the student.
instance Size Code where
    size ( Code cs ) = case parseModule cs of
        ParseOk m -> msize m
        _ -> 0

msize :: Data a => a -> Int
msize m = sum $ 1 : gmapQ msize m



code_example :: Code
code_example = Code $ unlines 
    [ "module Blueprint where"
    , ""  
    , "foo :: Int"
    , "foo = undefined"
    , ""
    , "test :: Bool"
    , "test = foo * foo == 9"
    ]


-- below here: not needed?

data Driver = QuickCheck Args
            | SmallCheck { tests_run :: Int 
                         , failures_shown :: Int
                         }
   deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Driver])


instance Reader StdGen 
instance ToDoc StdGen

$(derives [makeReader, makeToDoc] [''Args])



