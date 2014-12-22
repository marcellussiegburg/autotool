{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}

module Rewriting.Abstract.Problem where

import Rewriting.Abstract.Syntax
import Rewriting.Abstract.Braced

import qualified Autolib.Relation as R
import Autolib.Size
import Autolib.ToDoc
import Autolib.Reader
import Autolib.TES.Identifier
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable

data Problem = 
     Problem { property :: Prop
             , domain_size_should_be :: (Ordering, Int)
             , given :: M.Map Identifier (Braced Int Int)
             , wanted :: S.Set Identifier
             }
    deriving Typeable

problem0 :: Problem
problem0 = Problem
    { domain_size_should_be = (EQ, 3)
    , given = M.fromList 
        [(mk 0 "R", Braced (R.make [(1,2), (2,3)])) ]
    , wanted = S.fromList [ mk 0 "S" ]
    , property = prop0
    }

data Solution = 
     Solution { domain_size :: Int
              , assignment :: M.Map Identifier (Braced Int Int)
              }
    deriving Typeable

-- | this is arbitrary: the sum of the numbers
-- of edges of the relations.
instance Size Solution where 
    size s = sum $ map size $ M.elems $ assignment s

solution0 :: Solution
solution0 = Solution
    { domain_size = 3
    , assignment = M.fromList
        [(mk 0 "S", Braced (R.make [(3,2)]))]
    }

data Abstract_Rewriting = Abstract_Rewriting 
    deriving Typeable

derives [makeReader, makeToDoc] 
        [''Problem, ''Solution, ''Abstract_Rewriting ]
derives [makeReader] [''Ordering]

instance Show Abstract_Rewriting where show = render . toDoc

data Config =
     Config { unknowns :: [ Identifier ]
            , unary_operators :: [ Op1 ]
            , binary_operators :: [ Op2 ]
            , unary_properties :: [ Prop1 ]
            , binary_properties :: [ Prop2 ]
            , clauses :: Int
            , max_domain_size :: Int
            , candidates :: Int
            , generator_timeout_seconds :: Int -- ^ seconds
            }
    deriving Typeable

config0 :: Config
config0 = Config 
        { unknowns = [ read "R" ]
        , unary_operators = [ minBound .. maxBound ]
        , binary_operators = take 0 [ minBound .. maxBound ]
        , unary_properties = [ minBound .. maxBound ]
        , binary_properties = take 0 [ minBound .. maxBound ]
        , clauses = 3
        , max_domain_size = 4
        , candidates = 100
        , generator_timeout_seconds = 5
        }

derives [makeReader,makeToDoc] [''Config]
