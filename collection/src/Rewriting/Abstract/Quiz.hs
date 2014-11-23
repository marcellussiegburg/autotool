{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}


module Rewriting.Abstract.Quiz where

import Rewriting.Abstract.Data
import Rewriting.Abstract.ToDoc
import qualified Rewriting.Abstract.Solve as S
import Autolib.TES.Identifier
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import qualified Autolib.Util.Zufall as Z

import Data.Typeable
import Control.Monad ( forM, replicateM )
import Control.Applicative ( (<$>) )
import Data.List ( maximumBy )
import Data.Function (on )

data Config =
     Config { unknowns :: [ Identifier ]
            , unary_operators :: [ Op1 ]
            , unary_properties :: [ Prop1 ]
            , clauses :: Int
            , max_domain_size :: Int
            , candidates :: Int
            }
    deriving Typeable

config0 :: Config
config0 = Config 
        { unknowns = [ read "R" ]
        , unary_operators = 
              [ Inverse, Complement 
              , Transitive_Closure
              , Transitive_Reflexive_Closure
              ]
        , unary_properties = 
              [ Transitive
              , Reflexive, Irreflexive
              , Symmetric, Asymmetric, Antisymmetric
              , SN, WN
              -- , UN, UNC, CR, WCR 
              ]
        , clauses = 3
        , max_domain_size = 4
        , candidates = 1000
        }

derives [makeReader,makeToDoc] [''Config]

-- roll :: Config -> IO Prop
roll conf = do
    ps <- forM [ 1 .. candidates conf ] $ \ i -> do
        p <- prop conf
        return (p, smallest_solution_size conf p, size p)
    return $ maximumBy (compare `on` \ (p,s,z) -> (s,negate z)) ps

prop :: Config -> IO Prop
prop conf = And <$> replicateM (clauses conf) (clause conf)

clause conf = do
    base <- pick $ unknowns conf
    op <- pick $ id : (map Op1 $ unary_operators conf)
    p <- pick $ unary_properties conf
    sign <- pick [ id, Not . PropParens ]
    return $ sign $ Prop1 p $ op $ Ref base

pick :: [a] -> IO a
pick = Z.eins

smallest_solution_size :: Config -> Prop -> Maybe Int
smallest_solution_size conf prop = 
    let handle dom = 
            if dom > max_domain_size conf then Nothing
            else if S.solvable (unknowns conf) prop dom
            then Just dom else handle $ succ dom
    in  handle 1
           
