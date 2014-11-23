{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language MultiParamTypeClasses #-}
{-# language BangPatterns #-}

module Rewriting.Abstract.Quiz where

import Rewriting.Abstract.Data
import Rewriting.Abstract.Problem
import Rewriting.Abstract.ToDoc
import Rewriting.Abstract.Braced
import Rewriting.Abstract.Fixed () -- instances only
import qualified Rewriting.Abstract.Solve as S
import Autolib.TES.Identifier
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import qualified Autolib.Util.Zufall as Z

import Inter.Quiz
import Inter.Types

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable
import Control.Monad ( forM, replicateM, forever, when )
import Control.Applicative ( (<$>) )
import Data.List ( maximumBy )
import Data.Function (on )

import Control.Concurrent
import Control.Concurrent.STM
import System.IO
import System.Timeout


{-

the generator creates problem instances and solves them,
for domain sizes @[1, 2 .. max_domain_size  conf]@
for @generator_timeout_seconds conf@ time.
It picks an instance that maximizes the minimal domain size.

-}

-- roll :: Config -> IO Prop
roll conf = do
    let exec = do 
             p <- prop conf 
             let s = smallest_solution_size conf p
             return (p, s)
        cmp = compare `on` \ (p,s) -> (s, negate $ size p)
    start <- exec    
    best <- atomically $ newTVar start
    timeout (generator_timeout_seconds conf * 10^6) 
        $ replicateM (candidates conf) $ do
        this@(p,s) <- exec
        prev <- atomically $ readTVar best
        -- hPutStrLn stderr $ show $ toDoc this
        when (cmp this prev >= EQ) $ atomically 
             $ writeTVar best this
    atomically $ readTVar best

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
           
instance Generator Abstract_Rewriting Config Problem where
    generator _ conf key = do
        (p,_) <- roll conf
        return $ Problem
               { property = p
               , domain_size_should_be = (GT, 0)
               , given = M.empty
               , wanted = S.fromList $ unknowns conf
               }

instance Project Abstract_Rewriting Problem Problem where
    project _ p = p

make :: Make
make = quiz Abstract_Rewriting config0

