{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}

module Rewriting.Completion.CP where

import Autolib.TES
import Autolib.TES.Unify

import Autolib.ToDoc

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad ( guard )

data Symbol c => CP v c = CP { left :: Rule (Term v c)
                 , position :: Position
                 , right :: Rule (Term v c)
                 , substitution ::
                      M.Map (Either v v)
                            (Term (Either v v) c)
                 }

derives [makeToDoc] [''CP]


expand  :: (Symbol c, Ord v) =>
     CP v c -> (Term (Either v v) c, Term (Either v v) c)
expand cp = 
    ( apply_partial ( substitution cp  ) $ vmap Left $ rhs $ left cp
    , apply_partial ( substitution cp ) 
            $ poke (vmap Left $ lhs $ left cp) 
                   (position cp, vmap Right $ rhs $ right cp)
    )

normalize :: Ord v
          => (Term v c, Term v c)
          -> (Term Int c, Term Int c)
normalize (l,r) = 
    let vs = S.union (vars l) (vars r)
        m = M.fromList $ zip (S.toList vs) [1..]
    in  ( vmap (m M.!) l
        , vmap (m M.!) r
        )

criticalpairs :: (Ord v, Symbol c)
              => [ Rule (Term v c) ]
              -> [ CP v c ]
criticalpairs rules = do
    ( i, rule1 ) <- zip [ 0 :: Int .. ] $ rules 
    let x = vmap Left $ lhs rule1
    ( p, z ) <- positions x
    guard $ not $ isvar z

    ( j, rule2 ) <- zip [ 0 :: Int .. ] $ rules
    let y = vmap Right $ lhs rule2
    -- treat self-overlaps specially
    guard $ ( i == j ) <= not ( null p )

    u <- maybeToList $ mgu z y

    return $ CP
           { left = rule1
           , position = p
           , right = rule2
           , substitution = u
           }
