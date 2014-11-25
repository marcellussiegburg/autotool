{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Rewriting.Abstract.Fixed where

import Rewriting.Abstract.Syntax
import Rewriting.Abstract.Semantics
import Rewriting.Abstract.Braced
import Rewriting.Abstract.Problem

import Autolib.ToDoc
import Autolib.Multilingual
import Autolib.Reader
import Autolib.Reporter
import Autolib.Size

import Autolib.TES
import Autolib.TES.Identifier
import qualified Autolib.Relation as R

import Autolib.FiniteMap
import qualified Data.Map as M
import qualified Data.Set as S

import Challenger.Partial
import Inter.Types
import Data.Typeable
import Control.Monad ( when )



instance OrderScore Abstract_Rewriting where
    scoringOrder _ = Increasing

instance Partial Abstract_Rewriting Problem Solution where
    
    describe _ p = vcat
        [ multitext [(UK,"Define relations")
                    ,(DE,"Definieren Sie Relationen")]
          <+> toDoc (S.toList $ wanted p)
        , multitext [(UK,"on domain [1, 2 .. domain_size] with domain_size")
                    ,(DE,"auf dem Bereich [1, 2 .. domain_size] mit domain_size")]
          <+> let (rel,m) = domain_size_should_be p
              in  text ( case rel of LT -> "<" ; EQ -> "=" ; GT -> ">" ) <+> toDoc m
        , multitext [(UK, "such that this property holds:")
                    ,(DE, "mit den folgenden Eigenschaften:")]
          </> toDoc ( property p )
        , multitext [(UK, "in this environment:")
                    ,(DE, "mit den folgenden Vorgaben:")]
          </> toDoc ( given p )

{-
          </> vcat ( map ( \(k,v) -> 
                      hsep [ toDoc k, equals, toDoc v ]
              ) $ M.toList $ given p )
-}

        ]

    initial _ p = solution0

    partial _ p s = do
        let (cmp, t) = domain_size_should_be p
        when ( not $ cmp == compare (domain_size s) t ) 
            $ reject $ multitext [(UK,  "invalid domain size")
                                  ,(DE, "falscher Grundbereich")]
        let dom = S.fromList [ 1 .. domain_size s ]
        void $ forM ( M.toList $ assignment s ) $ \ (k,Braced r) -> do
            let wrong = do 
                    p @ (x,y) <- R.toList r; e <- [x,y] 
                    guard $ S.notMember e dom
                    return p
            when (not $ null wrong) $ reject $ vcat
                [ multitext [(UK,  "relation"),(DE, "Relation")] <+> toDoc k 
                , multitext [(UK, "uses elements from outside the domain")
                            ,(DE, "enthält Elemente außerhalb des Grundbereiches")] 
                </> toDoc wrong
                ]

    total _ p s = do
        let d = S.fromList [ 1 .. domain_size s ]
        let rel (Braced r) = R.make_on (d,d) $ R.toList r
            env = M.map rel
                $ M.union -- Note: left-biased, so student
                 -- cannot override what is given
                  (given p) (assignment s)
        (ok, doc) <- prop env ( property p )
        inform doc
        when (not ok) $ reject 
            $ multitext [(UK, "The stated property does not hold.")
                        ,(DE, "Die angegebenen Eigenschaften gelten nicht.")]

make :: Make
make = direct Abstract_Rewriting problem0


