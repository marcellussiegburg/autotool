{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module BDD.Top ( make_fixed, make_quiz ) where

import BDD.Data
import BDD.Check
import BDD.Semantics

import Autolib.TES
import Expression.Op
import Boolean.Op
import qualified Boolean.BDD

import qualified OBDD as O

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Inter.Types
import Inter.Quiz
import Data.Typeable

data Construct_BDD = Construct_BDD deriving ( Typeable )

type Formula v = Term v (Op Bool)

data Instance v = 
     Instance { formula :: Formula v
              , order :: [ v ]
              }
    deriving ( Typeable )

instance0 :: Instance Identifier
instance0 = Instance { formula = read "x && (not y || z)" , order = read "[x,y,z]" }

derives [makeReader, makeToDoc] [''Construct_BDD, ''Instance ]

instance Show Construct_BDD where show = render . toDoc

instance OrderScore Construct_BDD where 
    scoringOrder _ = None

instance Partial Construct_BDD (Instance Identifier) (BDD Identifier) where
    describe _ i = vcat
        [ text "Gesucht ist das ROBDD"
        , text "zur Formel" </> toDoc ( formula i )
        , text "mit der Variablenordnung" </> toDoc ( order i )
        ]
    initial _ i = bdd0 $ read "[x,y]"    
    partial _ i b = do
        check (order i) b
    total _ i b = do
        si <- Boolean.BDD.evaluate ( formula i )
        let sb = BDD.Semantics.value b
            diff = O.binary (/=) si sb
        case O.all_models diff of
            [] -> return ()
            m : _ -> reject $ text "Das BDD ist nicht äquivalent zur Formel, für Belegung"
                </> toDoc m

make_fixed :: Make
make_fixed = direct Construct_BDD instance0

make_quiz :: Make
make_quiz = undefined
