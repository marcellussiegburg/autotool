{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module BDD.Top ( make_fixed, make_quiz ) where

import BDD.Data
import BDD.Check
import BDD.Semantics
import BDD.Quiz

import Expression.Op
import Boolean.Op
import qualified Boolean.BDD

import qualified OBDD as O

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Inter.Types hiding ( Var )
import Inter.Quiz
import Data.Typeable
import System.Random

data Construct_BDD = Construct_BDD deriving ( Typeable )

data Instance = 
     Instance { formula :: Exp Bool
              , order :: Maybe [ Identifier ]
              }
    deriving ( Typeable )

instance0 :: Instance 
instance0 = Instance { formula = read "x && (not y || z)" , order = read "[x,y,z]" }

derives [makeReader, makeToDoc] [''Construct_BDD, ''Instance ]

instance Show Construct_BDD where show = render . toDoc

instance OrderScore Construct_BDD where 
    scoringOrder _ = Decreasing

instance Partial Construct_BDD Instance (BDD Identifier) where
    describe _ i = vcat
        [ text "Gesucht ist ein ROBDD"
        , case order i of
              Just vs -> text "mit der Variablenordnung" </> toDoc vs
              Nothing -> text "(mit beliebiger Variablenordnung)"
        , text "für die Funktion" </> toDoc ( formula i )
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


instance Generator Construct_BDD Config Instance where
    generator p conf key = do
        f <- roll_formula conf
        return $ Instance { formula = f , order = Just $ variables conf }

instance Project Construct_BDD Instance Instance where
    project _ i = i

make_quiz :: Make
make_quiz = quiz Construct_BDD config0

