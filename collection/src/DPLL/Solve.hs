{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module DPLL.Solve where

import DPLL.Data

import Autolib.Reader
import Autolib.ToDoc

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Typeable

data Step = Decide Literal | Propagate Literal | Learn Clause | Backtrack
          | Success -- Assignment 
          | Fail
    deriving (Eq, Typeable)


type Assignment = [ Literal ]

data State =
     State { formula :: CNF
           , assignment :: Assignment 
           , decisions :: [ Literal ]
           }

derives [makeReader, makeToDoc] [''Step, ''State]

solve :: CNF -> [ Step ]
solve cnf = work $ State { formula = cnf, assignment = [], decisions = [] }

work :: State  -> [ Step ]
work st = 
    let red = reduce (assignment st) (formula st)
        units = concat $ filter ( \ cl -> 1 == length cl ) red
        v = minimum $ map abs $ concat red
    in  if null red then success st
        else if elem [] red then conflict st
        else case units of
             [] -> decide (negate v) st
             u : _ -> propagate u st

success st = [ Success {- ( assignment st ) -} ]

decide lit st = Decide lit
       : work  st { decisions = lit : decisions st
                    , assignment = lit : assignment st 
                    } 

propagate lit st = Propagate lit 
       : work st { assignment = lit : assignment st }

conflict st = case decisions st of
    d : ds -> Backtrack 
         : work st { assignment = (negate d :) $ tail $ dropWhile ( /= d) $ assignment st
                      , decisions = ds }
    [] -> [ Fail ]

reduce :: Assignment -> CNF -> CNF
reduce ass cnf = 
      map ( filter ( \ l -> not ( elem (negate l) ass) ) )
    $ filter ( \ cl -> not (satisfies ass cl)) cnf

satisfies :: Assignment -> Clause -> Bool
satisfies ass cl = 
    not $ S.null $ S.intersection ( S.fromList ass) (S.fromList cl)
