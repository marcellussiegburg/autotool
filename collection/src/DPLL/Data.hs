{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module DPLL.Data where

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

type Variable = Int 
type Literal = Int 
type Clause = [ Literal ]
type CNF = [ Clause ]

variable :: Literal -> Variable
variable l = abs l

positive :: Literal -> Bool
positive l = l > 0

cnf0 :: CNF
cnf0 = [[1,2,3],[-1,-2],[2,-3],[1,-3],[-2,3]]
