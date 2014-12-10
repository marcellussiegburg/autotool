{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}

module Lattice.LLL.Task where

import Lattice.LLL.Compute

import Autolib.Reporter hiding ( run, execute )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Challenger.Partial
import Inter.Types

import Data.Typeable
import Data.Ratio

data Lattice_LLL = Lattice_LLL 
    deriving (Typeable, Read, Show)

instance OrderScore Lattice_LLL where
    scoringOrder _ = Increasing

instance Partial Lattice_LLL [[Integer]] [Step] where

    describe _ base = vcat 
       [ text "Give a sequence of steps of the LLL algorithm"
       , text "(using Shoup's condition for swap steps)"
       , text "for the lattice with base"
       , toDoc $ map V base
       , text "We use exact rational arithmetic in all computations,"
       , text "and fixed point format only for display."
       ]

    initial _ base = 
        [ Reduce { target = 1, factor = 2, using = 0 }
        , Swap { this = 2, that = 3 }
        ]

    partial _ base steps = do
        void $ foldM execute (start base) steps

    total _ base steps = do
        result <- silent $ foldM execute (start base) steps
        inform $ nice result
        case sizereductions result of
             [] -> inform $ text "base is size-reduced"
             (_,msg):_ -> reject msg
        case swaps result of
             [] -> inform $ text "base is Shoup-reduced"
             (_,msg):_ -> reject msg

execute s step = do
    inform $ vcat [ nice s, toDoc step ]
    case step of
        Reduce {target=t,factor=f,using=u} -> do
          silent $ do
            assert (elem t $ range s)
                   $ text "target: index in range?"
            assert (elem u $ range s)
                   $ text "using: index in range?"
            assert (t > u) $ text "target > using ?"
          silent $ do
            let s' = apply step s
            when ( not $ sizereductions_check s' t u ) 
                 $ reject $ vcat 
                    [ nice s'
                    , sizereductions_message s' t u
                    ]

        Swap { this = i, that = j } -> silent $ do
            assert (elem i $ range s) $ text "this: index in range?"
            assert (elem j $ range s) $ text "that: index in range?"
            assert (j == i+1) $ text "this + 1 == that ?"
            assert ( not $ shoup_check s i j ) 
                   $ shoup_message s i j
            
    return $ apply step s

make_fixed :: Make
make_fixed = direct Lattice_LLL 
  ( [ [ 1 , 0 , 0 , 0 , 0 , 0 , 0 , 7253 ]
    , [ 0 , 1 , 0 , 0 , 0 , 0 , 0 , 20717 ]
    , [ 0 , 0 , 1 , 0 , 0 , 0 , 0 , 59179 ]
    , [ 0 , 0 , 0 , 1 , 0 , 0 , 0 , 169045 ]
    , [ 0 , 0 , 0 , 0 , 1 , 0 , 0 , 482872 ]
    , [ 0 , 0 , 0 , 0 , 0 , 1 , 0 , 1379306 ]
    , [ 0 , 0 , 0 , 0 , 0 , 0 , 1 , 3939938 ]
    ] :: [[Integer]] )
