{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module BDD.Quiz where

import Expression.Op
import Boolean.Op
import qualified Boolean.BDD
import qualified OBDD as O

import Inter.Types hiding ( Var )

import Autolib.TES.Term
import qualified Autolib.TES.Binu as B

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Data.Function ( on )
import Data.List ( minimumBy )
import Data.Typeable
import System.Random

data Config =
     Config { formula_size :: Int
            , operators :: B.Binu (Op Bool)
            , variables :: [ Identifier ] 
            , bdd_size :: Int
            , bdd_candidates :: Int
            }
    deriving ( Typeable )

config0 :: Config
config0 = Config { formula_size = 12
                 , variables = read "[p,q,r,s]"
                 , operators = B.Binu
                    { B.binary  = [ read "&&", read "||" , read "<->" ]
                    , B.unary   = [ read "not" ]
                    , B.nullary = [] -- [ read "true", read "false" ]
                    }
                 , bdd_size = 12
                 , bdd_candidates = 1000
                 }

derives [makeReader, makeToDoc] [ ''Config ]

roll_formula conf = do
    fs <- forM [ 1 .. bdd_candidates conf ] $ \ i -> do
        f <- roll_term (operators conf) (variables conf) (formula_size conf)
        return $ case result $ Boolean.BDD.evaluate f of
            Nothing -> []
            Just s -> [ (f, abs $ O.size s - bdd_size conf) ]
    return $ fst $ minimumBy ( compare `on` snd ) $ concat fs
    

roll_term ops vars s =
    if s <= 1 then do
        pick $ map ( \f -> Node f []) (B.nullary ops) ++ map Var vars
    else do
        op <- pick $ map Left (B.unary ops) ++ map Right (B.binary ops)
        case op of
            Left u -> do
                arg <- roll_term ops vars $ s - 1
                return $ Node u [ arg ]
            Right b -> do
                sl <- randomRIO (1, s-1) ; let sr = s - 1 - sl
                arg1 <- roll_term ops vars sl 
                arg2 <- roll_term ops vars sr
                return $ Node b [ arg1, arg2 ]

pick xs = do
    i <-  randomRIO ( 0, length xs - 1) 
    return $ xs !! i
    
