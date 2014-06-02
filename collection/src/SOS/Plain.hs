{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}

module SOS.Plain where

import SOS.State

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Size

import qualified Data.Map as M
import qualified Data.Set as S

import Autolib.FiniteMap () -- instances
import Autolib.Set () -- instances

import Control.Applicative ( (<$>) )
import System.Random
import Data.Typeable

newtype Move = Move (Int,Int) 
    deriving ( Eq, Ord, Typeable )

newtype Program = Program (M.Map Observe Move) 
    deriving ( Eq, Ord, Typeable )

derives [makeReader, makeToDoc] [''Move, ''Program ]

instance Size Program where size (Program m) = M.size m

instance Show Move where show = render . toDoc
instance Show Program where show = render . toDoc

program0 n = Program $ M.fromList 
    $ zip (map observe $ states n) 
    $ concat $ repeat [ Move (0,1), Move(1,2), Move(2,0) ]

some n = Program <$> M.fromList <$> ( forM (states n) $ \ s -> do
        f <- randomRIO (0,2)
        t <- randomRIO (0,2)
        return (observe s, Move (f,t)) )
    
restricted = True

step :: Program -> State -> Reporter State
step (Program m) s = do
    let obs = observe s
    inform $ text "observation:" <+> toDoc obs
    case M.lookup obs m of
            Nothing -> reject $ text "not in domain of program"
            Just mo -> do
                inform $ toDoc mo
                move mo s

move :: Move -> State -> Reporter State
move m @ (Move (from, to)) (State xss) = do
    when ( from < 0 || length xss <= from ) 
         $ reject $ text "move start is out of range"
    when ( to   < 0 || length xss <= to   ) 
         $ reject $ text "move target is out of range"
    let (pre, this : post) = splitAt from xss
    when (null this) 
         $ reject $ text "move start is empty"
    let yss = pre ++ tail this : post
    let (pre, that : post) = splitAt to yss
    when ( restricted && (to == 1) && not (null that)) 
         $ reject $ text "cannot push on non-empty middle stack" 
    let out = pre ++ (head this : that) : post
    return $ State out

