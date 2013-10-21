{-# language DoAndIfThenElse #-}

module Regular.Grammatik where


import qualified Grammatik as G


import qualified Autolib.NFA as A
import qualified Autolib.ENFA as E
import  Autolib.ENFA.Uneps (uneps)


import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Set
import qualified Autolib.Relation

import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Set as S

g2nfa :: G.Grammatik -> A.NFA Char Int
g2nfa = uneps . g2enfa

g2enfa :: G.Grammatik -> E.ENFA Char Int
g2enfa g = 
    let s = execState ( forM_ ( S.toList $ G.regeln g ) rule ) s0
        sts = mkSet [ 0 .. pred $ top s ]
    in  E.ENFA { E.alphabet = G.terminale g
             , E.states = sts
             , E.starts = mkSet [ vars s M.! G.start g ]
             , E.finals = mkSet [ final s ]
             , E.trans = E.tcollect $ trans s
             , E.mirror_trans = E.mitcollect $ trans s
             , E.eps = Autolib.Relation.make_on (sts,sts)
                     $ eps s
             }

data S = S { final :: Int
           , top :: Int
           , vars :: M.Map Char Int
           , trans :: [ (Int,Char,Int) ]
           , eps :: [ (Int,Int) ]
           }

s0 :: S
s0 = S { final = 0, top = 1, vars = M.empty 
       , trans = [], eps = []
       }

rule :: ( [Char],[Char] ) -> State S ()
rule ( [lhs], rhs ) = do
    l <- variable lhs
    if null rhs then do
        modify $ \ s -> 
            s { eps = ( l, final s) : eps s }
    else do
        links l rhs

links l rhs = case rhs of
    [ last ] -> do
        r <- variable last
        modify $ \ s -> 
            s { eps = (l, r) : eps s }
    (c : cs) -> do
        r <- fresh
        modify $ \ s -> 
           s { trans = (l,c,r) : trans s }
        
fresh :: State S Int
fresh = do
    s <- get
    put $ s { top = succ $ top s }
    return $ top s

variable :: Char -> State S Int
variable c = do
    s  <- get
    case M.lookup c (vars s) of
        Nothing -> do
            r <- fresh
            modify $ \ s -> 
                s {  vars = M.insert c r $ vars s }
            return r
        Just k -> return k
