{-# language DoAndIfThenElse #-}

module Regular.Grammatik where


import qualified Grammatik as G
import qualified Grammatik.Property as G


import qualified Autolib.NFA as A
import qualified Autolib.NFA.Mirror as A

import qualified Autolib.ENFA as E
import  Autolib.ENFA.Uneps (uneps)


import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Set
import qualified Autolib.Relation
import qualified Condition as C

import Control.Monad.State.Strict
import qualified Data.Map as M
import qualified Data.Set as S

g2nfa :: G.Grammatik -> Reporter ( A.NFA Char Int )
g2nfa g = do
    ma <- try_g2enfa g
    case ma of
        Just a -> return a
        Nothing -> do
            mb <- try_g2enfa $ G.mirror g
            case mb of
                Just b -> return $ A.mirror b
                Nothing -> reject $ text "Die Grammatik ist weder rechts- noch links-linear."

try_g2enfa g = do
    r <- Autolib.Reporter.silent 
       $ Autolib.Reporter.wrap 
       $ C.condition G.Rechtslinear g
    return $ case r of
        Nothing -> Nothing
        Just _ -> Just $ uneps $ reli_g2enfa g

-- | rechtslineare Grammatik zu Automat
reli_g2enfa :: G.Grammatik -> E.ENFA Char Int
reli_g2enfa g = 
    let s = execState 
          ( forM_ ( S.toList $ G.regeln g ) $ rule g ) s0
        sts = mkSet [ 0 .. pred $ top s ]
    in  E.ENFA { E.alphabet = G.terminale g
             , E.states = sts
             , E.starts = mkSet [ vars s M.! G.start g ]
             , E.finals = mkSet [ final s ]
             , E.trans = E.tcollect $ trans s
             , E.mirror_trans = E.mitcollect $ trans s
             , E.eps = Autolib.Relation.make_on (sts,sts) $ eps s
             , E.mirror_eps = Autolib.Relation.make_on (sts,sts) $ map ( \(p,q)->(q,p)) $ eps s 
             , E.eps_is_trans_reflex = False
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

rule :: G.Grammatik -> ( [Char],[Char] ) -> State S ()
rule g ( [lhs], rhs ) = do
    l <- variable lhs
    links g l rhs

links g l rhs = case rhs of
    [] -> modify $ \ s -> 
       s { eps = (l, final s) : eps s }
    [ last ] | S.member last ( G.variablen g ) -> do
        r <- variable last
        modify $ \ s -> 
            s { eps = (l, r) : eps s }
    (c : cs) | S.member c ( G.terminale g ) -> do
        r <- fresh
        modify $ \ s -> 
           s { trans = (l,c,r) : trans s }
        links g r cs
        
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
