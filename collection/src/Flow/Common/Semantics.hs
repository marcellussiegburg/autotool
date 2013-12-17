{-# LANGUAGE ScopedTypeVariables #-}

module Flow.Common.Semantics where

-- import Flow.Program
import Flow.Expression
import Flow.Conditions
import Flow.State
import Flow.Common.Data
import Flow.Action
import Flow.Auto

import Flow.Transit

import Autolib.FiniteMap
import Autolib.ENFA
import Autolib.ENFA.Uneps
import qualified Autolib.NFA as N
import Autolib.NFA.Shortest
import Autolib.TES.Identifier (Identifier)

import Autolib.ToDoc
import Autolib.Reporter

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Control.Monad.State hiding ( State )
import Control.Applicative
import Data.List ( partition, sort )

semantics :: Set Flow.State.State
          -> Statement 
          -> Reporter ( N.NFA Label Vertex )
semantics all p = 
    evalStateT ( program (S.toList all)  p ) 
        $ ST { transitions = [], top = 0 }

program all p  = do
    start :: Int <- next
    goal  :: Int <- next
    handle all env0 ( start, p, goal )
    final <- next
    forM all $ \ st -> 
        transit ((goal,st),Just (st,Halt),(final,st))
    st <- get
    let times :: [a] -> [b] -> [(a,b)]
        times xs ys = (,) <$> xs <*> ys
    let enfa = eps_builder_all_final
                   (times [start] all :: [Vertex])
             $ transitions st
    return $ uneps enfa

---------------------------------------------------

data Target = Target_Goto  | Target_Continue | Target_Break
    deriving ( Eq, Ord, Show )

data Env = Env { cache :: M.Map (Target, Identifier) Int
               , current_loop :: Maybe (Int,Int)
               }
    deriving ( Eq, Ord, Show )

env0 = Env { cache = M.empty , current_loop = Nothing }


handle all env (start, stmt, goal) = case stmt of

    Label i sub -> case sub of
        While {} ->  -- Labels für Schleife werden jetzt angelegt  
            while all env (Just i) (start, sub, goal)
        _ ->  --  Labels für Sprünge wurden bei Betreten des Blocks allokiert
          case M.lookup (Target_Goto, i) $ cache env of
            Nothing ->  lift $ reject $ text "Label ohne umgebenden Block" </> toDoc sub
            Just target -> do
                 epsilon_connect all (start, target) 
                 handle all env (target, sub, goal)

    While {} -> while all env Nothing (start, stmt, goal)

    Atomic this -> 
        forM_ all $ \ st -> forM_ all $ \ st' ->
          transit 
            ((start,st)
            ,Just(st,Execute this) -- a real transition
            ,(goal,st'))

    Skip -> 
        block all env ( start,  [], goal )

    Block xs -> do
        let ns = do Label n _ <- xs ; return n
        when ( length (sort ns) < length ns ) $ lift $ reject
            $ text "Der Block enthält mehrfach definierte Marken" </> toDoc stmt
        env' <- foldM ( \ env n -> do 
                   t <- next 
                   return $ env { cache = M.insert (Target_Goto, n) t $ cache env }
                ) env ns
        block all env'  ( start,xs, goal)

    Branch {} -> branch all env (start,stmt,goal,goal)

    Continue t -> jump all env start (Target_Continue, t ) 
    Break    t -> jump all env start (Target_Break, t ) 
    Goto     t -> jump all env start (Target_Goto, Just t ) 

    _ -> lift $ reject $ text "Semantics.handle: missing case for" </> toDoc stmt


jump all env start (Target_Continue, Nothing ) = case current_loop env of
    Nothing -> lift $ reject $ text "Keine umgebende Schleife für: continue"
    Just (con,brk) -> epsilon_connect all (start, con)
        
jump all env start (Target_Break, Nothing ) = case current_loop env of
    Nothing -> lift $ reject $ text "Keine umgebende Schleife für: break"
    Just (con,brk) -> epsilon_connect all (start, brk)
        
jump all env start (target_type, Just name) = 
    case M.lookup (target_type, name) $ cache env of
        Nothing -> lift $ reject $ 
             let concept = case target_type of
                    Target_Goto -> text "Anweisung"
                    _ -> text "Schleife"
             in text "Es gibt keine" <+> concept <+> text "mit Label" <+> toDoc name
        Just target -> epsilon_connect all (start, target)



while all env mlabel (start, While test x, goal) = do
        -- need to introduce eps transition
        -- to make the transformation safe
        loop <- next
        epsilon_connect all (start, loop)
        let env' = Env { cache = case mlabel of
                Nothing -> cache env
                Just i -> M.insert (Target_Continue, i) loop
                        $ M.insert (Target_Break, i) goal
                        $ cache env
                       , current_loop = Just (loop,goal)
                       }
        branch all env'
             (loop, Branch test x Nothing, loop, goal) 

epsilon_connect all (from, to) = 
    forM_ all $ \ st -> transit ((from, st),Nothing,(to,st))


-- | all possible paths from start to goal
block all env  ( start, [], goal ) = do
    epsilon_connect all (start,goal)
block all env ( start, [x], goal ) = do
    handle all env  ( start, x, goal )
block all env  ( start, x:xs, goal ) = do
    mid <- next
    handle all env ( start, x, mid )
    block all env ( mid, xs, goal )


branch all env
    (start,Branch test yes mno ,goal1, goal2) = do
        let (yeah,noh) = 
                partition ( \ st -> evaluate st test ) 
                          all
        yy <- next
        epsilon_connect yeah (start, yy)
        nn <- next
        epsilon_connect noh  (start, nn)

        handle all env (yy,yes,goal1)
        let no = case mno of
                    Just no -> no ; Nothing -> Block []
        handle all env (nn,no,goal2)




{-

program all p  = do
    let start = 0
        nsts = zip [ start .. ] sts
    labels <- lift $ collect_labels nsts
    final <- next -- risky coding: top has been
             -- initialized to (length stmts) above,
             -- then the last of the (n+1) targets below
             -- leads to this final destination

    let with_label lab f = case M.lookup lab labels of
            Nothing -> lift $ reject $ hsep 
                 [ text "label", toDoc lab, text "nicht gefunden" ]
            Just n -> f n

    forM nsts $ \ ( n, Statement _ a ) -> case a of
        Skip -> forM_ all $ \ s -> 
            transit ((n, s), Nothing, (n+1, s))
        Action a -> forM_ all $ \ s -> forM all $ \ s'->
            transit ((n,s), Just(s, Execute a),(n+1,s'))
        Goto lab -> forM_ all $ \ s -> 
            with_label lab $ \ n' -> 
            transit ((n,s),Nothing,(n',s))
        If_Goto test lab -> with_label lab $ \ n' -> do
            let (yeah,noh) = 
                    partition 
                        ( \ st -> evaluate st test ) 
                        all
            forM_ yeah $ \ s -> 
                transit ((n,s),Nothing,(n',s))
            forM_ noh $ \ s -> 
                transit ((n,s),Nothing,(n+1,s))
    halt <- next
    forM all $ \ s -> 
        transit ((final,s),Just(s,Halt),(halt,s))

    let times :: [a] -> [b] -> [(a,b)]
        times xs ys = xs >>= \ x -> ys >>= \ y -> [(x,y)]
    st <- get
    let enfa = eps_builder_all_final
                   (times [start] all)
                   ( transitions st )
    return $ uneps enfa


------------------------------------------------

collect_labels nsts = foldM
    ( \ fm ( n, st @ (Statement mlab _ )) -> case mlab of
	    Nothing -> return fm
	    Just l  -> case lookupFM fm l of
	        Nothing -> return $ addToFM fm l n
		Just m  -> reject $ vcat
		    [ text "Label" <+> toDoc l
		    , text "in Anweisung" <+> toDoc st
		    , text "an Position" <+> toDoc n
		    , text "bereits definiert an Position" <+> toDoc m
		    ]
    ) emptyFM nsts


    

-}

