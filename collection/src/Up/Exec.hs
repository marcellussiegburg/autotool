module Up.Exec where

import Up.Type
import Up.Store

import Autolib.Reporter
import Autolib.ToDoc
import qualified Autolib.Output as O

import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.Cont as C
import qualified Data.Map as M

-- | execute program until the Halt statement,
-- return the resulting Store
execute :: Int -> Block -> Reporter Store
execute steps b = 
      flip S.execStateT ( Up.Store.blank steps )
    $ flip C.runContT return
    $ do
        global <- 
            C.lift $ Up.Store.frame 0 0
        C.callCC $ \ exit -> block exit global b

block exit f (Block ss) = do
    vs <- forM ss $ statement exit f
    case vs of
        [] -> return $ ValUnit
        _  -> return $ last vs

statement exit f s = case s of
    Halt -> exit ValUnit
    Missing -> rej [ text "soll Anweisung oder Deklaration ersetzt werden" ]
    Statement e -> evaluate exit f e
    Declaration tn e -> do
        v <- evaluate exit f e
        assign f tn v
        return v
    
assign f n v = do
    C.lift $ put f n v

traced f e a = do
    C.lift tick
    s <- C.lift S.get
    when (step s >= max_steps s) $ rej
        [ text "die Anzahl der Auswertungsschritte ist zu hoch" ]
    inf [ text "Schritt" <+> toDoc (step s) 
        <+> parens (text "in Frame" <+> toDoc f)
        <+> text "beginne Auswertung von" <+> toDoc e
        ]
    v <- C.mapContT ( S.mapStateT $ nested 2 ) a
    inf [ text "Ergebnis von Schritt" 
          <+> toDoc (step s)        
          <+> text "ist" <+> toDoc v
        ]
    return v

inf docs = C.lift $ S.lift $ inform $ vcat docs
rej docs = C.lift $ S.lift $ reject $ vcat docs

evaluate exit f e = traced f e $ case e of
    ConstInteger i -> return $ ValInt i
    Ref n -> C.lift $ get f n
    Program {} -> do
        return $ ValClosure { link = f, body = e }
    App fun args -> do
        cls <- evaluate exit f fun
        case cls of
            ValClosure { body = Program tns body, link = g } -> do
                vs <- forM args $ evaluate exit f
                h <- C.lift $ Up.Store.frame f g
                assign_parameters h tns vs
                block exit h body 
            _ -> rej [ text "Closure erwartet:"
                     , toDoc cls
                     ]

assign_parameters h [] [] = return ()
assign_parameters h [] vs = 
    rej [ text "überzählige Argumente:"
        , toDoc vs ]
assign_parameters h tns [] = 
    rej [ text "fehlende Argumente für Parameter:"
        , toDoc tns ]
assign_parameters h (tn : tns) (v: vs) = do
    assign h tn v
    assign_parameters h tns vs


-- | read value, following the static link chain
get :: Int -> Name 
    -> S.StateT Store Reporter Value
get i n = do
    s <- S.get     
    case M.lookup i $ store s of
        Nothing -> S.lift $ reject $ hsep
            [ text "Frame", toDoc i, text "fehlt" ]
        Just f -> case M.lookup n $ values f of
            Nothing -> get ( static_link f ) n
            Just v -> return v
        

-- | put value in current frame 
-- (creates or overwrites a binding)
put :: Int -> Name -> Value
       -> S.StateT Store Reporter ()
put i n v = do
    s <- S.get
    case M.lookup i $ store s of
        Nothing -> S.lift $ reject $ hsep
            [ text "Frame", toDoc i, text "fehlt" ]
        Just f -> do
            let f' = f { values = M.insert n v $ values f }
            S.put $ s { store = M.insert i f' $ store s }

    
