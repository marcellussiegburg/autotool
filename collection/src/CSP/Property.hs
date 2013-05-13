{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language LambdaCase #-}

module CSP.Property where

import CSP.Syntax

import Autolib.Reporter

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Control.Monad ( guard, sequence, void )

match :: (ToDoc a, Eq a) =>  Process a -> Process a -> Reporter ()
match pat val = case (pat, val) of
    (Undefined, _) -> check_defined val
    (Stop, Stop) -> return ()
    (Pre x p, Pre y q) | x == y -> match p q
    (Ext p q, Ext r s) -> do match p r ; match q s
    (Int p q, Int r s) -> do match p r ; match q s
    (Seq p q, Seq r s) -> do match p r ; match q s
    (Par c p q, Par d r s) | c == d -> do match p r ; match q s
    (Fix p, Fix q) -> match p q
    (Star p, Star q) -> match p q
    (Point,Point) -> return ()
    _ -> reject $ vcat
        [ text "Der Teilausdruck" <+> toDoc val
        , text "paßt nicht zum Muster" <+> toDoc pat
        ]
    
check_defined val = do
    let bad = filter ( \ case Undefined -> True ; _ -> False ) $ subs val
    when (not $ null bad) $ reject $ text "Der Ausdruck enthält Undefined."


data Iteration = Iteration_Star | Iteration_Fixpoint 
    deriving ( Eq, Ord, Typeable )

derives [makeReader,makeToDoc] [''Iteration]

explain_iteration :: Iteration -> Reporter ()
explain_iteration it = inform 
      $ text "Iteration soll durch" 
    <+> text ( case it of Iteration_Star -> "Stern" ; Iteration_Fixpoint -> "Fixpunkt")
    <> text "-Operator" 
    <+> text "ausgedrückt werden."
             
check_iteration :: ToDoc a => Iteration -> Process a -> Reporter ()
check_iteration it p = do
    let whine prop = do
            let bad = filter prop $ subs p
            when ( not $ null bad ) $ reject $ vcat
                [ text "diese Teilterme benutzen nicht erlaubte Operatoren:"
                , nest 4 $ toDoc bad 
                ]
    case it of
        Iteration_Star -> whine ( \ case Fix _ -> True ; Point -> True ; _ -> False )
        Iteration_Fixpoint -> whine ( \ case Star _ -> True ; _ -> False )
        

guarded :: ToDoc a
        => Process a 
        -> Reporter ()
guarded p = silent $ do
    inform $ text "sind alle Rekursionen bewacht (guarded)?"
    nested 2 $ void $ sequence $ do 
        s @ (Fix q) <- subs p
        return $ do
            inform $ text "untersuche" </> toDoc s
            check_guard q
    
check_guard r = nested 2 $ do    
    inform $ text "Teilterm" </> toDoc r
    nested 2 $ case r of        
        Stop -> inform $ text "benötigt keinen Wächter"
        Pre x p -> inform $ text "hat Wächter" <+> toDoc x
        Ext p q -> void $ forM [ p, q ] check_guard
        Int p q -> void $ forM [ p, q ] check_guard
        Seq p q -> do
            gp <- wrap $ check_guard p
            case gp of
                Just () -> inform $ text "erstes Argument ist bewacht: OK"
                Nothing -> do
                    inform $ text "erstes Argument ist nicht bewacht: untersuche zweites Argument"
                    check_guard q
        Par s p q -> void $ forM [ p, q ] check_guard
        Fix p -> inform $ text "wird separat untersucht"
        Point -> reject $ text "ist nicht bewacht"
