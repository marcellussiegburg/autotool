{-# language MultiParamTypeClasses #-}

module While.Check where

import While.Type
import RAM.Builtin
import While.Property

import qualified Machine.Numerical.Config as C

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Set

instance C.Check Property Program where
    check No_While = no_while
    check No_Loop = no_loop
    check No_IfZ = no_ifz
    check ( Max_Register m ) = max_register m
    check ( Builtins bs ) = builtins ( mkSet bs )

builtins :: Set Builtin -> Program -> Reporter ()
builtins allowed p = do
    inform $ text "erlaubt sind diese Builtin-Prozeduren:"
    inform $ nest 4 $ toDoc allowed
    let used = do
            b @ Assign {} <- subprograms p
            return b
        you = mkSet $ do Assign _ f _ <- used ; return f
    inform $ text "Sie benutzen:" <+> toDoc you
    let wrong = minusSet you allowed
    assert ( isEmptySet wrong ) $ text "sind alle zugelassen?"
    mapM_ check_arity used

check_arity :: Program -> Reporter ()
check_arity b@(Assign res name args) = do
    let ( ar, fun ) = get name
    when ( length args /= ar )
         $ reject $ vcat [ text "Fehler in Anweisung" <+> toDoc b
                         , fsep [ text "Das Builtin" , toDoc ( name )
                                , text "benötigt genau" ,toDoc ar
                                , text "Argumente"
                                ]
                         , fsep [ text "aber Sie verwenden"
                                , toDoc $ length $ args 
                                ]
                         ]

no_while :: Program -> Reporter ()
no_while p = do
    let whiles = do
            w @ ( While {} ) <- subprograms p
            return w
    when ( not $ null whiles ) $ reject
         $ text "Sie dürfen kein While benutzen."

no_loop :: Program -> Reporter ()
no_loop p = do
    let loops = do
            w @ ( Loop {} ) <- subprograms p
            return w
    when ( not $ null loops ) $ reject
         $ text "Sie dürfen kein Loop benutzen."

no_ifz :: Program -> Reporter ()
no_ifz p = do
    let ifzs = do
            w @ ( IfZ {} ) <- subprograms p
            return w
    when ( not $ null ifzs ) $ reject
         $ text "Sie dürfen kein IfZ benutzen."

max_register :: Int -> Program -> Reporter ()
max_register m p = do
  let wrong_befs = do
        b <- subprograms p
        guard $ not $ case b of
          Inc r -> r <= m
          Dec r -> r <= m
          Assign r _ args -> maximum (r:args) <= m
          IfZ r _ _ -> r <= m
          While r _ -> r <= m
          Loop r _ -> r <= m
          _ -> True
        return b
  when (not $ null wrong_befs) $ reject
     $ text "Diese Teilprogramme benutzen verbotene Register:"
       </> toDoc wrong_befs
       
