{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses  #-}

module Goto.Check where

import Goto.Type
import RAM.Builtin
import Goto.Property

import qualified Machine.Numerical.Config as C

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Set

instance C.Check Property Program where
    check ( Builtins bs ) = builtins ( mkSet bs )

builtins :: Set Builtin -> Program -> Reporter ()
builtins allowed p = do
    inform $ text "erlaubt sind diese eingebauten Funktionen:"
    inform $ nest 4 $ toDoc allowed
    let used = do
            b @ (Assign {} ) <-  p
            return b
    let you = mkSet $ do
            Assign res fun args <- used
            return $ fun
    inform $ text "Sie benutzen:" <+> toDoc you
    let wrong = minusSet you allowed
    assert ( isEmptySet wrong ) $ text "sind alle zugelassen?"
    mapM_ check_arity used

check_arity :: Statement -> Reporter ()
check_arity b@(Assign res name args) = do
    let ( ar, fun ) = get name
    when ( length args /= ar )
         $ reject $ vcat [ text "Fehler in Anweisung" <+> toDoc b 
                         , fsep [ text "Die Funktion" , toDoc ( name  )
                                , text "benötigt genau" ,toDoc ar
                                , text "Argumente"
                                ]
                         , fsep [ text "aber Sie verwenden"
                                , toDoc $ length $ args 
                                ]
                         ]



    
