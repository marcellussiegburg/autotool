module CSP.Property.Guarded where

import CSP.Syntax

import Autolib.Reporter

import Autolib.ToDoc
import Control.Monad ( guard, sequence, void )

check :: ToDoc a
        => Process a 
        -> Reporter ()
check p = silent $ do
    inform $ text "sind alle Rekursionen bewacht (guarded)?"
    nested 2 $ void $ sequence $ do 
        s @ (Fix q) <- subs p
        return $ do
            inform $ text "untersuche" </> toDoc s
            local q
    
local r = nested 2 $ do    
    inform $ text "Teilterm" </> toDoc r
    nested 2 $ case r of        
        Stop -> inform $ text "benötigt keinen Wächter"
        Pre x p -> inform $ text "hat Wächter" <+> toDoc x
        Ext p q -> void $ forM [ p, q ] local
        Int p q -> void $ forM [ p, q ] local
        Seq p q -> do
            gp <- wrap $ local p
            case gp of
                Just () -> inform $ text "erstes Argument ist bewacht: OK"
                Nothing -> do
                    inform $ text "erstes Argument ist nicht bewacht: untersuche zweites Argument"
                    local q
        Par s p q -> void $ forM [ p, q ] local
        Fix p -> inform $ text "wird separat untersucht"
        Point -> reject $ text "ist nicht bewacht"
