{-# language NoMonomorphismRestriction #-}

module CSP.Property.Right_Linear where

import CSP.Syntax

import Autolib.Reporter

import Autolib.ToDoc
import Control.Monad ( guard, sequence, void )
import Data.Maybe ( isJust )

ok = isJust . result . check

check :: ToDoc a
        => Process a 
        -> Reporter ()
check p = silent $ do
    inform $ text "sind alle Rekursionen rechts-linear?"
    nested 2 $ void $ sequence $ do 
        s @ (Fix q) <- subs p
        return $ do
            inform $ text "untersuche" </> toDoc s
            local q
    
-- | return True if the free Point does occur
-- (note: ignores nested Points, on purpose) 
local r = nested 2 $ do    
    inform $ text "Teilterm" </> toDoc r
    nested 2 $ case r of        
        Stop -> return False
        Pre x p -> local p
        Ext p q -> local2 p q
        Int p q -> local2 p q
        Seq p q -> do
            bad <- local p
            when bad $ reject
                 $ text "ist nicht rechtslinear (Rekursion im linken Argument)"
            local q
        Par s p q -> do
            bad <- local2 p q
            when bad $ reject 
                $ text "ist nicht rechtslinear (Rekursion unter Par)"
            return False     
        Fix p -> return False
        Point -> return True

local2 p q = do
    subs <- forM [ p, q ] local
    return $ or subs


