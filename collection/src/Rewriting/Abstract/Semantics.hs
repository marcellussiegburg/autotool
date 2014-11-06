{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Rewriting.Abstract.Semantics where

import Rewriting.Abstract.Syntax 
import Rewriting.Abstract.Rel

import Autolib.FiniteMap
import qualified Data.Map as M

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Relation.Ops

import Prelude hiding ( exp )

type Env = M.Map Identifier Rel

boolean :: (p -> Reporter Bool)
        -> Env -> Boolean p -> Reporter Bool
boolean atomic env b = case b of
    And bs -> and <$> forM bs ( boolean atomic env )
    Or  bs -> or  <$> forM bs ( boolean atomic env )
    Not b  -> not <$> boolean atomic env b
    Atomic a -> traced a $ atomic a

traced a m = do
    v <- m
    inform $ vcat [ text "value of" </> toDoc a 
                  , text "is" </> toDoc v
                  ]

prop :: Env -> Prop -> Reporter Bool
prop env p = case p of
    Prop1 p x -> do
        r <- exp env x
        case p of
            Reflexive -> 
                return $ r == R.reflex r
            Transitive ->
                return $ r == R.trans r
            Symmetric ->
                return $ r == R.inverse r
            Antisymmetric ->
                return $ subseteq (R.interesection r $ R.inverse r) (R.flat $ R.source r)
            SN -> do
                return $ nullR $ R.intersection 
                   (R.flat $ R.source r) (R.trans r)
            WN -> do
                let m = R.maxima r
                    p = R.pre_simages (R.trans r) m
                return $ p == R.domain r
            CR -> do
                let t = R.trans r ; t' = R.inverse t
                return $ subseteq ( S.times t' t )
                                  ( S.times t t' )
            WCR -> do
                let t = R.trans r ; t' = R.inverse t
                return $ subseteq ( S.times (R.inverse r) r )
                                  ( S.times t t' )
--    | Unique_Normalforms | UN
--    | Unique_Normalforms_wrt_Conversion | UNC

    Prop2 p x y -> do
         r <- exp env x ; s <- exp env y
         case p of
              Equals -> return $ r == s
              Subseteq -> return $ subseteq r s

subseteq r s = (R.plus r s) == s
nullR = null . R.pairs

exp :: Env -> Exp -> Reporter Rel
exp env x = case x of
    Identifier i -> case M.lookup env i of
        Nothing -> reject $ text "not bound"
        Just v -> return v
    Op1 o x -> do
        r <- exp env x
        case o of
            Inverse -> return $ R.inverse r
            Complement -> return $ R.complement r
            Transitive_Closure -> return $ R.trans r
            Transitive_Reflexive_Closure -> 
                return $ R.reflex_trans r
    Op2 o x y -> do
        r <- exp env x ; s <- exp env y
        case o of
            Union -> return $ R.plus r s
            Intersection -> return $ R.intersection r s
            Difference -> return $ R.difference r s
            Product -> return $ R.times r s

derives [makeReader, makeToDoc] 
    [''Boolean,''Prop, ''Prop1, ''Prop2, ''Exp, ''Op1, ''Op2]
