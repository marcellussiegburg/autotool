{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Rewriting.Abstract.Semantics where

import Rewriting.Abstract.Syntax 
import Rewriting.Abstract.Braced

import Autolib.FiniteMap
import qualified Data.Map as M

import Autolib.TES.Identifier
import Autolib.Reporter
import Autolib.ToDoc

import qualified Autolib.Relation as R
import qualified Autolib.Relation.Prop as R

import Control.Applicative

import Prelude hiding ( exp )

type Env dom = M.Map Identifier ( R.Type dom dom )

prop0 :: Prop
prop0 = 
    let r = Ref $ mk 0 "R" ; s = Ref $ mk 0 "S"
    in  And [ Prop2 Disjoint r s
            , Prop1 Transitive (Op2 Product r s)
            , Not (Prop1 Transitive (Op2 Product s r))
            ]

traced a m = do
    v <- m
    inform $ vcat [ text "value of" </> toDoc a 
                  , text "is" </> toDoc v
                  ]
    return v

prop :: Ord dom 
     => Env dom -> Prop -> Reporter Bool
prop env p = case p of
    And bs -> and <$> forM bs ( prop env )
    Or  bs -> or  <$> forM bs ( prop env )
    Not b  -> not <$> prop env b    
    Prop1 p1 x -> traced p $ do
        r <- exp env x
        case p1 of
            Null -> 
                return $ R.null r
            Total ->
                return $ R.null $ R.complement r
            Reflexive -> 
                return $ R.reflexive r
            Transitive ->
                return $ R.transitive r
            Symmetric ->
                return $ R.symmetric r
            Antisymmetric ->
                return $ R.antisymmetric r 
            SN -> do
                return $ R.null $ R.intersection 
                   (R.flat $ R.source r) (R.trans r)
            WN -> do
                return $ R.source r 
                  == R.pre_simages (R.trans r) ( R.maxima r)
            CR -> do
                let t = R.trans r ; t' = R.inverse t
                return $ R.isSubsetOf ( R.times t' t )
                                  ( R.times t t' )
            WCR -> do
                let t = R.trans r ; t' = R.inverse t
                return $ R.isSubsetOf ( R.times (R.inverse r) r )
                                  ( R.times t t' )
-- TODO:
--    | Unique_Normalforms | UN
--    | Unique_Normalforms_wrt_Conversion | UNC

    Prop2 p2 x y -> traced p $ do
         r <- exp env x ; s <- exp env y
         case p2 of
              Equals -> 
                  return $ r == s
              Subsetof -> 
                  return $ R.isSubsetOf r s
              Disjoint -> 
                  return $ R.null $ R.intersection r s

exp :: Ord dom 
    => Env dom -> Exp -> Reporter (R.Type dom dom)
exp env x = case x of
    Ref i -> case M.lookup i env of
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

