{-# language TemplateHaskell #-}

module Geo.Program.Eval where

import qualified Geo.Program.Type as G

import Autolib.ToDoc
import Autolib.Reporter

import qualified Data.Map.Strict as M

data Value k
    = Point (k,k)
    | Line (k,k,k)
    | Function Type [ Type ] ( [Value k] -> Reporter (Value k) )

data Type = PointT | LineT | FunctionT Type [Type]
    deriving (Eq)

derives [makeToDoc] [''Value,''Type]

typeOf v = case v of
    Point {} -> PointT
    Line {} -> LineT
    Function res args _ -> FunctionT res args

liftT t = case t of
    G.Point -> PointT ; G.Line -> LineT

getType (G.Typed t n) = liftT t
getName (G.Typed t n) = n

type Env n d = M.Map n (Value d)

informed exp action = do
    inform $ text "expression" <+> toDoc exp
    val <- nested 4 action
    inform $ text "has value" <+> toDoc val
    return val

assert_type t action = do
    v <- action
    when ( typeOf v /= t ) $ reject $ vcat
        [ text "types do not agree:"
        , text "expected:" <+> toDoc t
        , text "but got: " <+> toDoc (typeOf v)
        ]  
    return v

mkEnv kvs = M.fromList kvs

eval :: (ToDoc c, ToDoc d, ToDoc n, Ord n)
     => Env n d -> G.Exp n c -> Reporter (Value d)
eval env exp = informed exp $ case exp of
    G.Ref n -> ref env n
    G.Apply f args -> apply env f args
    G.Block decls result -> block env decls result

block env decls result = do
    env' <- foldM decl env decls
    eval env' result

decl env (G.Decl tn Nothing b) = do
    v <- assert_type (getType tn) $ eval env b
    return $ M.insert (getName tn) v env

decl env (G.Decl tn (Just args) b) = do
    let v = Function (getType tn) (map getType args)
          $ \ xs -> let env' = M.union (mkEnv $ zip (map getName args) xs) env
                    in  eval env' b    
    return $ M.insert (getName tn) v env
    
ref env n = case M.lookup n env of
            Nothing -> reject $ vcat
                       [ text "name" <+> toDoc n
                       , text "not bound in environment"
                       ]
            Just v -> return v 

apply env f args = do
        fv <- eval env f
        case fv of
            Function t ts work -> do
              when (length ts /= length args) $ reject $ vcat
                [ text "argument list length mismatch"
                ]  
              argvs <- forM (zip ts args) $ \ (t,a) -> do
                  assert_type t $ eval env a
              assert_type t $ work argvs
            _ -> reject $ vcat
                   [ text "value" <+> toDoc fv
                   , text "is not a function"
                   ]  
