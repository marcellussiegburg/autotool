-- | this is the general evaluation part
-- that handles bindings and function calls.
-- geometry specific implementations are elsewhere


module Geo.Program.Eval where

import qualified Geo.Program.Type as G
import Geo.Program.Value

import Autolib.ToDoc
import Autolib.Reporter

import qualified Data.Map.Strict as M

import Control.Monad.Trans
import Control.Monad.Writer



typeOf v = case v of
    Boolean {} -> BooleanT
    Number {} -> NumberT
    Point {} -> PointT
    Line {} -> LineT
    Circle {} -> CircleT
    Function res args _ -> FunctionT res args

liftT t = case t of
    G.Boolean -> BooleanT
    G.Number -> NumberT
    G.Point -> PointT
    G.Line -> LineT
    G.Circle -> CircleT

getType (G.Typed t n) = liftT t
getName (G.Typed t n) = n


informed exp action = do
    lift $ inform $ text "expression" <+> toDoc exp
    val <- mapWriterT ( \ act -> nested 4 act ) action
    lift $ inform $ text "has value" <+> toDoc val
    return val

assert_type :: Type -> Eval (Value d) -> Eval (Value d)
assert_type t action = do
    v <- action
    when ( typeOf v /= t ) $ lift $ reject $ vcat
        [ text "types do not agree:"
        , text "expected:" <+> toDoc t
        , text "but got: " <+> toDoc (typeOf v)
        ]  
    return v

mkEnv kvs = M.fromList kvs

eval :: (ToDoc d, ToDoc n, Ord n)
     => Env n (Value d) -> G.Exp n -> Eval (Value d)
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
            Nothing -> lift $ reject $ vcat
                       [ text "name" <+> toDoc n
                       , text "not bound in environment"
                       ]
            Just v -> return v 

apply :: (ToDoc d, ToDoc n, Ord n)
     => Env n (Value d)
     -> G.Exp n -> [ G.Exp n ]
     -> Eval (Value d)
apply env f args = do
        fv <- eval env f
        case fv of
            Function t ts work -> do
              when (length ts /= length args) $ lift $ reject $ vcat
                [ text "argument list length mismatch"
                ]  
              argvs <- forM (zip ts args) $ \ (t,a) -> do
                  assert_type t $ eval env a
              assert_type t $ work argvs
            _ -> lift $ reject $ vcat
                   [ text "value" <+> toDoc fv
                   , text "is not a function"
                   ]  
