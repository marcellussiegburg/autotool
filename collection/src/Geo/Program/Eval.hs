-- | this is the general evaluation part
-- that handles bindings and function calls.
-- geometry specific implementations are elsewhere


module Geo.Program.Eval where

import Prelude hiding (Num (..))
import Polynomial.Class

import Geo.Domain
import qualified Geo.Program.Type as G
import Geo.Program.Value

import Autolib.ToDoc
import Autolib.Reporter

import qualified Data.Map.Strict as M

import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative


-- | from a value representation to a type representation       
typeOf v = case v of
    Boolean {} -> BooleanT
    Number {} -> NumberT
    Point {} -> PointT
    Line {} -> LineT
    Circle {} -> CircleT
    Angle {} -> AngleT
    Function res args _ -> FunctionT res args

-- | from abstract syntax to type representation    
liftT t = case t of
    G.Boolean -> BooleanT
    G.Number -> NumberT
    G.Point -> PointT
    G.Line -> LineT
    G.Circle -> CircleT
    G.Angle -> AngleT

getType (G.Typed t n) = liftT t
getName (G.Typed t n) = n


informed exp action = do
    inf $ text "expression" <+> toDoc exp
    val <- mapWriterT ( mapStateT (nested 4) ) action
    inf $ text "has value" <+> toDoc val
    return val

inf d = lift $ lift $ inform d
rej d = lift $ lift $ reject d

-- | check (at run time)
-- that action yiels result of specified type.
-- fail (in the monad) if type is wrong.
assert_type
  :: Type
     -> Eval k s (Value d s)
     -> Eval k s (Value d s)
assert_type t action = do
    v <- action
    when ( typeOf v /= t ) $ rej $ vcat
        [ text "types do not agree:"
        , text "expected:" <+> toDoc t
        , text "but got: " <+> toDoc (typeOf v)
        ]  
    return v

mkEnv kvs = M.fromList kvs

-- | evaluate expression in environment.
eval
  :: (ToDoc d, ToDoc n, Ord n, Domain s d, ToDoc s)
     => Env n d s
     -> G.Exp n
     -> Eval d s (Value d s) 
eval env exp = informed exp $ case exp of
    G.Const i -> return $ Number $ fromInteger i
    G.Ref n -> ref env n
    G.Apply f args -> apply env f args
    G.Block decls result -> block env decls result

block env decls result = do
    env' <- foldM decl env decls
    eval env' result


curry3 f a b c = f (a,b,c)

-- | process a declaration.
-- return the new environment
-- (where the declared name is bound).

decl :: (Ord n, ToDoc n, ToDoc d, Domain s d, ToDoc s)
        => Env n d s -> G.Decl n -> Eval d s (Env n d s)

-- this is the case where we introduce a free object     
decl env (G.Decl tn Nothing Nothing) = do
    v <- case getType tn of
      NumberT -> Number <$> number
      PointT -> curry Point <$> number <*> number
      LineT -> curry3 Line  <$> number <*> number <*> number
      AngleT -> curry Angle <$> number <*> number
      t -> rej $ vcat
          [ text "cannot declare unknown of type" <+> toDoc t ]
    return $ M.insert (getName tn) v env

decl env (G.Decl tn Nothing (Just b)) = do
    v <- assert_type (getType tn) $ eval env b
    return $ M.insert (getName tn) v env

decl env (G.Decl tn (Just args) (Just b)) = do
    let v = Function (getType tn) (map getType args)
          $ \ xs -> let env' = M.union (mkEnv $ zip (map getName args) xs) env
                    in  eval env' b    
    return $ M.insert (getName tn) v env

-- | look up name in environment
ref :: ( Ord n , ToDoc n )
       => Env n d s -> n -> Eval d s (Value d s)
ref env n = case M.lookup n env of
            Nothing -> rej $ vcat
                       [ text "name" <+> toDoc n
                       , text "not bound in environment"
                       ]
            Just v -> return v 

-- | apply function to arguments
apply :: (ToDoc d, ToDoc n, Ord n, ToDoc s, Domain s d)
     => Env n d s
     -> G.Exp n -> [ G.Exp n ]
     -> Eval d s (Value d s)
apply env f args = do
        fv <- eval env f
        case fv of
            Function t ts work -> do
              when (length ts /= length args) $ rej $ vcat
                [ text "argument list length mismatch"
                ]  
              argvs <- forM (zip ts args) $ \ (t,a) -> do
                  assert_type t $ eval env a
              assert_type t $ work argvs
            _ -> rej $ vcat
                   [ text "value" <+> toDoc fv
                   , text "is not a function"
                   ]  
