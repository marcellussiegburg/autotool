{-# language GADTs #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}

module SOS.Exp where

import SOS.State

import Prelude hiding ( Left, Right, LT, EQ, GT )

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Text.Parsec.Expr

import Control.Applicative ( (<$>), (<*>) )

data Place = Left | Mid | Right 
    deriving ( Enum, Eq, Ord )

instance Reader Place where
    reader = foldr1 (<|>) 
           $ map ( \ (c, s) -> const c <$> my_reserved s )
           [ (Left, "left"), (Mid, "mid"), (Right, "right") ]

my_int = fromIntegral <$> my_integer

instance Reader (Exp Int) where
    reader =  Const <$> my_int <|> Top <$> reader
instance Reader (Exp Bool) where
    reader = buildExpressionParser
        [ [ Prefix ( const Not <$> my_symbol "!" )  ] 
        , [ Infix ( const And <$> my_symbol "&&" ) AssocLeft ]
        , [ Infix ( const Or <$> my_symbol "||" ) AssocLeft ]
        ]
        ( my_parens reader 
        <|> Compare <$> reader <*> reader <*> reader 
        <|> do my_reserved "null" ; Null <$> reader
        )
instance Reader (Exp Action) where
    reader = do my_reserved "move" ; Move <$> reader <*> reader
        <|>  do my_reserved "if" ; p <- reader
                my_reserved "then" ; t <- reader
                my_reserved "else" ; e <- reader
                return $ If p t e

data Action -- just used as type argument

data Compare = LT | LE | EQ | GE | GT

instance Reader Compare where
    reader = foldr1 (<|>) 
           $ map ( \ (c, s) -> const c <$> my_symbol s )
           [ (LT, "<"), (LE, "<="), (EQ, "==")
           , (GE, ">="), (GT, ">") 
           ] 

data Exp a where
    Move :: Place -> Place -> Exp Action
    Top :: Place -> Exp Int
    Const :: Int -> Exp Int
    Compare :: Exp Int -> Compare -> Exp Int -> Exp Bool
    Null :: Place -> Exp Bool
    Not :: Exp Bool -> Exp Bool
    And :: Exp Bool -> Exp Bool -> Exp Bool 
    Or  :: Exp Bool -> Exp Bool -> Exp Bool 
    If :: Exp Bool -> Exp Action -> Exp Action -> Exp Action

derives [makeToDoc] [''Place,''Compare, ''Exp]

instance Show (Exp Action) where show = render . toDoc

type Program = Exp Action

get :: Place -> State -> Reporter [Int]
get p (State xss) = return $ xss !! fromEnum p
    
move :: Exp Action -> State -> Reporter State
move m @ (Move from to) (State xss) = trace m $ do
    let (pre, this : post) = splitAt (fromEnum from) xss
    (t, xss') <- case this of
        [] -> reject $ text "place is empty"
        t : his -> return $ (t, pre ++ his : post)
    let (pre', that : post') = splitAt (fromEnum to) xss'
    return $ State $ pre' ++ (t : that) : post'

evalInt :: Exp Int -> State -> Reporter Int
evalInt e s = trace e $ case e of
    Const i -> return i
    Top p -> get p s >>= \ case
            [] -> reject $ text "place is empty"
            x:xs -> return x

evalBool :: Exp Bool -> State -> Reporter Bool
evalBool e s = trace e $ case e of
    Null p -> null <$> get p s
    Compare l c r -> do
        let f = case c of
                LT -> (<) ; LE -> (<=) ; EQ -> (==) 
                GE -> (>=) ; GT -> (>)
        f <$> evalInt l s <*> evalInt r s 
    Not e -> not <$> evalBool e s
    And l r -> (&&) <$> evalBool l s <*> evalBool r s
    Or  l r -> (||) <$> evalBool l s <*> evalBool r s

step :: Program -> State -> Reporter State
step p s = trace p $ case p of
    Move {} -> move p s
    If c t e -> evalBool c s >>= \ case
        True  -> step t s
        False -> step e s

trace :: ToDoc a => a -> Reporter b -> Reporter b
trace e a = do
    inform $ toDoc e
    a
