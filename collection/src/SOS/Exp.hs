{-# language GADTs #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language DeriveDataTypeable #-}
{-# language StandaloneDeriving #-}

module SOS.Exp where

import SOS.State

import Prelude hiding ( Left, Right, LT, EQ, GT )

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.Size

import Text.Parsec.Expr

import Control.Applicative ( (<$>), (<*>) )
import Data.Typeable
import Data.Data ( Data )
import Data.Generics.Schemes ( gsize )

data Place = Left | Mid | Right 
    deriving ( Enum, Eq, Ord, Typeable, Data )

instance Reader Place where
    reader = foldr1 (<|>) 
           $ map ( \ (c, s) -> const c <$> my_reserved s )
           [ (Left, "left"), (Mid, "mid"), (Right, "right") ]

instance ToDoc Place where
    toDoc p = case p of
        Left -> text "left" ; Mid -> text "mid" ; Right -> text "right"

my_int = fromIntegral <$> my_integer

instance Reader (Exp Int) where
    reader =  Const <$> my_int 
       <|> do my_reserved "top" ; Top <$> reader

instance ToDoc (Exp Int) where
    toDoc e = case e of
        Const i -> toDoc i
        Top p -> text "top" <+> toDoc p

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

instance ToDoc (Exp Bool) where
    toDocPrec p e = parens $ case e of
        Compare l c r -> toDoc l <+> toDoc c <+> toDoc r
        Null p -> text "null" <+> toDoc p
        Not e -> text "!" <+> toDoc e
        And l r -> toDoc l <+> text "&&" <+> toDoc r
        Or l r -> toDoc l <+> text "||" <+> toDoc r

instance Reader (Exp Action) where
    reader = do my_reserved "move" ; Move <$> reader <*> reader
        <|>  do my_reserved "if" ; p <- reader
                my_reserved "then" ; t <- reader
                my_reserved "else" ; e <- reader
                return $ If p t e
instance ToDoc (Exp Action) where
    toDoc e = case e of
        Move l r -> text "move" <+> toDoc l <+> toDoc r
        If p t e -> vcat 
               [ text "if" <+> toDoc p 
               , text "then" <+> toDoc t
               , text "else" <+> toDoc e
               ]

data Action -- just used as type argument
    deriving ( Typeable )
deriving instance Data Action

data Compare = LT | LE | EQ | GE | GT
    deriving ( Typeable, Data )

instance Reader Compare where
    reader = foldr1 (<|>) 
           $ map ( \ (c, s) -> const c <$> my_symbol s )
           [ (LT, "<"), (LE, "<="), (EQ, "==")
           , (GE, ">="), (GT, ">") 
           ] 

instance ToDoc Compare where
    toDoc c = case c of
        LT -> text "<" ; LE -> text "<=" 
        EQ -> text "=="
        GE -> text ">=" ; GT -> text ">"

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
  deriving (Typeable )

instance Size (Exp Int) where
    size (Top p) = 2 
    size (Const i) = 1
instance Size (Exp Bool) where
    size (Compare l c r) = 2 + size l + size r
    size (Null p) = 2
    size (Not e) = 1 + size e
    size (And l r) = 1 + size l + size r
    size (Or l r) = 1 + size l + size r
instance Size (Exp Action) where
    size (Move l r) = 3
    size (If p t e) = 1 + size p + size t + size e

program0 :: Int -> Program
program0 _ = If (Null Left)
              (If (Null Mid) (Move Right Mid) (Move Mid Right))
           (If (Null Mid) (Move Left Right)
               (If (Compare (Top Left) LT (Top Mid))
                   (Move Mid Left) (Move Left Mid)))
         

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
