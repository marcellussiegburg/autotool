{-# language TemplateHaskell #-}
{-# language NoMonomorphismRestriction #-}

module Geo.Program.Reader where

import Geo.Program.AST

import Autolib.Reader hiding ((<|>), many)
import Text.Parsec.Expr
import Control.Applicative hiding ( Const )

instance (Reader v) => Reader (Exp v) where
    reader = buildExpressionParser
        [ [ binary "*" Multiply , binary "/" Divide ]
        , [ binary "+" Add , binary "-" Subtract ]
        ] atomic

binary name op = 
    Infix ( my_reservedOp name *> return ( \ l r -> Oper l op r ) ) AssocLeft

atomic =     my_parens ( Parens <$> reader )
         <|> Const <$> reader
         <|> my_braces ( Block <$> program )
         <|> do
             f <- reader
             curried_args <- many $ my_parens (my_commaSep reader)
             return $ foldl ( \ f a -> Apply f a ) (Ref f) curried_args

program :: Reader v => Parser (Program v)
program = many reader

derives [makeReader] [''Type]

instance (Reader v) => Reader (Statement v) where
    reader = statement <* my_semi

statement = declare
        <|> ( Emit <$> reader <*> reader )
        <|> ( Return <$> (my_reserved "return" *> reader ))

declare = do
        tn <- reader
        ( do my_reservedOp "=" ; b <- reader ; return $ Decl tn Nothing $ Just b )
          <|> ( do fps <- my_parens ( my_commaSep reader ) ; b <- reader ; return $ Decl tn (Just fps) (Just b) )
          <|> ( return $ Decl tn Nothing Nothing )


instance Reader v => Reader (Typed v) where
    reader = Typed <$> reader <*> reader
    
derives [makeReader] [''Kind]
