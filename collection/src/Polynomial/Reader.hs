{-# language NoMonomorphismRestriction #-}
{-# language FlexibleInstances #-}

module Polynomial.Reader where

import Polynomial.Class
import qualified Prelude
import Prelude ( return, ($), Eq, Ord, read )

import Polynomial.Data

import Autolib.Reader
import Control.Applicative ((<$>),(<*>))
import Control.Lens ( (^.) )

import qualified Text.Parsec.Expr as E

data Exp r v = Const r | Fact (Factor v )
           | Negate (Exp r v)
           | Plus (Exp r v) (Exp r v)
           | Minus (Exp r v) (Exp r v)
           | Times (Exp r v) (Exp r v)
           | Divide (Exp r v) (Exp r v)

expr = E.buildExpressionParser table term

term    =  my_parens expr
          <|> ( Const <$> fromInteger <$> natural )
         <|> ( Fact <$> reader )

table   = [ [prefix "-" Negate  ]
            , [binary "*" Times E.AssocLeft, binary "/" Divide E.AssocLeft ]
            , [binary "+" Plus E.AssocLeft, binary "-" Minus   E.AssocLeft ]
            ]

          
binary  name fun assoc = E.Infix (do{ my_symbol name; return fun }) assoc
prefix  name fun       = E.Prefix (do{ my_symbol name; return fun })

instance (Ring r, Reader r, Ord v, Reader v) 
         => Reader (Poly r v) where 
    reader = expr Prelude.>>= buildR

buildR e = case e of
    Const c -> return $ constant c
    Fact f -> return $ poly [(one, mono[f])]
    Negate p -> negate <$> buildR p
    Plus p q -> (+) <$> buildR p <*> buildR q
    Minus p q -> (-) <$> buildR p <*> buildR q
    Times p q -> (*) <$> buildR p <*> buildR q
    Divide p q -> Prelude.fail "cannot divive, coeffienct domain is not a field"

instance (Ord v, Reader v) 
         => Reader (Poly Rational v) where 
    reader = expr Prelude.>>= buildF

buildF :: Ord v => Exp Rational v -> Parser (Poly Rational v)
buildF e = case e of
    Const c -> return $ constant c
    Fact f -> return $ poly [(one, mono[f])]
    Negate p -> negate <$> buildF p
    Plus p q -> (+) <$> buildF p <*> buildF q
    Minus p q -> (-) <$> buildF p <*> buildF q
    Times p q -> (*) <$> buildF p <*> buildF q
    Divide p q -> do
        (a, n) <- splitAbsolute <$> buildF q 
        if null n then do b <- buildF q ; return $ divF b a
            else Prelude.fail "cannot divide by non-constant"
        
instance Reader v => Reader (Factor v) where
    reader = do 
        v <- reader
        e <- option 1 $ do my_symbol "^" ; fromInteger <$> natural
        return $ factor v e

natural :: Parser Integer
natural = do
    ds <- many1 digit ; my_whiteSpace
    return $ read ds
