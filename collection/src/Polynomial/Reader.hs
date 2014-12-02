{-# language NoMonomorphismRestriction #-}

module Polynomial.Reader where

import Polynomial.Class
import qualified Prelude
import Prelude ( return, ($), Eq, Ord, read )
import Polynomial.Data
import Polynomial.Op

import Autolib.Reader
import Control.Applicative ((<$>),(<*>))

import qualified Text.Parsec.Expr as E

instance (Ring r, Reader r, Ord v, Reader v) 
    => Reader (Poly r v) where reader =  expr

expr = E.buildExpressionParser table term

term    =  my_parens expr 
          <|> ( fromInteger <$> natural )
         <|> do f <- reader ; return $ poly [(one,mono [f])]

table   = [ [prefix "-" negate  ]
            , [binary "*" (*) E.AssocLeft ]
            , [binary "+" (+) E.AssocLeft, binary "-" (-)   E.AssocLeft ]
            ]
          
binary  name fun assoc = E.Infix (do{ my_symbol name; return fun }) assoc
prefix  name fun       = E.Prefix (do{ my_symbol name; return fun })

instance Reader v => Reader (Factor v) where
    reader = do 
        v <- reader ; e <- option 1 $ do my_symbol "^" ; natural
        return $ Factor { _var = v, _expo = e }

natural :: Parser Integer
natural = do
    ds <- many1 digit ; my_whiteSpace
    return $ read ds
