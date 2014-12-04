{-# language NoMonomorphismRestriction #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}

module Polynomial.Unary.Reader where

import Polynomial.Class
import Polynomial.Patch
import qualified Prelude
import Prelude ( return, ($), Eq, Ord, read )
import Polynomial.Unary.Data

import Autolib.Reader
import Control.Applicative ((<$>),(<*>), (<*), (*>) )
import Control.Lens (over)

import qualified Text.Parsec.Expr as E

instance ( Ring c , Reader c  ) => Reader (P c Integer) where
    reader = do
        t <- factorI ; ts <- many $ opI <*> factorI
        return $ P $ t : ts

opI =   do my_reservedOp "+" ; return $ Prelude.id
   <|> do my_reservedOp "-" ; return $ \ (c,e) -> (negate c, e)



factorI :: (Ring c, Reader c) => Parser (c,Integer)
factorI = do
    f <- option one $ reader <* my_reservedOp "*"
    e <- option 0 $ my_reserved "x" *> option 1 ( my_reservedOp "^" *> reader )
    return (f,e)


instance (Base (Patch c) ~ c, Pattern (Patch c), Ring c, Reader c) => Reader (P (Patch c) (Patch Integer)) where
    reader = do
        t <- factorP ; ts <- many $ opP <*> factorP
        return $ P $ t : ts

opP =   do my_reservedOp "+" ; return $ Prelude.id
   <|> do my_reservedOp "-" 
          return $ \ (c,e) -> 
            ( case c of
                This i -> This $ negate i
                Any -> Any
            , e )

-- factorP :: ( Reader c) => Parser (c, Patch Integer)
factorP = do
    f <- option (inject one) $ reader <* my_reservedOp "*"
    e <- option (This 0) $ my_reserved "x" *> option (This 1) ( my_reservedOp "^" *> reader )
    return (f,e)

natural :: Parser Integer
natural = do
    ds <- many1 digit ; my_whiteSpace
    return $ read ds
