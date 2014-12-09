{-# language NoMonomorphismRestriction #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}

module Polynomial.Unary.Reader where

import Polynomial.Class
import Polynomial.Patch

import qualified Prelude
import Prelude ( return, ($), (.), Eq, Ord, read, Bool(..) )
import Polynomial.Unary.Data

import Autolib.Reader
import Control.Applicative ((<$>),(<*>), (<*), (*>) )
import Control.Lens (over)
import Control.Monad ( mzero )

import qualified Text.Parsec.Expr as E

instance (Reader c, Ring c) => Reader (Poly c) where
    reader = ( poly . Prelude.map (\(Id c,Id e) -> (c,e)) . unP ) 
           <$> reader 


{-

instance (Ring c , Reader c) => Reader (P c Integer) where
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

-}

instance (Pattern c,  Reader c, Ring (Base c)
         , Pattern e, Base e ~ Integer, Reader e) 
         => Reader (P c e) where
    reader = do
        t <- factorP ; ts <- many $ opP <*> factorP
        return $ P $ t : ts

-- | this parses "+" / "-" and returns id / negate function
opP =   do my_reservedOp "+" ; return $ Prelude.id
   <|> do my_reservedOp "-" 
          return $ \ (c,e) -> (pmap negate c, e)

-- | this parses  c * x ^ e
-- where "x" is fixed (the name of the (single) variable)
-- "c *" is optional (if missing, then c = one)
-- "^ e" is optional (if missing, then e = 1)
-- "* x ^ e" is optinal (if missine, then e = 0)
factorP = do
    (cpresent, c) <- option (False, inject one)
        $  (,) True <$> reader 
    spresent <- option False 
        $  my_reservedOp "*" *> return True
    let xe = my_reserved "x" 
          *> option (inject one) (my_reservedOp "^" *> reader )
        cont = case (cpresent, spresent) of
            (True, False) -> return $ inject zero
            (True, True) -> xe
            (False, False) -> xe
            (False, True) -> mzero
    (,) c <$> cont

natural :: Parser Integer
natural = do
    ds <- many1 digit ; my_whiteSpace
    return $ read ds
