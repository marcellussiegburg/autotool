module Polynomial.Reader where

import Polynomial.Data
import Autolib.Reader
import Control.Applicative ((<$>),(<*>))

instance Reader v => Reader (Factor v) where
    reader = do 
        v <- reader 
        e <- ( do my_symbol "^" ; reader ) <|> return 1
        return $ Factor { _var = v, _expo = e }

instance (Ord v, Reader v) => Reader (Mono v) where
    reader = mono <$> my_sepBy ( my_symbol "*" ) reader

instance (Ord v, Reader v) => Reader (Poly v) where
    reader = 
        let term = (,) <$> ( reader <|> return 1 ) <*> reader
        in  poly <$> do
                t <- term
                let op =     do my_symbol "+" ; return id
                         <|> do my_symbol "-" ; return $ \ (c,m)-> (negate c, m)
                ts <- many $ op <*> term
                return $ t : ts

my_sepBy sep item = 
        do x <- item ; xs <- many $ do { sep ; item } ; return $ x : xs
    <|> return []

