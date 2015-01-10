{-# language TemplateHaskell #-}

module Geo.Program.Reader where

import Geo.Program.AST

import Autolib.Reader hiding ((<|>), many)
import Control.Applicative hiding ( Const )

instance (Reader v) => Reader (Exp v) where
    reader = my_braces ( Block <$> many (reader <* my_semi)
                         <*> (my_reserved "return" *> reader <* my_semi ))
         <|> do
             f <- reader
             curried_args <- many $ my_parens (my_commaSep reader)
             return $ foldl ( \ f a -> Apply f a ) (Ref f) curried_args

derives [makeReader] [''Type]

instance (Reader v) => Reader (Decl v) where
    reader = do
        tn <- reader
        ( do my_reservedOp "=" ; b <- reader ; return $ Decl tn Nothing $ Just b )
          <|> ( do fps <- my_parens ( my_commaSep reader ) ; b <- reader ; return $ Decl tn (Just fps) (Just b) )
          <|> ( return $ Decl tn Nothing Nothing )

instance Reader v => Reader (Typed v) where
    reader = Typed <$> reader <*> reader
    
