{-# language TemplateHaskell #-}

module Geo.Program.Reader where

import Geo.Program.AST

import Autolib.Reader hiding ((<|>), many)
import Control.Applicative hiding ( Const )

instance (Reader v) => Reader (Exp v) where
    reader = my_braces ( Block <$> many (reader <* my_semi)
                         <*> (my_reserved "return" *> reader))
         <|> Apply <$> reader <*> my_parens (my_commaSep reader )
         <|> Ref <$> reader

derives [makeReader] [''Type]

instance (Reader v) => Reader (Decl v) where
    reader =
        Decl <$> reader <*>
        (     ( my_reservedOp "=" *> return Nothing )
          <|> ( Just <$> my_parens ( my_commaSep reader ) ) )
        <*> reader

instance Reader v => Reader (Typed v) where
    reader = Typed <$> reader <*> reader
    
