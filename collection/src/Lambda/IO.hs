{-# LANGUAGE PatternSignatures #-}
{-# LANGUAGE PatternSignatures #-}
{-# language PatternSignatures #-}

module Lambda.IO where


import Lambda.Data
import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader

import Text.ParserCombinators.Parsec ( parse )

instance ToDoc Lambda where
    toDocPrec p t = case t of
        Variable v -> toDoc v
        Apply {} -> 
            let ( fun, args ) = applications t
            in docParen ( p > 0 ) 
                   $ toDocPrec 9 fun <+> fsep ( map ( toDocPrec 9 ) args )
        Abstract {} -> 
            let ( vars, body ) = abstractions t
            in docParen ( p > 0 ) 
                   $ text "\\"
                   <+> fsep ( map toDoc vars )
                   <+> text "->"
                   <+> toDocPrec 0 body

instance Show Lambda where show = render . toDoc
                           
instance Reader Lambda where
    reader = application

atomic :: Parser Lambda 
atomic = my_parens ( reader :: Parser Lambda )
     <|> try abstraction
     <|> do ( this :: Identifier ) <- reader ; return $ Variable this

abstraction :: Parser Lambda
abstraction = do
    option "\\" $ my_symbol "\\"
    vars <- many ( reader :: Parser Identifier )
    my_reserved "->"
    body <- reader
    return $ abstract ( vars , body )

application :: Parser Lambda
application = do
    f : args <- many1 ( atomic :: Parser Lambda )
    return $ apply ( f , args )

