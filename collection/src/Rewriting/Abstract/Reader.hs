module Rewriting.Abstract.Reader where

import Rewriting.Abstract.Data

import Autolib.Reader
import Text.Parsec.Expr

import Control.Applicative ((<$>))

instance Reader Prop where
    reader = 
      let atomic =  
                ( do my_reserved "not" ; Not <$> atomic )
            <|> ( PropParens <$> my_parens reader )
            <|> prop1 
            <|> prop2
          conj = And <$> sepBy1 atomic (my_symbol "&&")
          disj = Or  <$> sepBy1 conj   (my_symbol "||")
      in  disj

prop1 = 
    let unary s f = do my_reserved s ; f <$> my_parens reader
    in  foldr1 (<|>) $ do 
            o <- [ minBound .. maxBound ] 
            return $ unary (show o) (Prop1 o)
prop2 = 
    let binary s f = do my_reserved s ; uncurry f <$> reader
    in  foldr1 (<|>) $ do 
            o <- [ minBound .. maxBound ] 
            return $ binary (show o) (Prop2 o)

instance Reader Exp where
    reader = 
        let unary s f = do my_symbol s ; return $ Op1 f
            binary s f = do my_symbol s ; return $ Op2 f
            atomic = ( ExpParens <$> my_parens reader )
                <|> ( Ref <$> reader )
        in  buildExpressionParser 
              [ [ Postfix $ unary "^-" Inverse 
                , Postfix $ unary "^+" Transitive_Closure
                , Postfix $ unary "^*" Transitive_Reflexive_Closure
                ]
              , [ Prefix $ unary "complement" Complement ]
              , [ Infix ( binary "." Product ) AssocLeft ]
              , [ Infix ( binary "&" Intersection ) AssocLeft ]
              , [ Infix ( binary "+" Union ) AssocLeft
                , Infix ( binary "-" Difference ) AssocLeft
                ]
              ] atomic
              

