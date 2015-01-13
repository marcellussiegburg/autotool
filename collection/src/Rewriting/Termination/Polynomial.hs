{-# language DeriveDataTypeable #-}
{-# language DoAndIfThenElse #-}

module Rewriting.Termination.Polynomial where

import qualified Polynomial.Type as P

import Polynomial.Class
import qualified Prelude
import Prelude hiding 
    ( Num (..), sum, product, (^), (/), Integer, null, gcd, divMod, div, mod )

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import Control.Lens
import Control.Monad ( when )
import Control.Applicative ( (<$>) )
import Data.Typeable
import Data.List ( transpose )


-- | encodes position of argument (starting from 1)
newtype X = X Int deriving (Eq, Ord, Typeable)
instance ToDoc X where toDoc (X i) = text "x" <> toDoc i
instance Show X where show = render . toDoc 
instance Reader X where 
    reader = do 
        Autolib.Reader.char 'x' ; ds <- many1 digit ; my_whiteSpace
        return $ X $ read ds


substitute :: P.Poly Integer X -> [ P.Poly Integer X ] 
           -> P.Poly Integer X
substitute f gs = sum $ do
    (c,m) <- P.terms f
    return $ product $ fromInteger c : do
        (X i,e) <- P.factors m
        return $ (gs !! pred i) ^ fromIntegral e
    
projection :: Int -> P.Poly Integer X
projection to = P.variable $ X to

must_be_monotone f arity p = do
    void $ forM ( P.terms p ) $ \ (c,m) -> do
        when (c < 0) $ reject $ hsep
             [ text "interpretation of symbol" <+> toDoc f <+> text "of arity" <+> toDoc arity
             , text "is" <+> toDoc p
             , text "is not monotone since part" 
             , toDoc (c, m)
             , text "has negative coefficient"
             ]
    void $ forM [ 1 .. arity ] $ \ i -> do
        let occurs_isolated = or $ do 
                (c,m) <- P.terms p 
                return $ case P.factors m of
                    [ (v,e) ] | v  == X i -> True
                    _ -> False
        when (not occurs_isolated) $ reject $ vcat
            [ text "interpretation of symbol" <+> toDoc f <+> text "of arity" <+> toDoc arity
            , text "is" <+> toDoc p
            , text "cannot prove monotonicity in argument" <+> toDoc (X i)
            , text "since it does not occur isolated in some monomial" 
            ]

weakly_greater :: P.Poly Integer X -> P.Poly Integer X -> Bool
weakly_greater p q = and $ do
    (c,m) <- P.terms $ p - q
    return $ c > 0

-- | precondition:  weakly_greater p q == True 
strictly_greater :: P.Poly Integer X -> P.Poly Integer X -> Bool
strictly_greater p q = 
    p ^. P.absolute  > q ^. P.absolute 

