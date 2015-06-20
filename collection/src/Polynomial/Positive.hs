-- | find positive values of a polynomial.
-- typical application: the Matiyasevich polynomial
-- where the positive values are exactly the primes.
-- http://primes.utm.edu/glossary/xpage/MatijasevicPoly.html

{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}

module Polynomial.Positive where

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader hiding ((<|>))
import Text.Parsec.Expr
import Autolib.TES.Identifier 

import Challenger.Partial
import Inter.Types hiding (Var)

import Autolib.FiniteMap
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Typeable
import Control.Lens
import Control.Applicative

data Polynomial_Positive = Polynomial_Positive
  deriving ( Typeable, Show, Read )

data P = Var Identifier | Lit Integer | Parens P 
       | Plus P P | Minus P P | Times P P | Power P Int
 deriving Typeable
 

instance ToDoc P where
  toDoc e = case e of
    Var i -> toDoc i ; Lit i -> toDoc i
    Parens e -> parens $ toDoc e
    Plus  x y -> toDoc x <+> text "+" <+> toDoc y
    Minus x y -> toDoc x <+> text "-" <+> toDoc y
    Times x y -> toDoc x <+> text "*" <+> toDoc y
    Power x y -> toDoc x <+> text "^" <+> toDoc y

instance Reader P where
  reader = buildExpressionParser
    [ [ Postfix up ]
    , [ Infix (my_reservedOp "*" *> return Times) AssocLeft ]
    , [ Infix (my_reservedOp "+" *> return Plus) AssocLeft
      , Infix (my_reservedOp "-" *> return Minus) AssocLeft
      ]
    ] atomic  

up = do
  my_reservedOp "^" 
  e <- reader
  return $ \ x -> Power x e

atomic = Parens <$> my_parens reader
  <|> Lit <$> reader
  <|> Var <$> reader

type S = FiniteMap Identifier Integer

instance OrderScore Polynomial_Positive where
  scoringOrder _ = Decreasing

instance Partial Polynomial_Positive P S where

  describe _ p = vcat
    [ text "Assign nonnegative integers to variables"
    , text "such that the value of the polynomial is strictly positive:"
    , nest 4 $ toDoc p
    ]

  initial _ p = M.fromList
     $ zip (S.toList $ variables p)
     $ concat $ repeat [ 2, 1, 0 ]

  partial _ p s = do
    let negative = M.filter (< 0) s
    when (not $ M.null negative) $ reject $ vcat
      [ text "these assignments are negative:"
      , nest 4 $ toDoc negative
      ]
    let needed = variables p
        provided = M.keysSet s
        missing = S.difference needed provided
    when (not $ S.null missing) $ reject $ vcat
      [ text "these variables are unassigned:"
      , nest 4 $ toDoc missing
      ]
      
  total _ p s = do
    let v = evaluate p s
    inform $ text "value is" <+> toDoc v
    when (v <= 0) $ reject $ text "value is not strictly positive"
    
variables :: P -> S.Set Identifier
variables p =  case p of
   Var v -> S.singleton v
   Lit l -> S.empty
   Parens x -> variables x
   Plus x y -> S.union (variables x) (variables y)
   Minus x y -> S.union (variables x) (variables y)
   Times x y -> S.union (variables x) (variables y)
   Power x y -> variables x

-- | FIXME: should have a more generic type,
-- and go to the polynomial library
evaluate :: P -> S -> Integer
evaluate p s = case p of
  Var v -> s M.! v
  Lit l -> l
  Parens x -> evaluate x s
  Plus x y -> evaluate x s + evaluate y s
  Minus x y -> evaluate x s - evaluate y s
  Times x y -> evaluate x s * evaluate y s
  Power x e -> evaluate x s ^ e

  
primes :: P
primes = read "(k+2)*(1 - (w*z+h+j-q)^2 - ((g*k+2*g+k+1)*(h+j)+h-z)^2 - (2*n+p+q+z-e)^2 - (16*(k+1)^3*(k+2)*(n+1)^2+1-f^2)^2 - (e^3*(e+2)*(a+1)^2+1-o^2)^2 - ((a^2-1)*y^2+1-x^2)^2 - (16*r^2*y^4*(a^2-1)+1-u^2)^2 - (((a+u^2*(u^2-a))^2 -1)*(n+4*d*y)^2 + 1 - (x+c*u)^2)^2 - (n+l+v-y)^2 - ((a^2-1)*l^2+1-m^2)^2 - (a*i+k+1-l-i)^2 - (p+l*(a-n-1)+b*(2*a*n+2*a-n^2-2*n-2)-m)^2 - (q+y*(a-p-1)+s*(2*a*p+2*a-p^2-2*p-2)-x)^2 - (z+p*l*(a-p)+t*(2*a*p-p^2-1)-p*m)^2)"
 
make_fixed :: Make
make_fixed = direct Polynomial_Positive primes

   
