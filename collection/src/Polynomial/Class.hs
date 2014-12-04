{-# language FlexibleInstances #-}
{-# language NoMonomorphismRestriction #-}
{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}

module Polynomial.Class where

import Prelude hiding (Num(..), (/), (^), div, divMod, mod
       , Integer, Rational)
import qualified Prelude

import Autolib.ToDoc
import Autolib.Reader

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Typeable

infixl 7  *, /
infixl 6  +, -

class Prelude.Eq r => Ring r where
    zero :: r
    negate :: r -> r
    (+) :: r -> r -> r
    one :: r
    (*) :: r -> r -> r
    fromInteger :: Prelude.Integer -> r

a - b = a + negate b

(^) :: Ring r => r -> Prelude.Integer -> r
a ^ b | b Prelude.>= 0 = 
    if b Prelude.== 0 then one
    else let (d,m) = Prelude.divMod b 2 
             c = a ^ d
         in  (c * c) * case m of 0 -> one ; 1 -> a

sum = Prelude.foldr (+) zero
product = Prelude.foldr (*) one

type Integer = Prelude.Integer

instance Ring Integer where
    zero = 0; (+) = (Prelude.+) ; negate = Prelude.negate 
    one = 1 ; (*) = (Prelude.*)
    fromInteger i = i

data Ratio z = z :% z 
     deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, Typeable)

type Rational = Ratio Integer

instance ToDoc z => ToDoc (Ratio z) where
    toDoc (a :% b) = parens $ hsep [ toDoc a, text ":%", toDoc b ]
instance (Normalize_Fraction z, Reader z) => Reader (Ratio z) where
    reader = my_parens $ (%) <$> reader <* my_reservedOp ":%" <*> reader 

class Normalize_Fraction z where
    (%) :: z -> z -> Ratio z

instance Normalize_Fraction Integer where
    p % q = let g = Prelude.gcd p q 
            in Prelude.div p g :% Prelude.div q g

instance ( Normalize_Fraction z, Ring z ) 
         => Ring (Ratio z) where
    zero = zero % one 
    (a :% b) + (c :% d) = (a * d + b * c) % (b * d)
    negate (a :% b) = negate a % b
    one = one % one 
    (a :% b) * (c :% d) = (a * c) % (b * d)
    fromInteger i = fromInteger i % one

data Complex r = r :+ r
     deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, Typeable)

instance ToDoc r => ToDoc (Complex r) where
    toDoc (a :+ b) = parens $ hsep [ toDoc a, text ":+", toDoc b ]
instance (Reader r) => Reader (Complex r) where
    reader = my_parens $ (:+) <$> reader <* my_reservedOp ":+" <*> reader 

instance Ring r => Ring ( Complex r ) where
    zero = zero :+ zero
    (a :+ b) + (c :+ d) = (a + c) :+ (b + d)
    negate (a :+ b) = (negate a :+ negate b)
    one = one :+ zero
    (a :+ b) * (c :+ d) = (a * c - b * d) :+ (a * d + b * c)
    fromInteger i = fromInteger i :+ zero

class Ring r => Euclidean_Ring r where
    norm :: r -> Prelude.Maybe Integer
    div :: r -> r -> r

mod p q = let d = div p q in p - d * q
divMod p q = (div p q, mod p q)

instance Euclidean_Ring Integer where
    norm = Prelude.Just Prelude.. Prelude.abs
    div = Prelude.div

instance Euclidean_Ring (Complex Integer) where
    norm (a :+ b) = Prelude.Just ( a * a + b * b )
    div (a :+ b) (c :+ d) =
        let r = fromInteger
            s :+ t = (r a :+ r b) / ( r c :+ r d)
            near :: Rational -> Integer
            near s = let half = one % (one + one)
                         (e :% d) = s + half
                     in  div e d
        in  near s :+ near t

class Ring r => Field r where
    (/) :: r -> r -> r

instance ( Normalize_Fraction a, Ring a )
         => Field (Ratio a) where
    (a :% b) / (c :% d) = (a * d) % (b * c)    

instance Field a => Field (Complex a) where
    (a :+ b) / (c :+ d) = 
        let (p :+ q) = (a :+ b) * (c :+ negate d)
            n = c * c + d * d
        in  (p / n) :+ (q / n)

data Step r = Step { quotient :: r, remainder :: r }
    deriving Typeable

derives [makeReader, makeToDoc] [''Step]

gcd_steps a b = 
    let helper a b = 
            if b Prelude.== zero then []
            else let (d, m) = divMod a b
            in Step { quotient = d, remainder = m }
               :  helper b m
    in  Step { quotient = zero , remainder = a }
    : Step { quotient = zero , remainder = b }
    : helper a b

gcd a b = let (g,p,q) = egcd a b in (p,q)

egcd a b =
    if b Prelude.== 0 then (a, 1, 0)
    else let (d, m) = divMod a b
             (g, p', q') = egcd b m
         -- p' * b + q' * m = g
         -- p' * b + q' * (a - d*b)
         -- q'*a + (p'- q'* d) b 
         in  (g, q', p' - q' * d )
