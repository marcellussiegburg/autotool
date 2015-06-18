{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language NoMonomorphismRestriction #-}
{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables #-}

module Polynomial.Class where

import Prelude hiding (Num(..), (/), (^)
       , div, divMod, mod, gcd
       , Integer, Rational)
import qualified Prelude

import Autolib.ToDoc
import Autolib.Reader

import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.SmallCheck
import Test.SmallCheck.Series
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Typeable

import Control.DeepSeq

infixl 7  *, /
infixl 6  +, -

-- * the class and its properties
       
class Prelude.Eq r => Ring r where
    zero :: r
    negate :: r -> r
    (+) :: r -> r -> r
    one :: r
    (*) :: r -> r -> r
    fromInteger :: Prelude.Integer -> r
    -- | this does not really belong here. it is used just for printing (the "-" sign)
    negative :: r -> Prelude.Bool
    
associative f a b c = f (f a b) c == f a (f b c)
commutative f a b = f a b == f b a
left_distributive f g a b c = 
    f (g a c) (g b c) == g (f a b) c
right_distributive f g a b c = 
    f (g a b) (g a c) == g a (f b c)

ring_spec (_ :: r ) = describe "Ring properties" $ do
  describe "addition" $ do
    it "left-zero" $ property $ \ a -> zero + (a ::r) == a
    it "right-zero" $ property $ \ a -> (a ::r) + zero == a
    it "is associative" $ property 
       $ associative ((+) :: r -> r -> r)
    it "is commutative" 
       $ property $ commutative ((+) :: r -> r -> r)
  describe "subtraction" $ do
    it "inverse" $ property $ \ a -> (a :: r) - a == zero
  describe "multiplication" $ do
    it "left-one" $ property $ \ a -> one * (a ::r) == a
    it "right-one" $ property $ \ a -> (a ::r) * one == a
    it "is associative" $ property 
       $ associative ((*) :: r -> r -> r)
  describe "addition/multiplication" $ do
    it "left-distributes" $ property 
        $ left_distributive ((+)::r->r->r) ((*)::r->r->r)
    it "right-distributes" $ property 
        $ right_distributive ((+)::r->r->r) ((*)::r->r->r)

spec d = hspecWith 
    $ defaultConfig { configSmallCheckDepth = d }

a - b = a + negate b

(^) :: Ring r => r -> Prelude.Integer -> r
a ^ b | b Prelude.>= 0 = 
    if b Prelude.== 0 then one
    else let (d,m) = Prelude.divMod b 2 
             c = a ^ d
         in  (c * c) * case m of 0 -> one ; 1 -> a

sum = Prelude.foldr (+) zero
product = Prelude.foldr (*) one

-- * instances for basic numerical types
        
type Integer = Prelude.Integer

instance Ring Integer where
    zero = 0; (+) = (Prelude.+) ; negate = Prelude.negate 
    one = 1 ; (*) = (Prelude.*)
    fromInteger i = i
    negative = (< 0)

instance Ring Int where
    zero = 0; (+) = (Prelude.+) ; negate = Prelude.negate 
    one = 1 ; (*) = (Prelude.*)
    fromInteger i = Prelude.fromInteger i
    negative = (< 0)

-- * ratios
    
data Ratio z = z :% z 
     deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, Typeable)

instance NFData z => NFData (Ratio z) where
  rnf (p :% q) = rnf p `seq` rnf q `seq` ()

instance (Serial m z, Ring z, Normalize_Fraction z ) 
         => Serial m (Ratio z) where
    series = (%) 
        <$> series 
        <~> ((\ x -> if x == zero then one else x)<$> series)

type Rational = Ratio Integer

-- FIXME: this instance is questionable. Print a rational as "(p/q)" (with parentheses)
-- or as  "p" (without) (which is interpreted as   "(p/1)"
instance (Ring z, ToDoc z) => ToDoc (Ratio z) where
    toDoc (a :% b) = if b == one then toDoc a else parens $ hsep [ toDoc a, text "/", toDoc b ]

instance (Ring z, Normalize_Fraction z, Reader z) => Reader (Ratio z) where
    reader = ( my_parens $ (%) <$> reader <* my_reservedOp "/" <*> reader )
        <|>  ( % one ) <$> reader 

class Normalize_Fraction z where
    (%) :: z -> z -> Ratio z

instance Normalize_Fraction Integer where
    p % q = 
        if q < 0 then negate p % negate q
        else let g = Prelude.gcd p q 
             in Prelude.div p g :% Prelude.div q g

instance ( Normalize_Fraction z, Ring z ) 
         => Ring (Ratio z) where
    zero = zero % one 
    (a :% b) + (c :% d) = if b == d then (a + c) % d else (a * d + b * c) % (b * d)
    negate (a :% b) = negate a % b
    one = one % one 
    (a :% b) * (c :% d) = (a * c) % (b * d)
    fromInteger i = fromInteger i % one
    negative (a :% b) = negative a -- hmpf

-- * complex numbers
    
data Complex r = r :+ r
     deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, Typeable)

instance NFData r => NFData (Complex r) where
  rnf (p :+ q) = rnf p `seq` rnf q `seq` ()

instance (Serial m z ) 
         => Serial m (Complex z) where
    series = (:+) <$> series <~> series

instance (Ring r, ToDoc r) => ToDoc (Complex r) where
    toDoc (a :+ b) = if b == zero then toDoc a else
        parens $ hsep [ toDoc a, text ":+", toDoc b ]
instance (Ring r, Reader r) => Reader (Complex r) where
    reader = ( my_parens $ (:+) <$> reader <* my_reservedOp ":+" <*> reader )
        <|>  ( :+ zero ) <$> reader

instance Ring r => Ring ( Complex r ) where
    zero = zero :+ zero
    (a :+ b) + (c :+ d) = (a + c) :+ (b + d)
    negate (a :+ b) = (negate a :+ negate b)
    one = one :+ zero
    (a :+ b) * (c :+ d) = (a * c - b * d) :+ (a * d + b * c)
    fromInteger i = fromInteger i :+ zero

-- * Euclidean ring and easy instances
    
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

euclidean_spec (_ :: r ) = 
  describe "Euclidean Ring properties" $ do
    it "norm/remainder" $ property $ \ p q ->
        if q /= (zero :: r) 
        then norm ( mod p q ) < norm q else True

-- * the Field class
  
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

-- * GCD computations
    
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

gcd_steps_spec (_ :: r) = 
    it "gcd_steps_spec" $ property $ \ a b -> 
        let ss = gcd_steps a (b  :: r)
        in  remainder ( last $ init ss ) == gcd a b

gcd a b = let (g,p,q) = egcd a b in g

egcd a b =
    if b Prelude.== zero then (a, one, zero)
    else let (d, m) = divMod a b
             (g, p', q') = egcd b m
         -- p' * b + q' * m = g
         -- p' * b + q' * (a - d*b)
         -- q'*a + (p'- q'* d) b 
         in  (g, q', p' - q' * d )

egcd_spec (_ :: r ) = 
    it "extended euclidean" $ property $ \ a b ->
        let (g, p, q) = egcd a b in (g :: r) == a * p + b * q
