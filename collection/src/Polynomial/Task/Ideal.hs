{-# language RebindableSyntax #-}
{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}

module Polynomial.Task.Ideal where

import qualified Prelude  
import Prelude
  hiding ( map, Num (..), (^), sum, Integer, Rational, fromInteger)

import Polynomial.Class
import Polynomial.Type

import Polynomial.Grobner.Compute

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.TES.Identifier
import Autolib.Size

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Data.Typeable
import Data.Maybe
import Data.List ( tails, maximumBy, minimumBy )
import Data.Function (on)
import Control.Applicative ((<$>),(<*>))
import System.Random

data Polynomial_Ideal_Membership
   = Polynomial_Ideal_Membership deriving (Typeable,Show,Read)

type P = Poly Rational Identifier

instance OrderScore Polynomial_Ideal_Membership where
  scoringOrder _ = Increasing

instance Measure Polynomial_Ideal_Membership ([P], P) [P] where
  measure _ (fs,g) cs =
    sum $ fmap (Prelude.fromIntegral . nterms) cs

instance Partial Polynomial_Ideal_Membership ([P], P) [P] where
  describe _ (fs,g) = vcat
    [ text "Show that the polynomial  g = " </> toDoc g
    , text "is contained in the ideal generated by  fs = " </> toDoc fs
    , text "by giving a list  cs  of polynomials"
    , text "such that  g = c_1 * f_1 + ... + c_n * f_n"
    , text "--"
    , text "Solution size (for highscore) is:  sum of number of terms in  fs."
    ]
    
  initial _ (fs,g) = reverse fs

  partial _ (fs,g) cs = do
    mequal ( text "length cs" , length cs )
           ( text "length fs" , length fs )

  total _ (fs, g) cs = do
    mequal ( text "sum_i  c_i * f_i", sum $ zipWith (*) cs fs )
           ( text "g", g )

x = variable $ mk 0 "x" :: P
y = variable $ mk 0 "y" :: P
z = variable $ mk 0 "z" :: P

make_fixed :: Make
make_fixed =  direct Polynomial_Ideal_Membership
  ([ x^2 + y - 1, x*y - x ], 3*x*y^2 - 4*x*y + x + 1 )

make_quiz :: Make
make_quiz =  quiz Polynomial_Ideal_Membership conf0

-- | the roller will build a set of random polynomials,
-- apply some Buchberger steps (towards constructing
-- a Grobner basis, but without necessarily finding one)
-- and then pick any polynomial that was produced.
-- there are @num_candidates@ experiments,
-- and we pick one that come near the @target_size_goal@ .
-- size of polynomial is number of terms.

data Config =
    Config { variables :: [Identifier]
           , base_size :: Int
           , coefficient_range :: (Integer,Integer)  
           , degree_range :: (Int,Int)
           , size_range :: (Int,Int)
           , buchberger_steps_range :: (Int,Int)
           , target_size_goal :: Int
           , num_candidates :: Int  
            }
    deriving Typeable

conf0 = Config
  { variables = read "[x,y,z]"
  , base_size = 3
  , coefficient_range = (-5,5)
  , degree_range = (0,3)
  , size_range = (2, 3)
  , buchberger_steps_range = (5, 10)
  , target_size_goal = 4
  , num_candidates = 10
  }


rmono conf = do
  c <- randomRIO $ coefficient_range conf
  ves <- forM ( variables conf ) $ \ v -> do
    e <- randomRIO $ degree_range conf
    return (v, e)
  return ( c , mono [ factor v e | (v,e) <- ves , e > 0 ] )  

rpoly conf = do
  s <- randomRIO $ size_range conf
  ms <- replicateM s $ rmono conf
  return $ poly ms

roll_one conf = do
  fs <- replicateM (base_size conf) $ rpoly conf
  let (lo,hi) = buchberger_steps_range conf
  case result $ algorithm63s option0{max_steps=Just hi} fs of
    Just s | not $ Prelude.null $ trace s -> do
      i <- randomRIO (0, length (trace s) - 1)
      let (c,g) = trace s !! i
      return $ Just ( (negate c, nterms g, Prelude.sum $ fmap Prelude.abs $ coefficients g )
        , (fmap (map fromInteger) fs, map fromInteger g))
    _ -> return Nothing
    
coefficients p = fmap fst $ terms p

roll conf = do
  cs <- replicateM (num_candidates conf) $ roll_one conf
  case catMaybes cs of
    [] -> roll conf
    ds -> do
      let p@(_, inst) = minimumBy (compare `on` fst) ds
      return inst

instance Generator Polynomial_Ideal_Membership Config ( [P], P ) where
  generator _ conf key = roll conf

instance Project Polynomial_Ideal_Membership ( [P], P )([P],P) where
  project _  = id

mequal (d1,x1) (d2,x2) = do
  inform $ vcat
    [ text "is" <+> d1 </> text "with value" <+> toDoc x1
    , text "equal to" <+> d2 </> text "with value" <+> toDoc x2
    , text "?"  
    ]  
  case x1 == x2 of
    True  -> do inform $ text "Yes."
    False -> do reject $ text "No."

$(derives [makeToDoc, makeReader] [''Config])
    