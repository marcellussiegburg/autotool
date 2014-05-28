{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module DPLL.Pattern where

import DPLL.Data

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import Control.Applicative ( (<$>) )
import System.Random

data Pattern v = Any | This v 


instance ToDoc v => ToDoc (Pattern v) where
    toDoc p = case p of
        Any -> text "*"
        This x -> toDoc x

instance Reader v => Reader (Pattern v) where
    reader = const Any <$> my_symbol "*"
         <|> This <$> reader


class Matches p d where
    matches :: p -> d -> Reporter ()

instance (Eq d, ToDoc d) => Matches d d where
    matches x y = when (x /= y) $ reject $ vcat
            [ text "Objekt" <+> toDoc y
            , text "paßt nicht zum Muster" <+> toDoc x
            ]

instance (Eq d, ToDoc d, Matches p d) => Matches (Pattern p) d where
    matches p y = case p of
        Any -> return ()
        This x -> matches x y

instance (ToDoc d, ToDoc p, Matches p d) => Matches [p] [d] where
    matches [] [] = return ()
    matches (p:ps) [] = reject $ vcat
       [ text "leere Liste" 
       , text "paßt nicht zu nicht-leerem Muster" <+> toDoc (p:ps)
       ]
    matches [] (d:ds) = reject $ vcat
       [ text "Objekt" <+> toDoc (d:ds)
       , text "paßt nicht zu leerem Muster"
       ]
    matches (p:ps) (d:ds) = do
        matches p d
        matches ps ds


class Punch d p where
    punch :: d -> IO p

punches :: Punch d p => Int -> d -> IO p
punches k d = 
    if k > 0 then punch d >>= punches (k-1) else return d

-- | introduce exactly one pattern
instance Punch d (Pattern d) where
    punch d = return Any

instance Punch (Pattern d) (Pattern d) where
    punch d = return Any

instance Punch d p => Punch [d] [p] where
    punch xs = do
        i <- randomRIO (0, length xs - 1)
        let (pre, this : post) = splitAt i xs
        that <- punch this
        return $ pre ++ that : post

