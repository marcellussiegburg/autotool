module Fourier_Motzkin.Reader where

import Fourier_Motzkin.Data 

import Autolib.Reader
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Data.Maybe (isJust)

instance (Ord v, Reader v) => Reader (Linear v) where
    reader =  Linear <$> M.fromListWith (+) <$> many ( do
        ms <- optionMaybe $ do {my_symbol "+" ;return 1} <|> do {my_symbol "-"; return (-1) } 
        mc <- optionMaybe $ reader
        mm <- optionMaybe (my_symbol "*") 
        mv <- optionMaybe reader
        guard $ or [ isJust mc, isJust mv ]
        guard $ isJust mm == and [ isJust mc, isJust mv ]
        return (mv, maybe 1 id ms * maybe 1 id mc) )

instance (Ord v, Reader v) => Reader (Atom v) where
    reader = do 
        my_symbol "0" 
        ( do my_symbol "<=" ; NonNegative <$> reader ) <|> (do my_symbol "<" ; Positive <$> reader )
