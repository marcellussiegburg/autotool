module CSP.STS.Bisi.Relation where

import qualified Autolib.Relation as R
import qualified Autolib.Set as S
import qualified Data.Set as S

import Autolib.Reader
import Autolib.ToDoc

relation :: ( Reader a, Reader b, Ord a, Ord b )
         => Parser ( R.Type a b )
relation = do
    r : rs <- flip sepBy1 ( my_symbol "+" ) atom
    return $ foldr R.plus r rs
    
atom :: ( Reader a, Reader b, Ord a, Ord b )
         => Parser ( R.Type a b )
atom =  try ( my_braces $ fmap R.make $ my_commaSep reader )
    <|> do 
        x <- set ; my_symbol "x" ; y <- set
        return $ R.make $ S.toList $ S.cross x y
    
set :: ( Reader a, Ord a )     
    => Parser ( S.Set a )
set = my_braces $ fmap S.fromList $ my_commaSep reader
