-- | for reading and printing relations
-- in set-like manner, as in   { (1,2), (3,4) }

-- WATCH OUT: with this notation, there is no way
-- to specifiy source and target sets,
-- so these must be repaired manually

module Rewriting.Abstract.Braced where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Autolib.TES
import Autolib.TES.Identifier
import qualified Autolib.Relation as R

import Control.Applicative ( (<$>) )

newtype Braced s t = Braced ( R.Type s t )

instance (Ord s, Ord t, ToDoc s, ToDoc t) 
    => ToDoc (Braced s t) where
    toDoc (Braced r) = braces $ hsep $ punctuate comma 
          $ map toDoc $ R.pairs r

instance (Ord s, Ord t, Reader s, Reader t) 
    => Reader (Braced s t) where
    reader = Braced <$> R.make 
       <$> ( my_braces $ my_commaSep $ reader )

instance Size (Braced s t) where
    size (Braced r) = size r

