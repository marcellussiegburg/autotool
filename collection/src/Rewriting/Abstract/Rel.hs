module Rewriting.Abstract.Rel where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Autolib.TES
import Autolib.TES.Identifier
import qualified Autolib.Relation as R

import Control.Applicative ( (<$>) )

newtype Rel = Rel ( R.Type Int Int )

instance ToDoc Rel where
    toDoc (Rel r) = braces $ hsep $ punctuate comma 
          $ map toDoc $ R.pairs r

instance Reader Rel where
    reader = Rel <$> R.make 
       <$> ( my_braces $ my_commaSep $ reader )

