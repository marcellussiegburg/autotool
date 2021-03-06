{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Pump.REG.Type where

import Autolib.Size
import Autolib.Hash
import Autolib.Reader
import Autolib.ToDoc


import Autolib.Util.Splits
import Control.Monad (guard)
import Data.Typeable
import GHC.Generics

data Zerlegung = Zerlegung
	       { u :: String, v :: String, w :: String }
     deriving (Eq, Ord, Typeable, Generic)

$(derives [makeReader, makeToDoc] [''Zerlegung])

instance Show Zerlegung where show = render . toDoc

instance Hash Zerlegung 

--------------------------------------------------------------------------

-- | alle Zerlegungen mit |uv| <= n, |v| > 0
zerlegungen :: String -> Int -> [ Zerlegung ]
zerlegungen p n = do
    ( ab, c ) <- take n $ splits p
    ( a, b  ) <- splits ab
    guard $ not (null b)
    return $ Zerlegung { u = a, v = b, w = c }

aufpump :: Zerlegung -> Int -> String
aufpump g i =  u g
	 ++ concat ( replicate i $ v g )
	 ++ w g





