{-# language TemplateHaskell #-}

module Inter.Wert where

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Wert = Reset 
          | Pending
          | No 
          | Ok Integer
          | Okay { punkte :: Integer, size_ :: Integer } 
    deriving ( Eq, Ord, Typeable )

okay p s = Okay { punkte = p, size_ = s }

size w = case w of 
    Ok p -> p
    Okay { } -> size_ w
    _ -> 0

is_okay w = case w of
    Ok _ -> True
    Okay {} -> True
    _ -> False

-- nice try, but constructors cannot be deprecated it seems
-- {-# DEPRECATED Ok "Use 'Okay' resp. smart constructor 'ok' instead" #-}

ok :: Integer -> Wert
ok s = Okay { punkte = 1, size_ = s }

-- | replace OK constructor by Okay
renovate :: Wert -> Wert
renovate w = case w of
    Ok s -> ok s
    _    -> w

$(derives [makeReader, makeToDoc] [''Wert])

