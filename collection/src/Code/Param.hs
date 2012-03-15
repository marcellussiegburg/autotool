{-# LANGUAGE TemplateHaskell #-}

module Code.Param where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Set

import Data.Typeable

data Encode c = Encode c deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Encode])

instance ToDoc c => Show (Encode c) where show = render . toDoc
                          

data Decode c = Decode c  deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Decode])

instance ToDoc c => Show (Decode c) where show = render . toDoc

data Compress c = Compress c  deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Compress])

instance ToDoc c => Show (Compress c) where show = render . toDoc


data ( Reader [a], ToDoc [a], Ord a ) => Config a =
     Config { alphabet :: Set a
	    , length_range :: ( Int, Int )
	    }
     deriving ( Typeable )

example :: Config Char
example = Config { alphabet = mkSet [ 'a' .. 'g' ]
		 , length_range = (10, 20)
		 }

$(derives [makeReader, makeToDoc] [''Config])

-- Local variables:
-- mode: haskell
-- End:
