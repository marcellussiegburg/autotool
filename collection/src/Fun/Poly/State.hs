module Fun.Poly.State where

import Fun.Poly.Type
import Fun.Poly.Cache

import Autolib.Reader
import Autolib.ToDoc hiding ( empty )

import Machine.History

import Data.Typeable

data State = State { schritt :: Int
		   , todo :: [ Exp ]
		   , stack :: [ Integer ]
		   -- die werden nicht angezeigt
		   , past :: [ State ]
		   , cache :: Cache Exp Integer
		   }
     deriving Typeable

instance History State where 
    history = past

essence s = ( todo s, stack s )

instance Eq State where 
    x == y = essence x == essence y
instance Ord State where 
    x `compare` y = essence x `compare` essence y

instance ToDoc State where
    toDocPrec d (State aa ab ac ad ae) = docParen (d >= 10)
	      (named_dutch_record (text "State")
	       [ text "schritt" <+.> equals <+.> toDocPrec 0 aa
	       , text "todo" <+.> equals <+.> toDocPrec 0 ab
	       , text "stack" <+.> equals <+.> toDocPrec 0 ac
		-- text "past" <+.> equals <+.> toDocPrec 0 ad,
		-- text "cache" <+.> equals <+.> toDocPrec 0 ae
	       ])

instance Show State where show = render . toDoc
