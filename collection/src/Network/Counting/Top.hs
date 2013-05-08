{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}

module Network.Counting.Top where

import Network.Counting.Data
import Network.Counting.Test

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import qualified Challenger as C
import Inter.Types
import Autolib.Size
import Data.Typeable

data Config = Config { width :: Int
                     , max_balancers :: Int
                     }
    deriving ( Typeable )

example_config = 
    Config { width = 4, max_balancers = 6 }

data Counting_Network = Counting_Network
    deriving ( Typeable )

$(derives [makeToDoc,makeReader] 
          [''Counting_Network, ''Config])

instance Show Counting_Network where 
    show = render . toDoc
instance Show Config where 
    show = render . toDoc

instance OrderScore Counting_Network where
    scoringOrder _ = Increasing

instance C.Partial Counting_Network Config Network where

    describe p i = vcat 
        [ text "Geben Sie ein Zählnetzwerk"
        , text "für" <+> toDoc ( width i ) <+> text "Eingänge"
        , text "mit höchstens" <+> toDoc (max_balancers i) <+> text "Verteilern an."
        ]

    initial p i = 
        let pairs (x:y:zs) = (x,y) : pairs zs
            pairs _ = []
            ws = map Wire [ 1 .. width i ]
        in  Network $ pairs ws ++ pairs (tail ws )

    partial p i n @ (Network bs) = 
        return ()
