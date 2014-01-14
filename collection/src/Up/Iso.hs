{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}

module Up.Iso where

import Up.Store

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import qualified Data.Map as M
import Control.Monad

data Links = Links [ (Int, Int) ]
    deriving (Eq, Ord, Typeable)

derives [makeReader, makeToDoc] [''Links]

nicely (Links xs) = vcat $ flip map ( zip [1 :: Int ..] xs )
    $ \ (i,(d,s)) -> hsep 
       [ text "Frame" , toDoc i,  text ":"
      , text "dynamischer Vorgänger", toDoc d, text ","
      , text "statischer Vorgänger", toDoc s, text ";"
      ]



iso :: Links -> Store -> Reporter ()
iso (Links dss) t = 
    forM_ (zip [1..] dss) $ \ (i,(d,s)) -> do
        case M.lookup i (store t) of
            Nothing -> reject $ text "es existiert kein Frame mit Nummer" <+> toDoc i
            Just f -> do
                when ( d /= dynamic_link f )
                    $ whine "dynamische" d f
                when ( s /= static_link f )
                    $ whine "statische" s f

whine tag v f = reject $ vcat 
    [ text "der" <+> text tag <+> text "Vorgänger"
      <+> text "soll" <+> toDoc v <+> text "sein:"
    , toDoc f
    ]
