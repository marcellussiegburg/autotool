{-# language TemplateHaskell #-}

module Up.Store where

import Up.Type
import Autolib.ToDoc

import Autolib.FiniteMap () -- for instances
import qualified Data.Map as M

import qualified Control.Monad.State as S

data Value = ValInt Integer
           | ValClosure Exp (Maybe Int)


data Frame = Frame { values :: M.Map Name Value
                   , dynamic_link :: Maybe Int
                   , static_link :: Maybe Int
                   }

data Store = Store { store :: M.Map Int Frame }

derives [makeToDoc] [''Value, ''Frame, ''Store ]

-- | allocate new frame
frame :: M.Map Name Value
      -> Maybe Int -> Maybe Int
      -> S.State Store Int
frame v dyn stat = do
    let f = Frame { values = v, dynamic_link = dyn
                  , static_link = stat
                  }
    s <- S.get
    let n = M.size $ store s 
    S.put $ s { store = M.insert n f $ store s }
    return n

