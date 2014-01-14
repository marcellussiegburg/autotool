{-# language TemplateHaskell #-}

module Up.Store where

import Up.Type
import Autolib.ToDoc

import Autolib.FiniteMap () -- for instances
import qualified Data.Map as M

import qualified Control.Monad.State.Strict as S

data Value = ValUnit
           | ValInt Integer
           | ValClosure Exp Int

data Frame = Frame { values :: M.Map Name Value
                   , dynamic_link :: Maybe Int
                   , static_link :: Maybe Int
                   }

data Store = Store { store :: M.Map Int Frame }

derives [makeToDoc] [''Value, ''Frame, ''Store ]

blank :: Store
blank = Store { store = M.empty }

-- | allocate new empty frame, return its address
frame :: Monad m
      => Maybe Int -> Maybe Int
      -> S.StateT Store m Int
frame dyn stat = do
    let f = Frame { values = M.empty
                  , dynamic_link = dyn
                  , static_link = stat
                  }
    s <- S.get
    let n = M.size $ store s 
    S.put $ s { store = M.insert n f $ store s }
    return n

