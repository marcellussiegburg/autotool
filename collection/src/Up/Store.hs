{-# language TemplateHaskell #-}
{-# language DisambiguateRecordFields #-}

module Up.Store where

import Up.Type
import Autolib.ToDoc

import Autolib.FiniteMap () -- for instances
import qualified Data.Map as M

import qualified Control.Monad.State.Strict as S

data Value = ValUnit
           | ValInt Integer
           | ValClosure { link :: Int
                        , body :: Exp
                        }

data Frame = Frame { number :: Int
                   , dynamic_link :: Int
                   , static_link :: Int
                   , values :: M.Map Name Value
                   }

data Store = Store { step :: Int
                   , max_steps :: Int
                   , store :: M.Map Int Frame 
                   }

derives [makeToDoc] [''Value, ''Frame, ''Store ]


blank :: Int -> Store
blank r = Store 
        { step = 0
        , max_steps = r
        , store = M.empty 
        }

tick :: Monad m => S.StateT Store m ()
tick = S.modify $ \  s -> s { step = succ $ step s }

-- | allocate new empty frame, return its address
frame :: Monad m
      => Int -> Int
      -> S.StateT Store m Int
frame dyn stat = do
    s <- S.get
    let n = succ $ M.size $ store s 
    let f = Frame { number = n
                  , values = M.empty
                  , dynamic_link = dyn
                  , static_link = stat
                  }
    S.put $ s { store = M.insert n f $ store s }
    return n

