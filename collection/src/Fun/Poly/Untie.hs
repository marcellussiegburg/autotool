module Fun.Poly.Untie where

import Fun.Poly.Type
import qualified Fun.Type as U

import Autolib.Reporter
import Autolib.ToDoc

-- | check that tier annotations are correct,
-- and remove them (so computation can run as before)

-- FIXME:
untie :: Fun -> Reporter U.Fun
untie f = case f of
  Zero t a -> return $ U.Zero a
  Succ0 t a -> return $ U.Succ0 a
  Succ1 t a -> return $ U.Succ1 a 
  Proj t a from to -> return $ U.Proj a from to
  Builtin t a b -> return $ U.Builtin a b
  Sub t a fs -> return $ U.Sub a fs
  PR t a fs -> return $ U.OR a fs
