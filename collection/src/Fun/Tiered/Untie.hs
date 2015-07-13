module Fun.Tiered.Untie where

import Fun.Tiered.Type
import qualified Fun.Type as U

import Data.List ( nub, transpose )

import Autolib.Reporter
import Autolib.ToDoc

-- | compute tiered types of arguments

tiered_type :: Fun -> Reporter [Tier]
tiered_type f = tracked f $ case f of
  Zero t a -> base t a
  Succ0 t a -> base t a
  Succ1 t a -> base t a
  Proj t a to -> base t a
  Builtin t a b -> base t a

  Sub t (A a) fs -> do
    tts <- forM fs tiered_type
    return $ map maximum $ transpose tts

  PR t (A a) [g,h0,h1] -> do
    tg <- tiered_type g  
    U.Binary_PR a <$> forM fs untie

base t (A a) = return $ replicate a [T 0] 

tracked f action = do
    inform $ text "computer tieder type for" </> toDoc f
    t <- nested 2 action
    inform $ text "tiered type is" </> toDoc t
    return t

untie :: Fun -> U.Fun
untie = case f of
  Zero t (A a) -> U.Zero a
  Succ0 t (A a) -> U.Binary_Succ0 a
  Succ1 t (A a) -> U.Binary_Succ1 a 
  Proj t (A a) to -> U.Proj a to
  Builtin t (A a) b -> U.Builtin a b
  Sub t (A a) fs -> U.Sub a $ map untie fs
  PR t (A a) fs -> U.Binary_PR a $ map untie fs
