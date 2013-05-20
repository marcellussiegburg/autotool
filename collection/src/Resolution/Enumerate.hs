module Resolution.Enumerate where

import Resolution.Data
import Resolution.Execute 

import Autolib.ToDoc

import qualified Data.Set as S
import Data.List (inits, tails)

import Debug.Trace

enumerate :: [Clause] -> [ S.Set Clause ]
enumerate cls = 
    levels ( S.fromList cls ) 
        $ \ s -> S.union s $ S.fromList $ do
             x : ys <- tails $ S.toList s
             y <- ys
             filter non_true $ resolves x y

non_true :: Clause -> Bool
non_true cl = not $ or $ do
    l : ls <- tails $ literals cl
    return $ turn l `elem` ls

splits xs = zip (inits xs) (tails xs)

levels s f = s : 
    let s' = f s
    in  if s' == s then [] else levels s' f

