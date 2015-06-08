{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Fun.Poly.Machine where

import Machine.Class

import Fun.Poly.Type
import Fun.Poly.State
import Fun.Poly.Step


import qualified Fun.Poly.Cache

import Autolib.Set
import Autolib.Size

instance Compute Fun State where
    next p s = mkSet $ step s
    accepting p s = null $ step s -- ??
    depth p s = schritt s

instance In Fun [ Integer ] State where
    input_reporter  p m = return $ input p m

input p m = State 
         { todo = [App p $ map Zahl m]
         , stack = []
         , schritt = 0
         , past = [] 
         , cache = Fun.Poly.Cache.empty
         }

instance Out Fun [ Integer ] State where
    output_reporter p s = return $ stack s

instance Encode [ Integer ] where
    encode = id

instance Decode [ Integer ] where
    decode [x] = x



