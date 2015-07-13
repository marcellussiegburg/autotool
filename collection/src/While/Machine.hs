{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module While.Machine where

import Machine.Class

import While.Type
import While.Memory
import While.State
import While.Step

import Autolib.Set
import Autolib.Size

instance Compute Program State where
    depth _ = schritt
    next p s = mkSet $ step s
    accepting p s = null $ todo s

instance In Program Memory State where
    input_reporter p m = do
        return $ State { memory = m, todo = [p], schritt = 0, past = [] }
instance Out Program Memory State where
    output_reporter p s = do
        return $ memory s

instance Encode Memory where
    encode xs = make $ zip [ 1 .. ] xs
instance Decode Memory where
    decode m = get m 0
    


