{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Goto.Machine where

import Machine.Class

import Goto.Type
import Goto.Memory
import Goto.State
import Goto.Step

import Autolib.Set
import Autolib.Size
import Autolib.TES.Identifier

instance Compute Program State where
    depth _ = schritt
    next p s = mkSet $ step s
    accepting p s = get_command s == Just Stop

instance In Program Memory State where
    input_reporter p m = do
        return $ State { memory = m, pc = 0, program = p, schritt = 0, past = [] }
instance Out Program Memory State where
    output_reporter p s = do
        return $ memory s

instance Encode Memory where
    encode xs = make $ zip [ 1 .. ] xs
    
instance Decode Memory where
    decode m = get m 0


