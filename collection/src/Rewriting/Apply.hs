{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

module Rewriting.Apply where

import Rewriting.Derive.Instance

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Size

class Size object => Apply tag system object action | tag -> system object action where
    example_object_of_size :: tag -> Int -> object
    -- | default instance
    example :: tag -> Instance system object
    -- | apply one action to object
    apply :: tag -> system -> object -> action 
          -> Reporter object
    -- | list of all possible actions
    actions :: tag -> system -> object 
            -> [ action ]

 
    

