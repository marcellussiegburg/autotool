-- | state transitions systems

{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}

module CSP.STS where

import Autolib.Reader
import Autolib.ToDoc

data STS s t = STS { start :: s
                   , visible :: [ (s, t, s) ]  
                   , hidden :: [ (s, s) ]
                   }  
               
$(derives [ makeReader, makeToDoc ] [ ''STS ] )