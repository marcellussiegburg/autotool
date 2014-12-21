module Geo.Program.Concrete where

import Geo.Program.Eval 

import Autolib.TES.Identifier
import Data.Ratio

concrete = mkEnv
  [ ( mknullary "pp_line"
    , Function LineT [PointT,PointT] $ \ [ Point (ax,ay), Point (bx,by) ] -> do
        return $ Line ( 1 , 1 , 1 )
    )
  ]  
