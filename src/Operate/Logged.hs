module Operate.Logged where


import qualified Control.SQL 
import System.IO

logged :: String -> IO a -> IO a
logged msg act = do
    Control.SQL.logged ( msg ++ " ..." )
    x <- act
    Control.SQL.logged ( "... " ++ msg )
    return x

