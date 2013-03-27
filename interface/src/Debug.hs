module Debug where

import System.IO.UTF8
import System.Time
import qualified Local
import Control.Monad ( when )
import qualified System.Cmd

jetzt :: IO String
jetzt = do
    clock <- getClockTime
    cal <- toCalendarTime clock
    return $ calendarTimeToString cal

debug msg = do
    s <- System.IO.UTF8.readFile "/tmp/tool.debug"
    case words s of
      [ "YES" ] -> do
        now <- jetzt
        System.IO.UTF8.appendFile "/tmp/tool.log" $ unlines [ now, msg ]
        -- hPutStrLn stderr cs
        -- hFlush stderr
      _ -> return ()  

system argv = do
    debug $ "start system: " ++ show argv
    res <- System.Cmd.system argv
    debug $ "  end system: " ++ show argv 
    debug $ "  with code: " ++ show res
    return res
