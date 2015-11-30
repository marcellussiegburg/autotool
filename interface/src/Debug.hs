{-# LANGUAGE CPP #-}

module Debug where

#if (__GLASGOW_HASKELL__<710)
import Prelude hiding (appendFile)
import System.IO.UTF8
#endif
import System.Directory
import System.Time

import Control.Monad ( when )
import qualified System.Cmd

jetzt :: IO String
jetzt = do
    clock <- getClockTime
    cal <- toCalendarTime clock
    return $ calendarTimeToString cal

debug msg = do
    d <- doesFileExist "/tmp/tool.debug"
    when d $ do
        now <- jetzt
        appendFile "/tmp/tool.log" $ unlines [ now, msg ]
        -- hPutStrLn stderr cs
        -- hFlush stderr

system argv = do
    debug $ "start system: " ++ show argv
    res <- System.Cmd.system argv
    debug $ "  end system: " ++ show argv 
    debug $ "  with code: " ++ show res
    return res
