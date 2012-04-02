module Paths_autotool_db (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/autotool-db-1.0/ghc-7.4.1"
datadir    = "/usr/local/share/autotool-db-1.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "autotool_db_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "autotool_db_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "autotool_db_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "autotool_db_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
