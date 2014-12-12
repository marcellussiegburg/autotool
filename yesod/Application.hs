{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import Network.HTTP.Client.Conduit (newManager)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))

import Handler.Home
import Handler.Aufgabe
import Handler.Direktoren
import Handler.DirektorErnennen
import Handler.Gruppe
import Handler.GruppeAnlegen
import Handler.Gruppen
import Handler.Resultate
import Handler.ResultatePflicht
import Handler.Schule
import Handler.SchuleAnlegen
import Handler.Schulen
import Handler.Semester
import Handler.SemesterAnlegen
import Handler.Semesters
import Handler.Statistik
import Handler.Studenten
import Handler.Tutoren
import Handler.TutorErnennen
import Handler.Vorlesung
import Handler.VorlesungAnlegen
import Handler.Waisenkinder

mkYesodDispatch "Autotool" resourcesAutotool

makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

makeFoundation :: AppConfig DefaultEnv Extra -> IO Autotool
makeFoundation conf = do
    manager <- newManager
    s <- staticSite

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = Autotool
            { settings = conf
            , getStatic = s
            , httpManager = manager
            , appLogger = logger
            }

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
