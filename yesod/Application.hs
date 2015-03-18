{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Control.Monad.Logger (runLoggingT)
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Database.Persist.MySQL (createMySQLPool, myConnInfo, myPoolSize)
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
import Yesod.Core.Types (loggerSet, Logger (Logger))

import Yesod.Auth (getAuth)

import Handler.Home
import Handler.Aufgabe
import Handler.AufgabeAnlegen
import Handler.AufgabeBenutzerId
import Handler.AufgabeBenutzerIdZufall
import Handler.AufgabeKonfiguration
import Handler.Aufgaben
import Handler.AufgabenAktuell
import Handler.AufgabeTesten
import Handler.AufgabeVorlage
import Handler.AufgabeVorlagen
import Handler.Direktoren
import Handler.DirektorErnennen
import Handler.Einsendung
import Handler.EinsendungAnlegen
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
import Handler.Server
import Handler.Servers
import Handler.Statistik
import Handler.Studenten
import Handler.Tutoren
import Handler.TutorErnennen
import Handler.Vorlesung
import Handler.VorlesungAnlegen
import Handler.Vorlesungen
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
    dbconf <- withYamlEnvironment "config/mysql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        mkFoundation p = Autotool
            { settings = conf
            , getStatic = s
            , connPool = p
            , httpManager = manager
            , persistConfig = dbconf
            , appLogger = logger
            }
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation logger

    p <- flip runLoggingT logFunc
       $ createMySQLPool (myConnInfo dbconf) (myPoolSize dbconf)
    let foundation = mkFoundation p

    flip runLoggingT logFunc
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
