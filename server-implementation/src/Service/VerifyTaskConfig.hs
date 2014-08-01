module Service.VerifyTaskConfig (
    verify_task_config,     verify_task_config_localized
) where

import Util.Task
import Util.Sign
import Util.Parse
import Util.Description
import Util.Timeout

import Types.Basic
import Types.Signed
import Types.Config
import Types.Description
import Types.TT

import Inter.Types
import Autolib.Reporter.IO.Type
import Autolib.Multilingual hiding (Make)

import Control.Monad.Error

verify_task_config
    :: TT Task -> TT Config
    -> IO (TT (Either Description (Signed (Task, Config))))
verify_task_config t c = verify_task_config_localized t c (TT DE)

verify_task_config_localized
    :: TT Task -> TT Config -> TT Language
    -> IO (TT (Either Description (Signed (Task, Config))))
verify_task_config_localized (TT task) (TT (CString config)) (TT lang)
    = withTimeout . fmap TT . runErrorT $ do
        Make _ _ _ verifyConf _ <- lookupTaskM task
        config' <- parseHelper lang "<config>" config
        let report = Autolib.Reporter.IO.Type.lift 
                   $ verifyConf config'
        rr <- liftIO $ result report
        case rr of
            Nothing -> liftIO (fromReport lang report) >>= throwError
            _       -> return ()
        return $ sign (task, CString config)
