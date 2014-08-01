module Service.GradeTaskSolution (
    grade_task_solution,     grade_task_solution_localized
) where

import Util.Sign
import Util.Task
import Util.Parse
import Util.Description
import Util.Timeout

import Types.Basic
import Types.Signed
import Types.Instance as I
import Types.Solution
import Types.Documented as D
import Types.TT

import Inter.Types
import Inter.Evaluate
import Autolib.Reporter.IO.Type
import Autolib.ToDoc ( hsep, text )
import Autolib.Multilingual hiding ( Make )
import qualified Autolib.Reporter.Classic.Type
import Inter.Wert (is_okay, size)

import Control.Monad.Error
import qualified Control.Exception as CE

grade_task_solution
    :: TT (Signed (Task, Instance)) -> TT Solution
    -> IO (TT (Either Description (Documented Double)))
grade_task_solution i s = 
    grade_task_solution_localized i s (TT DE)

grade_task_solution_localized
    :: TT (Signed (Task, Instance)) -> TT Solution -> TT Language
    -> IO (TT (Either Description (Documented Double)))
grade_task_solution_localized (TT sTaskInst) (TT (SString solution)) (TT lang)
    = withTimeout . fmap TT . runErrorT $ do
        (task, inst) <- verifyM sTaskInst
        Make p _ maker0 _ _ <- lookupTaskM task
        inst' <- parseHelper "<instance>" (I.contents inst)
        let assertTypes :: (conf -> Var p i b) -> (p, i) -> ()
            assertTypes _ _ = ()
            () = assertTypes maker0 (p, inst')
        eres <- liftIO $ CE.try 
           $ CE.evaluate $ evaluate p inst' solution 
        case eres of
          Left ex -> throwReport $ reject $ hsep
             [ text "unhandle exception"
             , text $ show ( ex :: CE.SomeException) 
             ]
          Right res -> do
            ( mres, out ) <- liftIO $ run res     
            score <- case mres of
                Nothing -> throwReport res
                Just score -> return score
            when (not (is_okay score)) $ throwReport res
            doc <- liftIO $ fromReport res
            let sz = size score
                sz' = if sz == 0 then 1 else sz
            return $ Documented { D.contents = fromIntegral sz',
                              D.documentation = doc }

throwReport :: Reporter b -> ErrorT Description IO a
throwReport rep = liftIO (fromReport rep) >>= throwError
