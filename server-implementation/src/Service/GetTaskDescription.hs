module Service.GetTaskDescription (
    get_task_description,     get_task_description_localized
) where

import Util.Task
import Util.Description

import Types.Basic
import Types.TaskDescription
import Types.TT

import Inter.Types
import Autolib.Multilingual hiding (Make)

import qualified Autolib.ToDoc as AT

get_task_description :: TT Task -> IO (TT TaskDescription)
get_task_description (TT name) = 
    get_task_description_localized (TT name) (TT DE)

get_task_description_localized :: TT Task -> TT Language -> IO (TT TaskDescription)
get_task_description_localized (TT name) (TT lang) = TT `fmap` do
    m@(Make _ _ _ _ conf) <- lookupTaskM name
    doc <- help lang conf
    let sample = Documented
            (CString (AT.render_for lang . AT.toDoc $ conf))
            doc
    return $ TaskDescription { task_sample_config = sample,
                               task_scoring_order = taskScoringOrder m }
