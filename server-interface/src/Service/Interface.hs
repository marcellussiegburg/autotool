module Service.Interface (
    get_server_info,
    get_task_types,

    get_task_description,
    verify_task_config,
    get_task_instance,
    get_task_instance_or_fail,
    grade_task_solution,

    get_task_description_localized,
    verify_task_config_localized,
    get_task_instance_localized,
    grade_task_solution_localized,
    get_task_instance_or_fail_localized,

    Server
) where

import Types.TT
import Types.Basic
import Types.Signed as S
import Types.Documented as D
import Types.ServerInfo
import Types.TaskTree
import Types.TaskDescription
import Types.Config
import Types.Instance as I
import Types.Solution

import Autolib.Multilingual (Language (..))

import Network.XmlRpc.Client

import Control.Applicative

type Server = String

get_server_info :: Server -> IO ServerInfo
get_server_info srv =
    unTT <$> remote srv "get_server_info"

get_task_types :: Server -> IO [TaskTree]
get_task_types srv =
    unTT <$> remote srv "get_task_types"

get_task_description :: Server -> Task -> IO TaskDescription
get_task_description srv a =
    unTT <$> remote srv "get_task_description" (TT a)

verify_task_config :: Server
    -> Task -> Config
    -> IO (Either Description (Signed (Task, Config)))
verify_task_config srv a b =
    unTT <$> remote srv "verify_task_config" (TT a) (TT b)

get_task_instance :: Server
    -> Signed (Task, Config) -> Seed
    -> IO (Signed (Task, Instance), Description, Documented Solution)
get_task_instance srv a b =
    unTT <$> remote srv "get_task_instance" (TT a) (TT b)

get_task_instance_or_fail :: Server
    -> Signed (Task, Config) -> Seed
    -> IO (Either Description (Signed (Task, Instance), Description, Documented Solution))
get_task_instance_or_fail srv a b =
    unTT <$> remote srv "get_task_instance_or_fail" (TT a) (TT b)

grade_task_solution :: Server
    -> Signed (Task, Instance) -> Solution
    -> IO (Either Description (Documented Double))
grade_task_solution srv a b =
    unTT <$> remote srv "grade_task_solution" (TT a) (TT b)


get_task_description_localized :: Server -> Task -> Language -> IO TaskDescription
get_task_description_localized srv a lang =
    unTT <$> remote srv "get_task_description_localized" (TT a) (TT lang)

verify_task_config_localized :: Server
    -> Task -> Config
    -> Language -> IO (Either Description (Signed (Task, Config)))
verify_task_config_localized srv a b lang =
    unTT <$> remote srv "verify_task_config_localized" (TT a) (TT b) (TT lang)

get_task_instance_localized :: Server
    -> Signed (Task, Config) -> Seed
    -> Language -> IO (Signed (Task, Instance), Description, Documented Solution)
get_task_instance_localized srv a b lang =
    unTT <$> remote srv "get_task_instance_localized" (TT a) (TT b) (TT lang)

get_task_instance_or_fail_localized :: Server
    -> Signed (Task, Config) -> Seed
    -> Language -> IO (Either Description (Signed (Task, Instance), Description, Documented Solution))
get_task_instance_or_fail_localized srv a b lang =
    unTT <$> remote srv "get_task_instance_or_fail_localized" (TT a) (TT b) (TT lang)

grade_task_solution_localized :: Server
    -> Signed (Task, Instance) -> Solution
    -> Language -> IO (Either Description (Documented Double))
grade_task_solution_localized srv a b lang =
    unTT <$> remote srv "grade_task_solution_localized" (TT a) (TT b) (TT lang)

