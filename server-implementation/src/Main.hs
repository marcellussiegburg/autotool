{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances #-}

-- The main RPC service.

module Main where

import Network.XmlRpc.Server

import Service.GetServerInfo
import Service.GetTaskTypes
import Service.GetTaskDescription
import Service.VerifyTaskConfig
import Service.GetTaskInstance
import Service.GetTaskInstanceOrFail
import Service.GradeTaskSolution
import Config (timeLimit, secret)

import System.IO
import System.Timeout

main :: IO ()
main = do
    hSetBinaryMode stdout True
    hSetBinaryMode stdin True
    -- note: timeouts are supposed to be handled by the individual services.
    -- this limit is a fallback.
    timeout (timeLimit * 3 `div` 2) $
        cgiXmlRpcServer proto
    return ()

-- supported RPC calls
proto :: [(String, XmlRpcMethod)]
proto = [
    ("get_server_info", fun get_server_info),
    ("get_task_types", fun get_task_types),
    ("get_task_description", fun get_task_description),
    ("verify_task_config", fun verify_task_config),
    ("get_task_instance", fun get_task_instance),
    ("get_task_instance_or_fail", fun get_task_instance_or_fail),
    ("grade_task_solution", fun grade_task_solution),
    ("get_task_description_localized", fun get_task_description_localized),
    ("verify_task_config_localized", fun verify_task_config_localized),
    ("get_task_instance_localized", fun get_task_instance_localized),
    ("get_task_instance_or_fail_localized", fun get_task_instance_or_fail_localized),
    ("grade_task_solution_localized", fun grade_task_solution_localized),
    ("ping", fun ping)
    ]

-- ping is not part of the official protocol, but does no harm
ping :: IO ()
ping = return ()
