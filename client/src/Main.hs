import Service.Interface

import Types.Config
import Types.Solution
import Types.TaskDescription
import Util.Xml.Output

import qualified Autolib.Output as O
import qualified Autolib.Output.Doc
import Autolib.Multilingual.Doc

import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    server <- case args of
        [server] -> return server
        _ -> do
            putStrLn "usage: autolat-client <URL>"
            exitWith (ExitFailure 1)

    putStrLn "\n=== Find server info ===\n"
    print =<< get_server_info server

    putStrLn "\n=== Get task types ===\n"
    print =<< get_task_types server

    putStrLn "\n=== Find task description ===\n"
    let task0 = "Convert_To_Exp-Quiz"
        task1 = "SuchbaumBinary-Quiz"
        task2 = "Convert_To_NFA-Direct"
    let task = task2
    print =<< get_task_description server task

    putStrLn "\n=== Configure task ===\n"
    let config0 = "-- com\nQuiz { generate = [ Alphabet (mkSet \"ab\"), Max_Size 5 ]\n\
                 \     , solve    = [ Alphabet (mkSet \"ab\"), Simple ] }"
        config1 = "Config\n    { start_size = 3 , min_key = 0 , max_key = 1000\n    , fixed_insert_ops = 1 , fixed_delete_ops = 0\n    , guess_insert_ops = 1 , guess_delete_ops = 0\n    }"
        config2 = "( Convert { name = Nothing , input = Exp a (a + b)^* b } , [ Sane , Min_Size 4 , Max_Size 10 , Deterministic, Alphabet (mkSet \"ab\") ] )"
    let config = config2
    res <- verify_task_config server task (CString config)
    print res

    case res of
        Right signedTaskConfig -> do
          putStrLn "\n=== Get a task instance ===\n"
          let seed = "test"
          -- mres2 <- get_task_instance_or_fail server signedTaskConfig seed
          mres2 <- fmap Right $ get_task_instance server signedTaskConfig seed
          case mres2 of
           Left err -> print (err :: Description)
           Right res2@(signedTaskInstance, DString desc, _) -> do
                 
            print res2

            print ( O.render ( xmlStringToOutput desc) :: Doc )

            do  putStrLn "\n=== Send an invalid solution ===\n"
                let solution = ""
                print =<< grade_task_solution server signedTaskInstance
                                              (SString solution)

            do  putStrLn "\n=== Send another solution - at time of testing, it was valid. ===\n"
                let solution0 = "((a+bb)(a+b))^*b(ab+b)^*"
                    solution2 = "NFA { alphabet = mkSet \"abcd\" , states = mkSet [ 1 , 2 , 3,4,5,6,7 ]    , starts = mkSet [ 1 ] , finals = mkSet [ 7 ]    , trans = collect [ ( 1 , 'b' , 1 ) , ( 1 , 'c' , 1 )                      , ( 1 , 'd' , 1 ) , ( 1 , 'a' , 2 )                      , ( 2 , 'b' , 3 ) , ( 3 , 'a' , 4 ) , ( 4 , 'b' , 5 )                      , ( 5 , 'c' , 6 ) , ( 6 , 'd' , 7 ) , ( 2 , 'a' , 1 )                      , ( 2 , 'c' , 1 ) , ( 2 , 'd' , 1 ) , ( 3 , 'b' , 1 )                      , ( 3 , 'c' , 1 ) , ( 3 , 'd' , 1 ) , ( 4 , 'a' , 1 )                      , ( 4 , 'c' , 1 ) , ( 4 , 'd' , 1 ) , ( 5 , 'a' , 1 )                      , ( 5 , 'b' , 1 ) , ( 5 , 'd' , 1 ) , ( 6 , 'a' , 1 )                      , ( 6 , 'b' , 1 ) , ( 6 , 'c' , 1 )                      ]    }"
                print =<< grade_task_solution server signedTaskInstance
                                              (SString solution2)

        _ -> return ()
