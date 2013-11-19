module Service.GetTaskInstance (
    get_task_instance
) where

import Util.Sign
import Util.Task
import Util.Description
import Util.Timeout
import Util.Cache

import Types.Basic
import Types.Signed as S
import Types.Documented as D
import Types.Config
import Types.Instance as I
import Types.Solution
import Types.TT

import Inter.Types as IT
-- import Control.Types (VNr (..))

-- import Autolib.Reporter
import Autolib.Reporter.IO.Type
import Autolib.Reader
import qualified Autolib.Reader as AR
import qualified Autolib.ToDoc as AT
import Challenger.Partial as CP

import Text.ParserCombinators.Parsec
import Control.Monad.Error

import Data.String
import Data.ByteString ( ByteString )
import Data.Digest.CRC32

import qualified Control.Exception as CE

nocache :: CacheFun
nocache _ = id

get_task_instance
    :: TT (Signed (Task, Config)) -> TT Seed
    -> IO (TT (Signed (Task, Instance), Description, Documented Solution))
get_task_instance  (TT sconf) (TT seed) = withTimeout $ fmap TT $ do

    (task, CString config) <- verifyM sconf
    Make _ _ maker0 _ _ <- lookupTaskM task
    Right config' <- return $ parse (parse_complete reader) "<config>" config
    let maker = maker0 config'

    -- ri <- gen maker (VNr 0) Nothing seed nocache
    let s = crc32 ( fromString seed :: ByteString )

    ri <- generate maker ( fromIntegral s ) 
          $ Util.Cache.cache

    res <- result $ Autolib.Reporter.IO.Type.lift ri

    i <- maybe (fail "internal error generating instance") return res

    let b = CP.initial (problem maker) i
    
    doc <- help b

    -- FIXME: this seems critical if it involves drawing (peng/graphviz):
    descr <- fromReport $ Autolib.Reporter.IO.Type.lift 
                        $ CP.report (problem maker) i

    let st = sign (task,
                   Instance { I.tag = IT.tag maker,
                              I.contents = -- AT.showDoc . AT.toDoc $ i
                                AT.render $ AT.toDoc i
                            })
        docked = Documented { D.contents = SString . AT.render . AT.toDoc $ b,
                          D.documentation = doc }
        result = ( st
           , descr
           , docked
           )

    i `seq` b `seq` st `seq` return result
