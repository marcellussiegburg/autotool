module CSP.STS.Bisi where

import CSP.STS.Type
-- import CSP.STS.Tau
import CSP.STS.Roll

import CSP.STS.Bisi.Refine

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import qualified Autolib.Relation as R
import qualified Data.Set as S
import Autolib.Set ( cross )
import Control.Monad ( guard, void, forM )

checks = do
    let n = 5 :: Int
    s <- roll [1..n ] "ab" 7 0
    t <- roll [1..n ] "ab" 7 0
    -- t <- mutate 3 s
    case bisi (s,t) of
        Nothing -> checks
        Just r -> do
            print s
            print t
            print r
            return r

s = STS { start = 3 , alphabet = S.fromList [ 'a' , 'b' ]
    , visible = [ ( 2 , 'b' , 4 ) , ( 1 , 'b' , 4 ) , ( 4 , 'a' , 2 )
                , ( 4 , 'a' , 1 )
                ]
    , hidden = [ ( 3 , 4 ) , ( 2 , 1 ) ]
    }

