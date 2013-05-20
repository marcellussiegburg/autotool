module Resolution.Roll where

import Resolution.Data
import Resolution.Config
import Resolution.Enumerate (enumerate)

import Autolib.Util.Zufall
import Autolib.Util.Sort
import Autolib.TES.Identifier
import Autolib.Set
import Autolib.ToDoc

import qualified Data.Set as S

import OBDD ( OBDD )
import qualified OBDD as O

variables :: Int -> [ Identifier ]
variables n = take n $ map ( mkunary . return ) [ 'a' .. ]

rclause :: Config -> IO Clause
rclause conf = do 
    s <- randomRIO $ literals_per_clause_bounds conf
    vs <- selektion s $ variables $ num_variables conf
    ls <- sequence $ do 
        v <- vs 
        return $ do 
            p <- randomRIO ( False, True )
            return $ Literal { name = v, parity = p }
    return $ Clause $ mkSet ls

semantics :: Clause -> OBDD Identifier
semantics ( Clause xs ) = 
    foldr ( O.|| ) ( O.constant False ) $ do
        x <- setToList xs
        return $ O.unit ( name x ) ( parity x )

roll_unsat :: Config -> IO [Clause]
roll_unsat conf = 
    let extend current cs = do
            c <- rclause conf
            if or $ do c' <- cs 
                       return $ issubset c c'
                then extend current cs
                else do
                    let next = current O.&& semantics c
                        cs' = c : cs
                    if O.null next 
                       then return cs' 
                       else extend next cs'
    in  extend ( O.constant True ) []

roll_with_target :: Config -> IO ( [Clause], Clause)
roll_with_target conf = do
    cls0 <- roll_unsat conf
    cls <- selektion (div (length cls0) 2) cls0
    let levels = enumerate cls
    target <- eins $ S.toList $ last levels
    return ( cls, target )

rset :: Config -> IO ( [ Clause ], Clause )
rset conf = case target conf of
    Empty -> do
        cs <- roll_unsat conf
        return ( cs, Clause emptySet )
    Random -> do
        roll_with_target conf

issubset ( Clause xs ) ( Clause ys ) = 
    subseteq xs ys

medium :: Config -> IO ( [ Clause ], Clause )
medium conf = do
    let n = 5
    candidates  <- sequence $ replicate n $ rset conf
    return $ sortBy ( \ (cs, t) -> length cs ) candidates !! (n `div` 2)

