{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}

module Lattice.LLL.Inverse where

import Lattice.LLL.Compute
import qualified Lattice.LLL.Task as LT
import qualified Lattice.Reduce as LR

import Autolib.Reporter hiding ( run, execute )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Data.Typeable
import Data.Ratio
import Data.Function (on)
import Data.List (minimumBy)
import Control.Applicative

data Lattice_LLL_Inverse = Lattice_LLL_Inverse
    deriving (Typeable, Read, Show)

instance OrderScore Lattice_LLL_Inverse where
    scoringOrder _ = Increasing

instance Measure Lattice_LLL_Inverse [Step] [[Integer]] where
    measure _ _ base = sum $ map abs $ concat base

instance Partial Lattice_LLL_Inverse [Step] [[Integer]] where

    describe _ steps = vcat 
       [ text "Give a lattice base such that the sequence of steps"
       , toDoc $ map Line steps
       , text "is a valid execution of the LLL algorithm,"
       , text "using Shoup's condition for swap steps."
       , text "Note: we use exact rational arithmetic in all computations,"
       , text "and fixed point format only for display."
       ]

    initial _ steps = 
       let dim = maximum $ for steps $ \ s -> case s of
               Reduce {} -> target s
               Swap {} -> that s
       in  for [0 ..dim] $ \ i -> 
           for [0 .. dim] $ \ j -> if i == j then 1 else 0


    partial _ steps base = do
        void $ foldM LT.execute (start base) steps

    total _ steps base = do
        result <- silent $ foldM LT.execute (start base) steps
        inform $ nice result
        case sizereductions result of
             [] -> inform $ text "base is size-reduced"
             r:_ -> reject $ sizereductions_message result 
                       (target r) (using r)
        case swaps result of
             [] -> inform $ text "base is Shoup-reduced"
             s:_ -> reject $ shoup_message result s

make_fixed :: Make
make_fixed = direct Lattice_LLL_Inverse [ Swap 0 1 ]

roll :: LR.Config -> IO [[Integer]]
roll c = LR.base <$> LR.roll c

instance Generator Lattice_LLL_Inverse LR.Config ([Step],[[Integer]]) where
    generator p conf key = do
        candidates <- replicateM 100 $ do
            base <- roll conf
            let steps = lll base
            return ( steps, base )
        return $ minimumBy (compare `on` \ (s,b) ->
                 abs (length s - 10)) candidates

instance Project Lattice_LLL_Inverse ([Step],[[Integer]]) [Step] where
    project _ (steps,base) = steps

make_quiz :: Make
make_quiz = quiz Lattice_LLL_Inverse LR.config0

