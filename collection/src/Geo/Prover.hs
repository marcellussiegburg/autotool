-- is a main module

{-# language TupleSections #-}
{-# language PatternSignatures #-}
{-# language OverloadedStrings #-}
{-# language RebindableSyntax #-}

import Geo.Program.Type
import Geo.Program.Value (Message (..),Trace)
import Geo.Program.Reader (program)

import qualified Geo.Program.Run

import qualified Prelude
import Prelude hiding (Num (..), null, fromInteger)
import Polynomial.Class
import Polynomial.Type 
import Polynomial.Grobner.Compute

import Autolib.Reader
import Autolib.ToDoc
import Autolib.TES.Identifier
import Autolib.Reporter
import System.Environment
import Control.Applicative
import System.Random
import System.IO
import Data.String ( fromString )

main = do
  args <- getArgs
  (f, s) <- case args of
    [] -> ("stdin",) <$> getContents
    [f] -> (f,) <$> readFile f
  let p = parse (program <* eof ) f s
  case p of
       Left e -> error $ show e
       Right x -> handle $ Block x

handle p = do
  putStrLn "input program"
  print $ toDoc p
  
  putStrLn "concrete evaluation (with random rational values)"
  g <- newStdGen
  let (out, msg :: Doc) =
        export $ Geo.Program.Run.concrete g p
  print msg ; hFlush stdout
  case out of
    Just (val, trace) -> do
      print $ vcat [ text "value" <+> toDoc val
               , text "trace" <+> toDoc trace
               ]
{-
      when (not $ null $ collect Assume trace) $ print $ vcat
         [ text "the trace contains assumptions"
         , text "so the above computation is meaningless"
         ]  
-}

  putStrLn "symbolic evaluation (polynomials)"
  let (out, msg :: Doc) =
        export $ Geo.Program.Run.symbolic p
  print msg ; hFlush stdout
  case out of
    Just (val, trace) -> do
      print $ vcat [ text "value" <+> toDoc val
               , text "trace" <+> toDoc trace
               ]  
      prove trace

prove trace = do
  putStrLn "analyze the claims in the trace."
  putStrLn "NOTE: we ignore nondeg. conditions"
  let prohibited = collect Prohibit trace
      assumed = collect Assume trace
      claimed = collect Claim trace
  forM_ (zip [1 :: Int ..] claimed) $ \ (k, claim) -> do    
    print $ toDoc (k, Claim, claim)
    let t = mk 0 "t" -- Rabinowitsch identifier
        r =  1 - variable t * claim
        fs = r : assumed
    print $ vcat [ text "ideal generated by", toDoc fs ]
    b <- buchbergerIO $ {- total_interreduce -} fs
    print $ vcat [ text "grobner basis", toDoc b ]
    -- let c' = reduce b claim
    -- print $ vcat [ text "reduced claim", toDoc c' ]

instance ToDoc StdGen 

collect :: Kind -> Trace (Ratio p) -> [p]
collect k trace = do
  Message {kind = k',contents= p :% q } <- trace
  guard $ k == k'
  return p
  
      