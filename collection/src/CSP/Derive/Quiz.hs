{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module CSP.Derive.Quiz where

import CSP.Syntax
import CSP.Step
import qualified CSP.Roll
import qualified CSP.Property.Guarded

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad ( guard )

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Data.Typeable
import Data.List ( maximumBy )
import Data.Ord ( comparing )
import Control.Monad ( forM, when )
import System.IO ( hFlush, stdout )

levels :: Ord a => Process a -> [[ Process a ]]
levels p = do
    let helper [] = []
        helper xs = 
          let ys = S.toList $ S.fromList 
                 $ map snd $ xs >>= successors
          in  xs : helper ys    
    helper [p]
    
interesting_terms p (minw,maxw) cut = do    
    ( r, dist ) <- M.toList $ 
         M.fromListWith min $ do
            ( k , rs ) <- zip [ 0 .. cut ] 
                   $ takeWhile ( \ qs -> length qs < maxw )
                   $ levels  p
            r <- rs
            guard $ size r <= size p
            return ( r, ( length rs > minw, k ) )
    return ( dist, (p, r ))
  
target_roller     :: ( ToDoc a, Ord a  )
       => Config a 
       -> IO ( (Bool,Int), (Process a, Process a ))
target_roller conf = do
    outs <- forM [ 1 .. generator_repeats conf ] $ \ k -> do
        p <- CSP.Roll.roll_free
            ( process_alphabet conf )
            ( process_size conf )
        let ok = CSP.Property.Guarded.ok    p
        let cut = max_derivation_length conf 
            minw = min_derivation_tree_width conf
            maxw = max_derivation_tree_width conf
        return $ if not ok then [] else 
                 interesting_terms p (minw, maxw) cut
    case concat outs of    
        [] -> target_roller conf
        co -> return $ maximumBy ( comparing fst ) co
    

rejects :: Ord a => Int 
                 -> S.Set a
                 -> Process a -> [[ S.Set a ]]
rejects width sigma p = do
    qs <- takeWhile ( \ qs -> length qs < width ) $ levels p
    return $ do
        q <- qs
        guard $ null $ tau q
        let accepts = S.fromList $ map fst $ real q
        return $ S.difference sigma accepts
        
interesting_rejects sigma p (width, wadth) cut = do
    ( r, dist ) <- M.toList $ 
         M.fromListWith min $ do
            ( k , rs ) <- zip [ 0 .. cut ] 
                          $ rejects wadth sigma p
            r <- rs
            return ( r, (length rs > width, k ) )
    return ( dist, (p, r ))
    
data Goal_Type = Target | Rejects deriving Typeable

data Config a = Config
    { goal_type :: Goal_Type
    ,  process_alphabet :: [ a ]  
    , process_size :: Int
    , max_derivation_length :: Int  
    , max_derivation_tree_width :: Int  
    , min_derivation_tree_width :: Int  
    , generator_repeats :: Int  
    }  deriving Typeable  
                
$(derives [makeReader, makeToDoc] [''Config, ''Goal_Type])
                

config = Config    
    { goal_type = Rejects
    , process_alphabet = "abc"
    , process_size = 6
    , max_derivation_length = 10                 
    , min_derivation_tree_width = 2
    , max_derivation_tree_width = 100
    , generator_repeats = 100
    }                          
    
    

reject_roller :: ( ToDoc a, Ord a  )
       => Config a 
       -> IO ( (Bool, Int), (Process a, S.Set a ))
reject_roller conf = do
    outs <- forM [ 1 .. generator_repeats conf ] $ \ k -> do
        p <- CSP.Roll.roll_free
            ( process_alphabet conf )
            ( process_size conf )
        let sigma = CSP.Syntax.alphabet p    
        let ok = CSP.Property.Guarded.ok    p
        let cut = max_derivation_length conf 
            wid = min_derivation_tree_width conf
            wad = max_derivation_tree_width conf
        return $ if not ok then [] else 
                 interesting_rejects sigma p (wid, wad)  cut
    return $ maximumBy ( comparing fst ) $ concat outs
    
t1 = Fix (Ext (Ext (Pre 'a' (Fix Point))
              (Par [ 'b' ] (Pre 'c' Point) (Pre 'c' Point)))
         (Pre 'b' (Pre 'b' (Seq (Pre 'b' Point) (Pre 'b' Point)))))
