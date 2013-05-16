{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module CSP.Fail where

import CSP.Syntax
import CSP.Step
import qualified CSP.Roll
import qualified CSP.Property.Guarded
import CSP.Property

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
    
interesting_terms p width cut = do    
    ( r, dist ) <- M.toList $ 
         M.fromListWith min $ do
            ( k , rs ) <- zip [ 0 .. cut ] 
                   $ takeWhile ( \ qs -> length qs < width )
                   $ levels  p
            r <- rs
            guard $ size r <= size p
            return ( r, k )
    return ( dist, (p, r ))
  
target_roller     :: ( ToDoc a, Ord a  )
       => Config a 
       -> IO ( Int, (Process a, Process a ))
target_roller conf = do
    outs <- forM [ 1 .. generator_repeats conf ] $ \ k -> do
        p <- CSP.Roll.roll_free
            ( how_to_iterate conf )
            ( process_alphabet conf )
            ( process_size conf )   
        let ok = CSP.Property.Guarded.ok    p
        let cut = max_derivation_length conf 
            wid = max_derivation_tree_width conf
        return $ if not ok then [] else 
                 interesting_terms p wid  cut
    return $ maximumBy ( comparing fst ) $ concat outs
    

rejects :: Ord a => Int -> Process a -> [[ S.Set a ]]
rejects width p = do
    let sigma = alphabet p
    qs <- takeWhile ( \ qs -> length qs < width ) $ levels p
    return $ do
        q <- qs
        guard $ null $ tau q
        let accepts = S.fromList $ map fst $ real q
        return $ S.difference sigma accepts
        
interesting_rejects p width cut = do
    ( r, dist ) <- M.toList $ 
         M.fromListWith min $ do
            ( k , rs ) <- zip [ 0 .. cut ] $ rejects width p
            r <- rs
            return ( r, k )
    return ( dist, (p, r ))
    
data Config a = Config
    { process_alphabet :: [ a ]  
    , process_size :: Int
    , how_to_iterate :: Iteration
    , max_derivation_length :: Int  
    , max_derivation_tree_width :: Int  
    , generator_repeats :: Int  
    }  deriving Typeable  
                
$(derives [makeReader, makeToDoc] [''Config])
                

config = Config    
    { process_alphabet = "abcd"
    , process_size = 5
    , how_to_iterate = Iteration_Star
    , max_derivation_length = 10                 
    , max_derivation_tree_width = 100
    , generator_repeats = 100
    }                          
    
    

reject_roller :: ( ToDoc a, Ord a  )
       => Config a 
       -> IO ( Int, (Process a, S.Set a ))
reject_roller conf = do
    outs <- forM [ 1 .. generator_repeats conf ] $ \ k -> do
        p <- CSP.Roll.roll_free
            ( how_to_iterate conf )
            ( process_alphabet conf )
            ( process_size conf )
        let ok = CSP.Property.Guarded.ok    p
        let cut = max_derivation_length conf 
            wid = max_derivation_tree_width conf
        return $ if not ok then [] else 
                 interesting_rejects p wid  cut
    return $ maximumBy ( comparing fst ) $ concat outs
    
t1 = Fix (Ext (Ext (Pre 'a' (Fix Point))
              (Par [ 'b' ] (Pre 'c' Point) (Pre 'c' Point)))
         (Pre 'b' (Pre 'b' (Seq (Pre 'b' Point) (Pre 'b' Point)))))
