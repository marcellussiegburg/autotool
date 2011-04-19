{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module CSP.STS.Bisi.Central where

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import CSP.STS.Type
import CSP.STS.Dot
import Autolib.Dot.Dotty ( peng )

import CSP.STS.Bisi.Report
import CSP.STS.Bisi.Refine
import CSP.STS.Roll

import Autolib.Set
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Autolib.Relation as R
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Data.Typeable
import Control.Monad ( void )
import Data.List ( maximumBy )
import Data.Ord ( comparing )

data STS_Bisi = STS_Bisi 
    deriving ( Read, Show, Typeable )

instance OrderScore STS_Bisi where
    scoringOrder _ = Increasing
    
instance Partial STS_Bisi     
           ( STS Int Char, STS Int Char )  
           [( Int, Int)] where
    report  p ( s, t ) = do           
        inform $ vcat
          [ text "Gesucht ist eine Bisimulation"
          , text "zwischen den Zustandsübergangssystemen"
          ]  
        inform $ text "S = " <+> toDoc s
        peng s
        inform $ text "T = " <+> toDoc t
        peng t
            
    initial p (s, t ) = S.toList 
        $ cross ( states s ) ( states t )
        
    total p (s, t) r = void $ check_bisi (s,t) $ R.make r    
    
make_fixed :: Make    
make_fixed = direct STS_Bisi 
    ( STS { start = 2 :: Int , alphabet = mkSet [ 'a' , 'b' ]
    , visible = [ ( 5 , 'a' , 3 ) , ( 1 , 'b' , 3 ) , ( 5 , 'b' , 3 )
                , ( 5 , 'b' , 2 ) , ( 5 , 'a' , 5 ) , ( 4 , 'b' , 2 )
                ]
    , hidden = [ ]
    }
    , STS { start = 1 :: Int , alphabet = mkSet [ 'a' , 'b' ]
    , visible = [ ( 3 , 'b' , 1 ) , ( 4 , 'b' , 2 ) , ( 4 , 'b' , 5 )
                , ( 3 , 'a' , 5 ) , ( 3 , 'a' , 3 )
                ]
    , hidden = [ ]
    } )
    
data Config = 
        Config { num_states :: Int
            , letters :: [Char]
            , num_visible :: Int
            , num_hidden :: Int  
            , generator_repeat :: Int  
            }  deriving ( Typeable )
     
$(derives [makeReader, makeToDoc] [''Config])

example_config :: Config     
example_config = Config
    { num_states = 5
    , num_visible = 7 
    , num_hidden = 0                
    , letters = "ab"               
    , generator_repeat = 1000            
    }                     

instance Generator STS_Bisi Config 
    ( STS Int Char, STS Int Char, [(Int,Int)] ) where
      generator _ conf key = multi conf

instance Project STS_Bisi  
    ( STS Int Char, STS Int Char, [(Int,Int)] ) 
        ( STS Int Char, STS Int Char ) where
      project _ (s,t,r) = (s,t)

multi conf = do        
    out <- sequence 
           $ replicate ( generator_repeat conf )
           $ single conf
    let (s,t,mr) = snd $ maximumBy (comparing fst) out 
    case mr of 
        Nothing -> multi conf 
        Just r -> return ( s, t, r )
    
single conf = do
    s <- roll [ 1 .. num_states conf ] (letters conf) 
         (num_visible conf) ( num_hidden conf )
    t <- roll [ 1 .. num_states conf ] (letters conf) 
         (num_visible conf) ( num_hidden conf )
    let r :: Maybe [(Int,Int)]
        r = fmap R.pairs $ bisi (s,t) 
    return ( fmap ( negate . length ) r, (s,t,r))
    
    
make_quiz :: Make    
make_quiz = quiz STS_Bisi example_config