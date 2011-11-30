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
import Autolib.Size
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
        inform $ text "S1 = " <+> toDoc s
        peng s
        inform $ text "S2 = " <+> toDoc t
        peng t
            
    initial p (s, t ) = pick $ S.toList 
        $ cross ( states s ) ( states t )
        
    total p (s, t) r = 
        void $ check_bisi (s, text "S1")
                          (t, text "S2")  
             $ R.make r 
    
pick (x:xs) = x : pick ( odds xs ) ; pick _ = []
odds (x:y:zs) = y : odds zs ; odds _  = []

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
    { num_states = 4
    , num_visible = 6 
    , num_hidden = 0                
    , letters = "ab"               
    , generator_repeat = 1000            
    }                     

instance Generator STS_Bisi Config 
    ( STS Int Char, STS Int Char, ( R.Type Int Int ) ) where
      generator _ conf key = multi conf

instance Project STS_Bisi  
    ( STS Int Char, STS Int Char, ( R.Type Int Int ) ) 
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
    let full = num_states conf * num_states conf
    s <- roll_reachable 
         [ 1 .. num_states conf ] (letters conf) 
         (num_visible conf) ( num_hidden conf )
    t <- roll_reachable 
         [ 1 .. num_states conf ] (letters conf) 
         (num_visible conf) ( num_hidden conf )
    -- t <- mutate 3 s
    let mr = bisi (s,t)     
    let quality = case mr of 
            Just r  | size r <= div full 2 -> 
                Just ( negate $ size r )
            _ -> Nothing
    return ( quality, (s,t,mr))
    
    
make_quiz :: Make    
make_quiz = quiz STS_Bisi example_config