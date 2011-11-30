-- | failure semantics

{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module CSP.STS.Fail where

import Challenger.Partial
import Inter.Types
import Inter.Quiz


import CSP.STS.Type
import CSP.STS.Dot
import CSP.Fail.Compute
import CSP.Fail.Quiz
import Autolib.Dot.Dotty ( peng )

import Autolib.Set
import qualified Data.Set as S
import qualified Data.Map as M
import Autolib.ToDoc
import Autolib.Reader

import Autolib.NFA.Shortest ( is_accepted )

import Autolib.Reader
import Autolib.Reporter
import Data.Typeable


data STS_Fail = STS_Fail deriving ( Read, Show, Typeable )

instance OrderScore STS_Fail where
    scoringOrder _ = Increasing
    
instance Partial STS_Fail 
         ( STS Int Char, STS Int Char )    
         ( [ Char ], Set Char ) where
    report p ( a, b ) = do           
        inform $ vcat
          [ text "Begründen Sie durch ein Beispiel,"
          , text "daß die folgenden Zustandsübergangssysteme"
          , text "unterschiedliche Ablehnungssemantik besitzen:" 
          ]  
        inform $ text "A = " <+> toDoc a
        peng a
        inform $ text "B = " <+> toDoc b
        peng b
        
    initial p (a,b) = 
        ( take 2 $ S.toList $ alphabet a , alphabet a )
    
    total p (a,b) (w,r) = do
        pa <- compute ( text "A" ) a (w,r)
        pb <- compute ( text "B" ) b (w,r)
{-
        let fa = failures a
            fb = failures b 
        let u = map Right w ++ [ Left r ]
        let pa = is_accepted fa u     
            pb = is_accepted fb u
        inform $ text "Gehört diese Ablehnung zur Semantik von A?" </> toDoc pa    
        inform $ text "Gehört diese Ablehnung zur Semantik von B?" </> toDoc pb
-}
        when ( pa == pb ) $ reject $ text  "Die Antworten dürfen nicht übereinstimmen."


compute name s (w,r) = nested 4 $ do
    inform $ text "Gehört diese Ablehnung zur Semantik von" 
           <+> name <+> text "?"
    nested 4 $ case failure_trace s (w,r) of
        Left msg -> do inform msg ; return False
        Right msg -> do inform msg ; return True

make_fixed :: Make
make_fixed = direct STS_Fail (s1, s2)

(s1, s2) =   (STS { start = 3 :: Int , alphabet = mkSet [ 'a' , 'b' ]
    , visible = [ ( 2 , 'b' , 4 ) , ( 2 , 'a' , 4 ) , ( 4 , 'b' , 1 )
                , ( 4 , 'a' , 2 ) , ( 1 , 'b' , 2 ) , ( 2 , 'a' , 2 )
                ]
    , hidden = [ ( 1 , 1 ) , ( 3 , 1 ) , ( 3 , 2 ) ]
    },STS { start = 4 :: Int , alphabet = mkSet [ 'a' , 'b' ]
    , visible = [ ( 4 , 'a' , 4 ) , ( 3 , 'b' , 3 ) , ( 1 , 'a' , 2 )
                , ( 2 , 'b' , 3 ) , ( 4 , 'b' , 3 ) , ( 3 , 'b' , 4 )
                , ( 2 , 'a' , 2 )
                ]
    , hidden = [ ( 3 , 4 ) , ( 1 , 2 ) , ( 4 , 2 ) , ( 3 , 3 ) ]
    })     


data Config = 
     Config { num_states :: Int
            , letters :: [Char]
            , num_visible :: Int
            , num_mutated :: Int  
            , num_hidden :: Int  
            , generator_repeat :: Int  
            }  deriving ( Typeable )
     
example_config :: Config     
example_config = Config
    { num_states = 4
    , num_visible = 6 
    , num_mutated = 2                
    , num_hidden = 0                
    , letters = "ab"               
    , generator_repeat = 1000            
    }                     
     
$(derives [makeReader, makeToDoc] [''Config])


instance Generator STS_Fail Config 
    ( STS Int Char, STS Int Char
    , ( [[ Char]] , [[ Either ( Set Char) Char]] )
    )  where
      generator _ conf key = 
          roll ( letters conf ) ( num_states conf )
               ( num_visible conf ) ( num_hidden conf )
               ( num_mutated conf )
               ( generator_repeat conf )
instance Project STS_Fail 
    ( STS Int Char, STS Int Char
    , ( [[Char]], [[ Either ( Set Char) Char]] )
    )
    ( STS Int Char, STS Int Char ) where
      project _ (a,b,u) = (a,b)

make_quiz :: Make
make_quiz = quiz STS_Fail example_config

