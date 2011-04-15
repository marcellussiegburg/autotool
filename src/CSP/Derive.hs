{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module CSP.Derive where

import CSP.Syntax
import CSP.Step
import CSP.Tree
import qualified CSP.Derive.Quiz as Q

import Challenger.Partial
import Inter.Types
import Inter.Quiz
import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Data.Typeable
import qualified Data.Set as S

data CSP_Derive = CSP_Derive 
    deriving ( Read, Show, Typeable )

instance Measure CSP_Derive ( Instance a ) [ Int ] where
    measure p inst xs = fromIntegral $ length xs

instance OrderScore CSP_Derive where
    scoringOrder _ = Increasing


data Ord a => Instance a = 
     Instance { start :: Process a 
              , steps :: Maybe Int  
              , goal    :: Goal a 
              }  
     deriving ( Typeable )
              
data Ord a => Goal a 
      = Target ( Process a )
      | Rejects ( Set a )  
     deriving ( Typeable )   
      
              
$(derives [makeReader, makeToDoc] [''Instance, ''Goal])


instance Partial CSP_Derive ( Instance Char ) [ Int ] where
    report p inst = do
        let step_info = case steps inst of
               Nothing -> empty
               Just s  -> text "der Länge" <+> toDoc s
        inform $ vcat
            [ fsep [ text "Gesucht ist eine Ableitung"
                   , step_info, text ", die hier beginnt:"
                   ]
            , nest 4 $ toDoc $ start inst
            ]
        peng $ start inst        
        case goal inst of
           Target t -> do
              inform $ vcat 
                [ text "und hier endet:"
                , nest 4 $ toDoc t
                ]
              peng t
           Rejects r -> do
              inform $ text "und zu einem stabilen Zustand mit dieser Ablehnungsmenge führt:"
                  </> toDoc r
              
        inform $ vcat
            [ text "Jeder Ableitungsschritt wird durch eine Nummer"
            , text "aus einer Liste von möglichen Schritten bezeichnet."
            ]
        
    initial p inst = [ 0 ]

    partial p inst xs = do
        result <- derive ( start inst ) xs
        return ()

    total p inst xs = do
        result <- silent $ derive ( start inst ) xs
        check_length inst xs
        check_result inst result ( alphabet ( start inst ))

-- single :: ( Eq a, ToDoc a  )
--        =>   Process a -> Int -> Reporter ( Process a )
        
status p  =  do
    inform $ text "aktueller Prozeß ist" </> toDoc p
    peng p
    case     successors p of
        [] -> inform $ text "keine Übergänge (deadlock)"
        tqs -> inform $ text "mögliche Übergänge und Nachfolger sind" 
            </> vcat ( map ( \ (k,(t,q)) -> 
             ( toDoc k 
             <+> text ":" <+> text ( unwords $ words $ show  t ) )
             </> text "Nachfolger" <+> toDoc q ) 
                  $ zip [0 :: Int .. ] tqs )        
       
       
single p k = do
    let tqs = successors p
    inform $ text "Sie wählen Nummer" <+> toDoc k  
    silent $ assert ( 0 <= k && k < length tqs )
           $ text "Nummer zulässig?"
    let (t,q) = tqs !! k
    inform $ text ( unwords $ words $ show t )    
    status q    
    return q    

derive start xs = do
    status start
    foldM single start xs

check_length inst xs = do
    case steps inst of
        Nothing -> return ()
        Just s -> assert ( s == length xs )
                         $ text "Ableitungslänge korrekt?"
                           
check_result inst t sigma = do
    inform $ text "Ableitungsergebnis korrekt?"
    case ( goal inst ) of
        Target g -> assert ( g == t )
                      $ text "stimmt mit Ziel überein?"
        Rejects r -> do            
            assert ( null $ tau t ) 
                 $ text "der Prozeß ist stabil?"
            let acc = S.fromList $ map fst $ real t  
                rej = S.difference sigma acc
            inform $ text "Der Prozeß lehnt ab:"  
              </> toDoc rej
            when  ( rej /= r ) $ reject   
                 $ text "aber gefordert war" </> toDoc r


make_fixed :: Make
make_fixed = direct CSP_Derive 
    $ Instance { start = Pre 'a' Stop
               , goal = Rejects ( mkSet "a" )
               , steps = Nothing       
               }          

instance Generator CSP_Derive ( Q.Config Char ) 
         ( Instance Char ) where
    generator _ conf key = case Q.goal_type conf of
        Q.Target -> do
          (dist,(p,q)) <- Q.target_roller conf
          return $ Instance { start = p, goal = Target q
                            , steps = Nothing }
        Q.Rejects -> do
          (dist,(p,r)) <- Q.reject_roller conf
          return $ Instance { start = p, goal = Rejects r
                            , steps = Nothing }

instance Project CSP_Derive ( Instance Char ) 
         ( Instance Char ) where
    project p = id

make_quiz :: Make
make_quiz =  quiz CSP_Derive $ Q.config 



