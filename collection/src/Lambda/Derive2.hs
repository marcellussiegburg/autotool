{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}

module Lambda.Derive2 where

import Lambda.Type
import Lambda.Position
import Lambda.Tree ( peng )

import qualified Lambda.Quiz ( generator)
import qualified Lambda.Derive.Config as C
import qualified Lambda.Derive.Instance as I

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter hiding ( action )
import Data.Typeable

import qualified Data.Set as S

data Lambda_Derive2 = Lambda_Derive2
    deriving ( Read, Show, Typeable )

data Action = Rename { from :: Identifier, to :: Identifier }
            | Reduce { formal :: Identifier, body :: Lambda, argument :: Lambda }
    deriving ( Typeable )

exampleA0 :: Action
exampleA0 = Rename { from = read "x", to = read "y" }

exampleA1 :: Action
exampleA1 = Reduce { formal = read "x"
                  , body = read "x"
                  , argument = read "\\ x -> x"
                  }

data Step = Step { position :: Position 
                 , action :: Action
                 }
    deriving ( Typeable )

exampleS0 :: Step
exampleS0 = Step { position = [0] , action = exampleA0 }

exampleS1 :: Step
exampleS1 = Step { position = [ ] , action = exampleA1 }

data Instance = Instance { start :: Lambda, goal :: Lambda
                         , steps :: Maybe Int
                         , draw_trees :: Bool
                         }
    deriving ( Typeable )

exampleI :: Instance
exampleI = Instance 
         { start  = read "(\\ x -> x x)(\\ x -> x x)"
         , goal   = read "(\\ x -> x x)(\\ x -> x x)"
         , steps = Just 2
         , draw_trees = False
         }

derives [makeReader, makeToDoc] [''Step, ''Action, ''Instance]

instance Show     Step where show = render . toDoc
instance Show   Action where show = render . toDoc
instance Show Instance where show = render . toDoc

instance Measure Lambda_Derive2 Instance [ Step ] where
    measure p inst xs = fromIntegral $ length xs

instance OrderScore Lambda_Derive2 where
    scoringOrder _ = Increasing

instance Partial Lambda_Derive2 Instance [ Step ] where
    report p inst = do
        let step_info = case steps inst of
               Nothing -> empty
               Just s  -> text "der Länge" <+> toDoc s
        inform $ vcat
            [ fsep [ text "Gesucht ist eine Ableitung"
                   , step_info, text ", die"
                   ]
            , nest 4 $ toDoc $ start inst
            ]
        when (draw_trees inst) $ peng $ start inst        
        inform $ vcat 
            [ text "überführt in"
            , nest 4 $ toDoc $ goal inst
            ]
        when (draw_trees inst) $ peng $ goal inst

    initial p inst = [  exampleS1, exampleS0 ]

    partial p inst steps = do
        return ()

    total p inst steps = do
        result <- derive2 (draw_trees inst) ( start inst ) steps
        check_result inst result
        check_length inst steps

check_length inst xs = do
    case steps inst of
        Nothing -> return ()
        Just s -> assert ( s == length xs )
                         $ text "Ableitungslänge korrekt?"
                           
check_result inst t = do
    assert ( goal inst == t )
           $ text "Ableitungsergebnis korrekt?"

make_fixed :: Make
make_fixed = direct Lambda_Derive2 exampleI

instance Generator Lambda_Derive2 C.Type Instance where
    generator p conf key = do
        i <- Lambda.Quiz.generator conf key
        return $ Instance 
              { start = I.from i
              , goal = I.to i
              , steps = I.steps i
              , draw_trees = C.draw_trees conf
              }

instance Project Lambda_Derive2 Instance Instance where
    project p i = i


make_quiz :: Make
make_quiz = quiz Lambda_Derive2 C.example



derive2 :: Bool -> Lambda -> [ Step ] -> Reporter Lambda
derive2 drawing t steps = do
    t' <- foldM (step drawing) t steps
    inform $ vcat [ text "Ergebnis der Ableitung ist", nest 4 $ toDoc t' ]
    return t'

step :: Bool -> Lambda -> Step -> Reporter Lambda
step drawing t s = do
    inform $ vcat 
        [ text "*****************************************************"
        , text "Aktueller Term ist" 
        , nest 4 $ toDoc t 
        ]
    when drawing $ peng t

    inform $ vcat [ text "Aktueller Schritt ist", nest 4 $ toDoc s ]

    inform $ vcat [ text "Teilterm an Position" <+> toDoc (position s) <+> text "ist"]
    u  <- peek t ( position s )
    inform $ nest 4 $ toDoc u

    u' <- substep u ( action s )
    t' <- poke t ( position s, u' )
    
    return t'

substep :: Lambda -> Action -> Reporter Lambda
substep t act = do
    -- inform $ vcat [ text "Anwendung von" <+> toDoc act
    --               , text "auf Teilterm" <+> toDoc t
    --               ]
    t' <- case act of
      Rename {} -> do
        -- inform $ text "Umbenennung eines formalen Parameters einer Abstraktion"
        case t of
            Abstract f b -> do
                when ( f /= from act ) $ reject $ text
                    "der formale Parameter ist nicht" <+> toDoc (from act)
                when ( S.member (to act) $ Lambda.Type.free_variables b ) $ reject
                    $ text "Fehler:" <+> vcat
                         [ text "bei Umbenennung von" <+> toDoc f 
                         , text "nach" <+> toDoc (to act)
                         , text "in" <+> toDoc t
                         , text "wird ein freies Vorkommen einer Variablen gebunden."
                         ]
                liftM (Abstract $ to act) $ substitute f (Variable $ to act) b
            _ -> reject $ text "Dieser Term ist keine Abstraktion."
       
      Reduce {} -> do
        -- inform $ text "Reduktion eines Redexes"
        case t of
            Apply (Abstract f b) a -> do
                when ( f /= formal act ) $ reject $ text
                    "der formale Parameter ist nicht" <+> toDoc (formal act)
                when ( b /= body act ) $ reject $ text
                    "der Funktionskörper ist nicht" <+> toDoc (body act)
                when ( a /= argument act ) $ reject $ text
                    "das Argument ist nicht" <+> toDoc (argument act)
                substitute f a b
            _ -> reject $ text "Dieser Term ist kein Redex."

    inform $ vcat [ text "Resultat der Anwendung ist" <+> toDoc t' ]
    return t'

-- | substitute  x  by  a  in  b
substitute x a b = case b of
    Variable y -> return $ if y == x then a else b
    Apply l r -> liftM2 Apply (substitute x a l) (substitute x a r)
    Abstract y c -> 
      if y == x then return b else do
        when ( S.member y ( Lambda.Type.free_variables a ) ) $ reject 
            $ text "Fehler:" <+> vcat
            [ text "bei Ersetzung von" <+> toDoc x
            , text "durch" <+> toDoc a 
            , text "in" <+> toDoc b
            , text "wird eine freies Vorkommen einer Variablen gebunden."
            ]
        liftM (Abstract y) (substitute x a c)

