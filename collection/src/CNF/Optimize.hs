{-# language DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving #-}

module CNF.Optimize where

import OBDD (OBDD)
import qualified OBDD as O

import qualified Autolib.TES.Binu as B
import qualified Autolib.TES.Type as T

import Expression.Op
import qualified Boolean2.Instance as I

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Size

import Inter.Types
import Inter.Quiz

import qualified Data.Set as S
import qualified Data.Map as M


import Data.Typeable

data CNF_Optimize = CNF_Optimize
    deriving ( Read, Show, Typeable )

instance OrderScore CNF_Optimize where
    scoringOrder _ = Increasing

type F = Exp (OBDD Identifier)

instance Partial CNF_Optimize (F, Int) F where

    describe _ (f,k) = vcat
             [ text "Gesucht ist eine Formel G in konjunktiver Normalform,"
             , text "die äquivalent ist zu F:"
             , nest 4 $ toDoc f
             , text "mit  size(G) <= " <+> toDoc k 
             , text "G darf zusätzliche Variablen enthalten,"
             , text "über diese wird existentiell quantifiziert."
             ]

    initial _ (f,k) = f

    partial _ (f,k) g = do
        let extra = S.difference ( variables g ) ( variables f )
        when ( not $ S.null extra ) $ inform $ vcat
             [ text "zusätzliche Variablen:"
             , nest 4 $ toDoc extra
             ]
        mf <- eval f
        mge <- eval g
        let mg = O.exists_many extra mge
            diff = O.binary ( \ x y ->  x /= y ) mf mg
            mods = O.all_models diff
        when ( O.satisfiable diff ) $ reject $ vcat
             [ text "nicht äquivalent, z. B. bei Belegung(en)"
             , nest 4 $ vcat $ map toDoc $ take 3 mods
             ]

    total _ (f,k) g = do
        inform $ text "prüfe konjunktive Normalform"
        let wrong = S.filter ( \ o -> not $ name o `elem` [ "&&", "||", "not" ] ) $ syms g
        when (not $ S.null wrong) $ reject $ vcat
          [ text "diese Operatoren sind nicht zugelassen:"
          , nest 4 $ toDoc wrong
          ]
        I.subcheck "&&" [ "&&", "||", "not" ] g
        I.subcheck "||" [ "||", "not" ] g
        I.subcheck "!"  [] g

        inform $ text "size(G)" <+> equals <+> toDoc (size g)
        assert ( size g <= k ) $ text "size (G) <=" <+> toDoc k

variables t = case t of
  T.Var v -> S.singleton v
  T.Node f args -> S.unions $ map variables args

optable :: M.Map String (Op (OBDD Identifier))
optable = M.fromList $ do
  op <- B.nullary bops ++ B.unary bops ++ B.binary bops
  return (name op, op)

eval :: F -> Reporter (OBDD Identifier)
eval t = case t of
  T.Var v -> return $ O.unit v True
  T.Node f args -> case M.lookup (show f) optable of
    Nothing -> reject $ vcat
      [ text "Fehler in Teilausdruck" <+> toDoc t
      , text "Es gibt keinen Operator" <+> toDoc f
      ]
    Just op -> forM args eval >>= inter op 
    

make_fixed :: Make
make_fixed = direct CNF_Optimize
    ( read "(a <-> b) <-> (c <-> d)" :: F, 10 :: Int )

instance Ops (OBDD Identifier) where
  bops = B.Binu
    { B.nullary = nullary, B.unary = unary, B.binary = binary }

nullary :: [ Op (OBDD Identifier) ]
nullary = do
  u <- [ False, True ]
  return $ Op { name = show u, arity = 0
              , precedence = Nothing, assoc = AssocNone
              , inter = lift0 $ O.constant u
              }

unary :: [ Op (OBDD Identifier) ]
unary = [ Op { name = "not" , arity = 1
		, precedence = Just 10 , assoc = AssocNone
		, inter = lift1 $ O.not
		}
        ]

instance ToDoc (OBDD Identifier) where

deriving  instance Typeable OBDD 
  
binary :: [ Op (OBDD Identifier) ]
binary =
  [ Op { name = "&&" , arity = 2
       , precedence = Just 8 , assoc = AssocLeft
       , inter = lift2 ( O.&& )
       }
  , Op { name = "||" , arity = 2
       , precedence = Just 7 , assoc = AssocLeft
       , inter = lift2 (O.||)
       }
  , Op { name = "<->", arity = 2
       , precedence = Just 6 , assoc = AssocNone
       , inter = lift2 (O.binary (==))
       }
  , Op { name = "->", arity = 2
       , precedence = Just 5 , assoc = AssocNone
       , inter = lift2 (O.binary (<=))
       }
  , Op { name = "xor", arity = 2
       , precedence = Nothing , assoc = AssocNone
       , inter = lift2 ( O.binary (/=))
       }
  ]
    

