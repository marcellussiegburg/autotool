module Rewriting.TRS.Steps where

import Rewriting.TRS.Step
import Rewriting.TRS

import Type.Tree

import Autolib.TES.Apply ( apply )
import Autolib.TES.Unify ( match )

import Autolib.Schichten
import Autolib.Set
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Multilingual
import Autolib.FiniteMap

import Data.Maybe

-- | list all possible rewrite steps
-- starting from given term
steps :: ( Symbol v, Symbol c )
      => TRS v c
      -> Term v c
      -> [ Step v c ]
steps rs t = do
    ( p, s ) <- positions t
    ( k, r ) <- zip [ 0 .. ] $ rules rs
    sub <- maybeToList $ match ( lhs r ) s
    return $ Step
           { rule_number = k
           , position = p
           , substitution = sub
           }

successors :: ( Symbol v, Symbol c )
      => TRS v c
      -> Term v c
      -> [ Term v c ]
successors rs t = do
    s <- steps rs t
    let Just t' = result $ exec rs t s
    return t'

data Derivation v c = 
    Derivation { goal :: Term v c
               , actions :: [ Step v c ]
               , start :: Term v c
               }

instance ( Symbol v, Symbol c ) => Eq ( Derivation v c ) where
    d == e = goal d == goal e

instance ( Symbol v, Symbol c ) => Ord ( Derivation v c ) where
    d `compare` e = goal d `compare` goal e


reachables ::  ( Symbol v, Symbol c )
          => TRS v c
          -> [ Term v c ]
          -> [ Set ( Derivation  v c )  ]
reachables trs ts = 
     let nach d = mkSet $ do
             let t = goal d
             step <- steps trs t
             maybeToList $ do
                 s <- result $ exec trs t step
                 return $ Derivation 
                        { goal = s
                        , actions = step : actions d
                        , start = start d
                        }
     in  schichten' nach 
             $ mkSet 
             $ map ( \ t -> Derivation { start = t, goal = t, actions = [] } )
             $ ts

-- | execute one rewrite step
exec ::  ( Symbol v, Symbol c )
     => TRS v c
     -> Term v c 
     -> Step v c 
     -> Reporter ( Term v c )
exec trs t step = do
    inform $ vcat
           [ multitext [(DE, "Anwenden des Ersetzungsschrittes")
	     	       ,(UK, "apply step")
		       ]
           , nest 4 $ toDoc step
           , multitext [(DE, "auf den Term")
	     	       ,(UK, "to term")
		       ]
           , nest 4 $ toDoc t
           ]
    let k = rule_number step
    inform $ multitext [(DE, "die Regel Nummer") 
    	     	       ,(UK, "the rule number")
		       ]
	   <+> toDoc k
    rule <- if k < length ( rules trs )
         then do
             let rule = rules trs !! k
             inform $ multitext [(DE, "ist"), (UK, "is")] <+> toDoc rule
             return rule
         else reject $ multitext [(DE, "existiert nicht.")
	      	       		 ,(UK, "does not exist.")
				 ]

    let p = position step
    inform $ multitext [(DE, "der Teilterm an Position")
    	     	       ,(UK, "the subterm at position")
		       ]
	   <+> toDoc p 
    s <- case mpeek t ( position step ) of
         Just s -> do 
             inform $ multitext [(DE, "ist"), (UK, "is")] <+> toDoc s
             return s
         Nothing -> reject $ multitext [(DE, "existiert nicht")
	 	    	     	       ,(UK, "does not exist")
				       ]

    let sub = substitution step
    inform $ multitext [(DE, "die substituierte linke Regelseite ist")
    	     	       ,(UK, "the instantiated lhs is")
		       ] 
    slhs <- mapply sub $ lhs rule
    inform $ toDoc slhs

    assert ( slhs == s )
           $ multitext [(DE, "stimmt überein mit Teilterm an Position?")
	     	       ,(UK, "agrees with subterm at position?")
		       ]

    inform $ multitext [(DE, "die substituierte rechte Regelseite ist")
    	     	       ,(UK, "the instantiated rhs is")
		       ] 
    srhs <- mapply sub $ rhs rule
    inform $ toDoc srhs

    let res = poke t ( p,  srhs )
    inform $ multitext [(DE, "der resultierende Term ist")
    	     	       ,(UK, "the resulting term is")
		       ] 
           $$ ( nest 4 $ toDoc res )
    peng res

    return res

-------------------------------------------------------------------------

mpeek :: Term v c 
      -> Position 
      -> Maybe ( Term v c )
mpeek t [] = return t
mpeek ( Node f args ) (x : xs) = do
    guard $ x < length args
    mpeek ( args !! x ) xs
mpeek _ _ = Nothing

mapply :: ( Symbol v, Symbol w, Symbol c )
       => FiniteMap v ( Term w c)
       -> Term v c
       -> Reporter ( Term w c )
mapply sub ( Var v ) = do
    case lookupFM sub v of
        Just t -> return t
        Nothing -> reject $ fsep
            [ multitext [(DE, "Variable"), (UK, "variable")]
	    , toDoc v
	    , multitext [(DE, "ist nicht gebunden"), (UK,"not bound")] 
	    ]
mapply sub ( Node f args ) = do
    ys <- mapM ( mapply sub ) args
    return $ Node f ys
