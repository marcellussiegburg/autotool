module Rewriting.SRS.Steps where

import Rewriting.SRS.Step
import Rewriting.SRS.Raw

import Autolib.TES.Rule
import Autolib.Symbol

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Multilingual

import Data.List ( inits, tails )
import Data.Maybe

-- | list all possible rewrite steps
-- starting from given term
steps :: (  Symbol c )
      => SRS  c
      -> [c]
      -> [ Step c ]
steps rs w = do
    ( p, rest ) <- zip [0..] $ tails w
    ( k, r ) <- zip [ 0 .. ] $ rules rs
    let ( pre, post ) = splitAt ( length $ lhs r ) rest
    guard $ lhs r == pre
    return $ Step
           { rule_number = k
           , position = p
           }


-- | execute one rewrite step
exec ::  ( Symbol c )
     => SRS c
     -> [ c ] 
     -> Step c 
     -> Reporter [ c ]
exec srs w step = do
    inform $ vcat
           [ multitext [(DE, "Anwenden des Ersetzungsschrittes")
		       ,(UK, "apply step")
		       ]
           , nest 4 $ toDoc step
           , multitext [(DE, "auf das Wort")
		       ,(UK, "to word")
		       ]
           , nest 4 $ toDoc w
           ]
    let k = rule_number step
    inform $ multitext [(DE, "die Regel Nummer")
		       ,(UK, "the rule number")
		       ] 
	   <+> toDoc k
    rule <- if k < length ( rules srs )
         then do
             let rule = rules srs !! k
             inform $ multitext [(DE, "ist"), (UK, "is")] 
		    <+> toDoc rule
             return rule
         else reject $ multitext [(DE, "existiert nicht.")
				 ,(UK, "does not exist.")
				 ]

    let p = position step
        ( pre, midpost ) = splitAt p w
    inform $ multitext [(DE, "das Teilwort an Position")
		       ,(UK, "the subword at position")
		       ] 
	   <+> toDoc p 
    if p > length w 
         then reject $ multitext [(DE, "existiert nicht")
				 ,(UK, "does not exist")
				 ]
         else inform  $ multitext [(DE, "ist"), (UK, "is")] 
		      <+> toDoc midpost

    let ( mid, post ) = splitAt ( length $ lhs rule ) midpost
    assert ( mid == lhs rule )
           $ multitext [(DE, "linke Regelseite ist Präfix des Teilwortes an Position?")
		       ,(UK, "lhs is prefix of subword at position?")
		       ]
    inform $ multitext [(DE, "Suffix ist"), (UK, "suffix is")] 
	   <+> toDoc post

    let res = pre ++ rhs rule ++ post
    inform $ multitext [(DE, "resultierendes Wort ist")
		       ,(UK, "the resulting word is")
		       ] 
           $$ ( nest 4 $ toDoc res )

    return res

