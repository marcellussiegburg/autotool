{-# OPTIONS -fglasgow-exts #-}

module Code.Nonprefix where

import Code.Formal

import Challenger.Partial
import Inter.Types

import Autolib.Reporter
import Autolib.Set
import Autolib.ToDoc

import Data.Typeable

data Nonprefix = Nonprefix deriving ( Read, Show, Typeable )

instance Partial Nonprefix () ( Set String ) where
    describe Nonprefix () = vcat
        [ text "Gesucht ist ein (kleiner) Code,"
	, text "der keine Präfixcode ist"
	, text "und dessen Spiegelbild kein Präfixcode ist."
	, text ""
	, parens ( text "Als Größe zählt hier die Summe der Wortlängen." )
	]
    initial Nonprefix () = mkSet [ "10", "11" ]
    partial Nonprefix () ws = do
        prefix_free ( text "" ) ws
	prefix_free ( text "der Spiegelbilder" ) $ smap reverse ws
    total   Nonprefix () ws = do
        inform $ text "Ist die Menge" <+> toDoc ws <+> text "ein Code?"
        case code_counter_examples $ setToList ws of
	    [] -> inform $ text "Ja."
	    w : _ -> reject $ vcat
	        [ text "Nein."
		, nest 4 $ text "Hinweis: betrachten Sie das Wort"
		, nest 8 $ text (take (max 1 (length w `div` 2)) w ++ "..." )
		]

instance Measure Nonprefix () ( Set String ) where
    measure Nonprefix () ws = 
        fromIntegral $ sum $ map length $ setToList ws

make_fixed = direct Nonprefix ()