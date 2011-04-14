{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}

module Convert.Input where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Set
import Data.Typeable

import Autolib.NFA ( NFAC )
import qualified Autolib.NFA
import qualified Autolib.Exp
import qualified Autolib.Exp.Inter
import qualified Autolib.Exp.Sanity

import qualified Exp.Property
import qualified NFA.Property
import qualified NFA.Test

import qualified CSP.Syntax
import qualified CSP.Trace
import qualified CSP.Property.Guarded
import qualified CSP.Property.Right_Linear

data Autolib.NFA.NFAC c Int => 
     Input c = NFA ( Autolib.NFA.NFA c Int )
	     | Exp ( Autolib.Exp.RX c ) -- ^ for backward compatibility
             | Regular_Expression { alphabet :: Set c
		 , expression ::  Autolib.Exp.RX c 
		 }
             | Process ( CSP.Syntax.Process c )
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Input])

example :: Input Char
example = Regular_Expression { alphabet = mkSet "ab"
	      , expression = read "(a + b^2)^*"
	      } 

verify_source ::  Autolib.NFA.NFAC c Int 
     => Input c -> Reporter ()
verify_source (NFA aut) = do
    NFA.Test.test NFA.Property.Sane aut
verify_source (Exp e ) = do
    return ()
verify_source (e @ Regular_Expression {}) = do
    Autolib.Exp.Sanity.sanity_alpha 
        ( alphabet e ) ( expression e )
verify_source (Process p) = do
    CSP.Property.Guarded.check p
    CSP.Property.Right_Linear.check p

lang :: Autolib.NFA.NFAC c Int 
     => Input c -> Doc
lang (NFA aut) =
    vcat [ text "die von diesem Automaten akzeptierte Sprache:"
	 , nest 4 $ toDoc aut
	 ]
lang (Exp e) = 
    vcat [ text "die von diesem Ausdruck erzeugte Sprache:"
	 , nest 4 $ toDoc e
	 ]
lang (e @ Regular_Expression {}) =
    vcat [ text "die von diesem Ausdruck erzeugte Sprache:"
	 , nest 4 $ toDoc $ expression e
	 , text "über dem Alphabet" <+> toDoc ( alphabet e )
	 ]
lang (Process p) =     
    vcat [ text "die Spursprache des Prozesses"
         , nest 4 $ toDoc p  
         ]  

instance Autolib.NFA.NFAC c Int => Nice ( Input c ) where
     nice = lang

min_det_automaton :: Autolib.NFA.NFAC c Int 
		  => Input c -> Autolib.NFA.NFA c Int
min_det_automaton (NFA aut) = 
    Autolib.NFA.minimize0 aut    
min_det_automaton (e @ Regular_Expression {}) = 
    Autolib.Exp.Inter.inter_det
              ( Autolib.Exp.Inter.std_sigma $ setToList $ alphabet e ) 
              ( expression e )
min_det_automaton ( Exp e ) = 
    error "Convert.Input.min_det_automaton: use Regulare_Expression instead of Exp"
min_det_automaton ( Process p ) 
    = Autolib.NFA.minimize0 
    $ Autolib.NFA.normalize  
    $ CSP.Trace.auto p
    

