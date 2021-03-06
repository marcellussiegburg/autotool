{-# LANGUAGE PatternSignatures #-}
{-# LANGUAGE PatternSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Operate.Student where

import Operate.Types
-- import Inter.Evaluate

import Types.Documented as D
import Types.Solution
import qualified Util.Xml.Output as UXO

import Operate.Bank
import Operate.Common
import qualified Operate.Language
import qualified Operate.Store

import Gateway.CGI
import Gateway.Help

import qualified Operate.Param as P
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import Control.Types (toString, VNr, MNr)
import Challenger.Partial

import qualified Gateway.Html

import Autolib.Multilingual hiding ( Make )

import qualified Autolib.Multilingual as M
import qualified Autolib.Multilingual.Html as H

import Autolib.Reporter.IO.Type hiding ( wrap )
import Autolib.ToDoc
import Autolib.Reader
import qualified Autolib.Output
import qualified Autolib.Output as O
import qualified Control.Exception as CE

import Data.Typeable
import Data.Maybe
import Control.Monad ( when, mzero )

import Control.SQL (logged)

data Method = Textarea | Upload
    deriving ( Eq, Show, Typeable )

-- | eingabe und bewertung der lösung
-- für tutor zum ausprobieren
-- für student echt
solution vnr  manr stud auf = do
    lang <- get_preferred_language -- ; plain $ show lang

    ( sti, docsol , icom ) <- make_instant vnr manr stud auf
    let SString sol = D.contents docsol
        DString doc = D.documentation docsol
        out = UXO.xmlStringToOutput doc
        ini = text sol

    let past = mkpar stud auf

    -- let ini  = initial p i
    br
    parameter_table auf

    h3 $ specialize lang 
       $ M.make [(DE, "Aufgabe"), (UK, "Problem")]
    html $ specialize lang ( Autolib.Output.render icom :: H.Html)

    when ( not $ A.current auf ) vorbei

    ---------------------------------------------------------
    html $ Gateway.Html.anchor Gateway.Html.! [ Gateway.Html.name "hotspot" ]
         Gateway.Html.<< ""

    h3 $ specialize lang
       $ M.make [(DE, "Lösung"), (UK, "Solution")]

    open table
    method <- click_choice_with_default 0 "Eingabe-Methode"
       [ ( "Textfeld", Textarea ), ( "Datei-Upload", Upload ) ]
    close

    mcs <- case method of
        Textarea -> do
	    ex    <- submit $ specialize lang
	    	     	    $ M.make [(DE, "Beispiel laden")
			      	     ,(UK, "load example")
				     ]
	    prev  <- submit $ specialize lang
	    	     	    $ M.make [(DE, "vorige Einsendung laden")
			      	     ,(UK, "load previous solution")
				     ]
	    -- esub  <- submit "Textfeld absenden"
	    br
	    when ( ex || prev ) blank

            let b0 = render $ ini 
	    def <- io $ if prev 
	      then Operate.Store.latest Operate.Store.Input past
                      `CE.catch` \ (CE.SomeException _) -> return b0
	      else return b0
	    open table

	    open row
            sol <- textarea def
            close -- row

            open row 
            open table
            open row
            plain $ specialize lang
                  $ M.make 
                  [ (DE, "Der Typ der Lösung ist:" )
		  , (UK, "solution is of type:")
	          ]
            html $ specialize lang 
                 $ ( O.render :: O.Output -> H.Html )
                 $ out
            close ; close ; close

            open row
	    esub  <- submit $ specialize lang
			    $ M.make [(DE, "Textfeld absenden")
				     ,(UK, "submit textarea")
				     ]
	    close -- row


	    open row
{-            
            let helper :: Gateway.Html.Html
                helper = specialize lang
                    $ Autolib.Output.render 
                 $ Autolib.Output.Beside
                      ( Autolib.Output.Doc
		      $ multitext [ (DE, "ein Ausdruck vom Typ" )
				  , (UK, "an expression of type")
				  ] )
                      ( help ini )
            html helper -- FIXME
-}
	    close -- row
	    close -- table
            return sol

	Upload -> do
            plain $ specialize lang
		  $ M.make [(DE, "Datei auswählen:")
			   ,(UK, "choose file:")
			   ]
            up <- file undefined
	    fsub  <- submit $ specialize lang
			    $ M.make [(DE, "Datei absenden")
				     ,(UK, "submit file")
				     ]
            when ( not fsub ) $ mzero -- break
	    return up

    Just cs <- return mcs
    hr ; h3 $ specialize lang
	    $ M.make [(DE, "Bewertung")
		     ,(UK, "Evaluation")
		     ]
    -- (res, o ) <- io $ run $ evaluate p i cs
    (res, o ) <- io $ evaluate auf sti cs lang
    
    let com = Autolib.Output.render o :: H.Html
    html $ specialize lang com
    return ( Just icom, Just cs, fromMaybe No res, Just com )

parameter_table auf = do
    lang <- get_preferred_language
    h3 $ unwords [ specialize lang 
		   $ M.make [(DE, "Aufgabe"), (UK, "Problem")]
		 , toString $ A.name auf 
		 ]
    
    io $ logged $ unlines
      [ unwords [ "plain", toString $ A.remark auf ]
      -- , unwords [ "enc"  , CBUS.encodeString $   toString $ A.remark auf ]
      -- , unwords [ "dec"  , CBUS.decodeString $   toString $ A.remark auf ]        
      ]
        
    above ( plain $ specialize lang $ M.make [(DE, "Hinweise"), (UK, "Remark")] )
	            ( pre $ toString $ A.remark auf )


make_instant vnr manr stud auf = do
    -- let conf = read $ toString $ A.config auf
{-    
    conf <- 
        case parse ( parsec_wrapper 0 ) "input" $ toString $ A.config auf of
	     Right ( x, rest ) -> return x
	     Left err -> do
		  plain "should not happen: parse error in A.config"
		  pre $ show err
		  pre $ show $ A.config auf
		  mzero
    io $ make_instant_common vnr manr stud $ fun conf
-}
    make_instant_common vnr manr stud auf


