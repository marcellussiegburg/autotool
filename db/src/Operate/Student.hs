{-# LANGUAGE PatternSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Operate.Student where

import Operate.Types
-- import Inter.Evaluate

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

import qualified Text.XHtml

import Autolib.Multilingual hiding ( Make )

import qualified Autolib.Multilingual as M
import qualified Autolib.Multilingual.Html as H

import Autolib.Reporter.IO.Type hiding ( wrap )
import Autolib.ToDoc
import Autolib.Reader
import qualified Autolib.Output
import qualified Control.Exception as CE

import Data.Typeable
import Data.Maybe
import Control.Monad ( when, mzero )

data Method = Textarea | Upload
    deriving ( Eq, Show, Typeable )

-- | eingabe und bewertung der lösung
-- für tutor zum ausprobieren
-- für student echt
solution vnr  manr stud auf = do

    lang <- Operate.Language.choose

    ( sti, ini, icom ) <- make_instant vnr manr stud auf

    let past = mkpar stud auf

    -- let ini  = initial p i
    br
    parameter_table auf

    h3 "Aufgabenstellung"
    html $ specialize lang icom

    when ( not $ A.current auf ) vorbei

    ---------------------------------------------------------
    html $ Text.XHtml.anchor Text.XHtml.! [ Text.XHtml.name "hotspot" ]
         Text.XHtml.<< ""

    h3 "Neue Einsendung"

    open table
    method <- click_choice_with_default 0 "Eingabe-Methode"
       [ ( "Textfeld", Textarea ), ( "Datei-Upload", Upload ) ]
    close

    mcs <- case method of
        Textarea -> do
	    ex    <- submit "Beispiel laden"
	    prev  <- submit "vorige Einsendung laden"
	    -- esub  <- submit "Textfeld absenden"
	    br
	    when ( ex || prev ) blank

            let b0 = render $ toDoc ini 
	    def <- io $ if prev 
	      then Operate.Store.latest Operate.Store.Input past
                      `CE.catch` \ (CE.SomeException _) -> return b0
	      else return b0
	    open table
	    open row
            sol <- textarea def
	    esub  <- submit "Textfeld absenden"
	    close -- row
	    open row
{-            
            let helper :: Text.XHtml.Html
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
            plain "Datei auswählen:"
            up <- file undefined
	    fsub  <- submit "Datei absenden"
            when ( not fsub ) $ mzero -- break
	    return up

    Just cs <- return mcs
    hr ; h3 "Neue Bewertung"
    -- (res, o ) <- io $ run $ evaluate p i cs
    (res, o ) <- io $ evaluate auf sti cs
    
    let com = Autolib.Output.render o :: H.Html
    html $ specialize lang com
    return ( Just icom, Just cs, fromMaybe No res, Just com )

parameter_table auf = do
    h3 $ unwords [ "Aufgabe", toString $ A.name auf ]
    above ( plain "Hinweise" )
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
    io $ make_instant_common vnr manr stud auf


