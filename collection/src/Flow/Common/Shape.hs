{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language LambdaCase #-}

module Flow.Common.Shape where

import Flow.Common.Data

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import Data.Typeable

data Condition 
    = No_Loops 
    | No_Labels -- ^ program contains no labels
    | No_Gotos -- ^ program contains no gotos
    | Simple_Branches -- ^ just "if (b) goto foo;"
    | Simple_Loops -- ^ no break or continue
    | Flat 
    | And [ Condition ]
    deriving ( Eq, Ord, Typeable )

derives [makeReader,makeToDoc] [''Condition]

check :: Condition -> Statement -> Reporter ()
check c s = let { ss = substatements s } in case c of  
    And cs -> forM_ cs $ \ c -> check c s
    No_Loops -> whine "Das Programm darf keine Schleifen enthalten." 
            $ filter ( \ case While {} -> True ; _ -> False ) ss
    No_Labels -> whine "Das Programm darf keine Marken enthalten." 
            $ filter ( \ case Label {} -> True ; _ -> False ) ss
    No_Gotos -> whine "Das Programm darf keine Sprünge enthalten." 
            $ filter ( \ case Goto {} -> True ; _ -> False ) ss
    Simple_Branches -> whine "Alle Verzweigungen sollen die Form 'if (b) goto l' haben."
            $ filter ( \ case 
                 Branch c y n -> case (y,n) of (Goto _, Nothing) -> False; _ -> True 
                 _ -> False ) ss
    Simple_Loops -> whine "Alle Schleifen sollen einfach sein (ohne break/continue)."
            $ filter ( \ case Break _ -> True ; Continue _ -> True ; _ -> False ) ss
    Flat -> case s of
        Block ss -> whine "Der Block darf keine Blöcke enthalten."
            $ filter ( \ case Block _ -> True ; _ -> False ) $ ss >>= substatements
        _ -> reject $ text "Das Programm soll ein Block sein."

whine msg bad = when (not $ null bad) $ reject $ 
   text msg </> ( text "Folgende Teilprogramme verletzen diese Bedingung:"
                 </> vcat ( map toDoc bad  ) )

