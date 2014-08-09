{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}


-- | this is the general module (top module after refactoring)

module Rewriting.Derive where

import Rewriting.Apply
import Rewriting.Derive.Instance

{-
import Rewriting.Step
import Rewriting.Steps
import Rewriting.Derive.Quiz
import Rewriting.Derive.Config
-}

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Multilingual
import Autolib.Reader
import Autolib.FiniteMap
import Autolib.Size

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Control.Monad
import Data.Typeable


data Derive tag = Derive tag 
    deriving ( Eq, Ord, Typeable )

instance OrderScore ( Derive tag ) where
    scoringOrder _ = Increasing

instance ToDoc tag => ToDoc ( Derive tag ) where
    toDoc ( Derive t ) = text "Derive-" <> toDoc t

instance ToDoc tag => Show (Derive tag) where show = render . toDoc
                          


instance Reader tag => Reader ( Derive tag ) where
    reader = do 
        my_symbol "Derive-"
        tag <- reader
        return $ Derive tag

class ( Reader x, ToDoc x, Typeable x ) => RDT x
instance ( Reader x, ToDoc x, Typeable x ) => RDT x

newtype Solution o a = Solution (o,[a])
    deriving (Reader, ToDoc, Typeable)

instance Size ( Solution o a )  where
    size (Solution (p,xs)) = length xs

instance ( RDT tag, RDT action, RDT object , RDT system
         , Eq object
         , Apply tag system object action 
         )
    => Partial ( Derive tag ) ( Instance system object ) (Solution object action) where

    report ( Derive tag ) inst = do 
        inform $ vcat
            [ multitext [(DE, "gesucht ist für das System")
	      		,(UK, "for the system")
			]
            , nest 4 $ toDoc $ system inst
            , multitext [(DE, "eine Folge von Schritten")
	      		,(UK, "give a sequence of steps")
			]
            , nice $ derivation_restriction inst
            , multitext [(DE,"von"),(UK,"from")] <+> nice ( from inst )
            , multitext [(DE, "nach"),(UK,"to")] <+> nice ( to inst )
            ]

    initial ( Derive tag ) inst =
        let p = case from inst of Fixed o -> o ; Sized _ s -> example_object_of_size tag s 
        in  Solution ( p , take 2 $ actions tag ( system inst ) p  )

    total ( Derive tag ) inst (Solution (start,steps)) = do
        check_object_restriction (multitext [(DE,"Start-Objekt"),(UK,"start object")])
            (from inst) start

        let sys = system inst
        end <- foldM ( apply tag sys ) start steps

        check_object_restriction (multitext [(DE,"Ziel-Objekt"),(UK,"final object")])
            (to inst) end
        
        check_derivation_restriction (multitext [(DE,"Ableitung"),(UK,"derivation")])
            (derivation_restriction inst) (length steps)


make_fixed :: ( RDT tag, RDT action, RDT object , RDT system
         , Eq object
         , Apply tag system object action 
         )
           => tag -> Make
make_fixed tag = direct ( Derive tag ) ( example tag )


{-

make_quiz :: Make
make_quiz = quiz Derive Rewriting.Derive.Config.example


instance (Symbol v, Symbol c, Reader ( TRS v c ) )
    => Generator Derive ( Config v c ) ( Instance v c ) where
    generator Derive conf key = roll conf

instance (Symbol v, Symbol c, Reader ( TRS v c ) )
    => Project  Derive ( Instance v c ) ( Instance v c ) where
    project  Derive inst = inst

firstvar trs = take 1 $ do
    r <- regeln trs
    t <- [ lhs r, rhs r ]
    lvars t
-}

