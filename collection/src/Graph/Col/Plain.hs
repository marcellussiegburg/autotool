{-# language OverloadedStrings #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}

module Graph.Col.Plain where

import Graph.Util
import Graph.Color

import Autolib.Graph.Ops ( gmap )
import Autolib.Graph.Kneser ( petersen )
import Autolib.Dot ( peng, Layout_Program (..) )

import qualified Autolib.Multilingual as M
-- import qualified Text.PrettyPrint.HughesPJ as T
import Data.String 

import Inter.Types
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Size
import Autolib.FiniteMap
import qualified Challenger as C

import qualified Autolib.Reporter.Set
import Data.Typeable

data Col = Col deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Col where
    scoringOrder _ = Increasing

instance ( GraphC a, Show a )
    => C.Partial Col ( Integer, Graph a ) ( FiniteMap a Color ) where

    report p (c, g) = do
        inform $ vcat
	    [ M.make [ (M.DE, "Gesucht ist eine konfliktfreie Knoten-Färbung des Graphen")
                     , (M.UK, "Give a conflict free colouring of")
                     ]
	    , nest 4 $ toDoc g
	    ]
        peng $ g { layout_program = Dot
		 , layout_hints = [ "-Nshape=ellipse" ]
		 }
	inform $ fsep 
	    [ M.make [ ( M.DE, "mit höchstens" )
                     , ( M.UK, "with at most" )  
                     ]
            , toDoc c
            , M.make [ ( M.DE, "verschiedenen Farben." ) 
                     , ( M.UK, "different colours." )
                     ]
            ]

    initial p (c, g) = listToFM $ do
        v <- lknoten g
        let col = toEnum $ fromIntegral $ hash v `mod` 3
	return ( v, col )

    partial p (c, g) f = do
        let s1 = ( M.make [ ( M.DE, "Knotenmenge des Graphen" )
                          , ( M.UK, "node set of graph" )
                          ]
                 , knoten g 
                 )
	    s2 = ( M.make [ ( M.DE, "gefärbte Knoten" )
                          , ( M.UK, "coloured nodes" )
                          ]
                 , mkSet $ keysFM f 
                 )
	Autolib.Reporter.Set.subeq s1 s2
    
    total p (c, g) f = do
        let col v = lookupWithDefaultFM f (error $ "Graph.Col.Plain" ++ show v) v
        let fg = gmap ( \ v ->  (v, col v ) ) g 
        inform $ vcat 
               [ M.make [ ( M.DE, "Der gefärbte Graph ist" )
                        , ( M.UK, "The coloured graph is" )
                        ]
               ]
        peng $ fg { layout_program = Dot
		  , layout_hints = [ "-Nshape=ellipse" ]
		  }
	let wrong = do
	      k <- lkanten g
	      guard $ col (von k) == col (nach k)
	      return k
        when ( not $ null wrong ) $ reject $ vcat
	     [ M.make [ (M.DE, "Diese Kante(n) verlaufen zwischen gleichfarbigen Knoten:" )
                      , (M.UK, "These edge(s) connect nodes of equal colour:")
                      ]
	     , nest 4 $ toDoc wrong
	     ]
	inform $ M.make [ ( M.DE, "Die Färbung ist konfliktfrei." )
                        , ( M.UK, "The colouring is free of conflicts." )
                        ]
        let cc = C.measure p (c, g) f 
	inform $ M.make [ (M.DE, "Sie benutzt"), (M.UK, "it uses" ) ]
		 <+> toDoc cc
		 <+> M.make [ (M.DE, "Farben.") , (M.UK, "colours.") ]
	when ( cc > c ) $ reject 
             $ M.make [ ( M.DE, "erlaubt sind aber höchstens" )
                      , ( M.UK, "but the largest number allowed is")
                      ]
             <+> toDoc c

instance ( GraphC a, Show a )
    => C.Measure Col ( Integer, Graph a ) ( FiniteMap a Color ) where
    measure p i f = fromIntegral $ cardinality $ mkSet $ eltsFM f 

make :: Make
make = direct Col ( 3 :: Integer, petersen )



