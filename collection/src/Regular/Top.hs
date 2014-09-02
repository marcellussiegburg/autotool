{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DatatypeContexts #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language DeriveDataTypeable #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveDataTypeable #-}

module Regular.Top where

import Regular.Type

import Autolib.NFA.Eq


import Autolib.NFA ( NFA )
import qualified Autolib.NFA.Example 
import qualified NFA.Property

import Autolib.Exp ( RX )
import qualified Exp.Property
import qualified Autolib.Exp.Inter as E

import qualified Autolib.Logic.Formula.FO as L
import qualified Regular.Logic as L

import qualified Grammatik as G
import qualified Grammatik.Property as G

import Inter.Types

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash

import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter
import Autolib.Informed
import Autolib.Size

data Regular from to = Regular
    deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore ( Regular from to ) where
    scoringOrder _ = Increasing

data (RegularC from, RegularC to ) => Config from to = Config from [ Property to ] deriving (   Typeable )

derives [makeReader, makeToDoc] [''Config]

instance ( RegularC from, RegularC to ) => C.Verify ( Regular from to ) (Config from to) where
    verify p ( Config given spec ) = return ()

instance ( RegularC from, RegularC to ) => C.Partial ( Regular from to ) ( Config from to) to where

    report p  ( Config given spec :: Config from to )  = do
        inform $ vcat
            [ text "Gegeben ist" <+> bestimmt given , nest 4 $ toDoc given ]
        inform $ vcat 
            [ text "Gesucht ist" <+> unbestimmt ( undefined :: to )
            , text "mit folgenden Eigenschaften"
            , nest 4 $ vcat (map toDoc spec)
            ]

    initial p ( Config given props  ) = 
        Regular.Type.initial  props

    partial p ( Config from props ) to = do
        validate props to

    total p ( Config from props  ) to = do
        alpha <- alphabet ( to `asTypeOf` undefined ) props 
        flag <- nested 4 $ do
             f <- semantics alpha from
             t <- semantics alpha to
             equ ( informed ( text "Sprache der Aufgabenstellung" ) f )
                 ( informed ( text "Sprache Ihrer Einsendung" ) t )
        when (not flag) $ reject $ text ""


instance ( RegularC from, RegularC to, Size to ) => C.Measure ( Regular from to ) ( Config from to ) to where
    measure p (Config from props ) to = fromIntegral $ size to


make :: (  RegularC to, RegularC from)
     => String 
     -> Config from to  
     -> Make
make tag (conf :: Config from to) = 
    let t = "Regular" ++ "." ++ tag
    in  Make (Regular :: Regular from to) t
        ( \ inst -> Var
            { problem = Regular 
            , tag = t
            , key = \ matrikel -> return matrikel
            , generate = \ salt cachefun -> 
                  return $ return inst
            } :: Var (Regular from to)
                     (Config from to)
                     to
        ) ( \ _con -> return () ) -- verify
        conf

-- urgh: quadratic number of conversions

-- nfa 2 (nfa, exp, gram, fo)

make_nfa2nfa :: Make
make_nfa2nfa = make "nfa2nfa"
   ( Config  Autolib.NFA.Example.example 
             NFA.Property.example 
     :: Config (NFA Char Int) (NFA Char Int) )

make_nfa2exp :: Make
make_nfa2exp = make "nfa2exp"
    ( Config (E.inter E.std $ read  "a (a+b)^* b" )
             Exp.Property.example 
       :: Config (NFA Char Int) (RX Char) )

make_nfa2gram :: Make
make_nfa2gram = make "nfa2gram"
   ( Config Autolib.NFA.Example.example [ G.Rechtslinear ]
       :: Config (NFA Char Int) G.Grammatik )

make_nfa2fo :: Make
make_nfa2fo = make "nfa2fo"
   ( Config Autolib.NFA.Example.example L.example
      :: Config (NFA Char Int) L.Formula )

-- exp 2 (nfa, exp, gram, fo)

make_exp2nfa :: Make
make_exp2nfa = make "exp2nfa"
    ( Config (read  "a (a+b)^* b" )
             NFA.Property.example 
       :: Config (RX Char) (NFA Char Int) )

make_exp2exp :: Make
make_exp2exp = make "exp2exp"
   ( Config (read "(a+b)^* - a^* b^*" )
            Exp.Property.example
     :: Config (RX Char) (RX Char) )

make_exp2gram :: Make
make_exp2gram = make "exp2gram"
    ( Config (read  "a (a+b)^* b" )
            [ G.Rechtslinear ] 
       :: Config (RX Char) G.Grammatik )

make_exp2fo :: Make
make_exp2fo = make "exp2fo"
    ( Config (read  "a (a+b)^* b" )
             L.example 
       :: Config (RX Char) (L.Formula) )


-- gram 2 (nfa, exp, gram, fo)


make_gram2nfa :: Make
make_gram2nfa = make "gram2nfa"
   ( Config G.example3 NFA.Property.example 
       :: Config G.Grammatik (NFA Char Int) )


make_gram2exp :: Make
make_gram2exp = make "gram2exp"
    ( Config G.example3 
             Exp.Property.example 
       :: Config G.Grammatik (RX Char) )

make_gram2gram :: Make
make_gram2gram = make "gram2gram"
   ( Config G.example3 [ G.Rechtslinear ] 
      :: Config G.Grammatik G.Grammatik )

make_gram2fo :: Make
make_gram2fo = make "gram2fo"
   ( Config G.example3 L.example 
      :: Config G.Grammatik L.Formula )

-- fo 2 (nfa, exp, gram, fo)

make_fo2nfa :: Make
make_fo2nfa = make "fo2nfa"
    ( Config ( read "exists p : exists q : p < q && a(p) && b(q)" )
             NFA.Property.example 
       :: Config (L.Formula) (NFA Char Int) )

make_fo2exp :: Make
make_fo2exp = make "fo2exp"
    ( Config ( read "exists p : exists q : p < q && a(p) && b(q)" )
             Exp.Property.example 
       :: Config (L.Formula) (RX Char) )

make_fo2gram :: Make
make_fo2gram = make "fo2gram"
    ( Config ( read "exists p : exists q : p < q && a(p) && b(q)" )
             [ G.Rechtslinear ]
       :: Config (L.Formula) (G.Grammatik) )

make_fo2fo :: Make
make_fo2fo = make "fo2fo"
   ( Config  ( read "exists p : exists q : p < q && a(p) && b(q)" ) L.example
      :: Config L.Formula L.Formula )



