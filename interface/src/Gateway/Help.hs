{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverloadedStrings, CPP #-}

module Gateway.Help (
    Help (..),
    Source (..),
    drift
) where

import Autolib.Output

#if ( __GLASGOW_HASKELL__ >= 710 )
-- must be created with:  autotool-package-translator > ./src/Package_Translator.hs
import Package_Translator
#endif
    
import Data.Typeable
import Data.List ( intersperse, isPrefixOf )
import Data.Char (isDigit)

class ( Typeable a ) => Help a where
      help :: a -> Output
      help x = doc_link $ typeOf x 

instance ( Typeable a ) => Help a 

tuple :: [Output] -> Output
tuple os = besides $ [ Text "(" ] ++ intersperse (Text ", ") os ++ [ Text ")" ]

doc_link :: TypeRep -> Output
doc_link t =
    let ( tyc, tapps ) = splitTyConApp t
	twodee top entries = Above top $ Itemize entries
	onedee top entries = besides
	    $ top : map paren entries
	paren entry = besides [ Text "(", entry, Text ")" ]
        tapps' = map doc_link tapps
    in  case show tyc of
        "[]" -> besides $ [ Text "[" ] ++ tapps' ++ [ Text "]" ]
        "(,)" -> tuple tapps'
        "(,,)" -> tuple tapps'
        "(,,,)" -> tuple tapps'
        _ ->  onedee ( tycon_link tyc ) tapps'

besides = foldr1 Beside

tycon_link tyc = 
   let local = "http://autotool.imn.htwk-leipzig.de/docs/"
       hackage = "http://hackage.haskell.org/package/"
       ty = tyConName tyc
       mod = redot $ undot $ tyConModule tyc
       pack =
#if (__GLASGOW_HASKELL__ >= 710 )
         case M.lookup (tyConPackage tyc) package_translator of
           Nothing -> tyConPackage tyc ; Just s -> s
#else
         unversion $ tyConPackage tyc
#endif
       unversion = reverse
           . dropWhile ( \ c -> isDigit c || c == '.' || c == '-' )
           . reverse
   in  Named_Link ( show tyc ) 
       $ if isPrefixOf "autotool" pack || isPrefixOf "autolib" pack
         then local ++ pack ++ "/" ++ mod ++ ".html#t:" ++ ty
         else hackage ++ pack ++ "/docs/" ++ mod ++ ".html#t:" ++ ty

builtin t = undot $ case t of
    "Int"     -> "GHC.Types"
    "Char"    -> "GHC.Types"
    "Float"   -> "GHC.Types"
    "Double"  -> "GHC.Types"
    "Integer" -> "GHC.Integer"
    "Bool"    -> "GHC.Bool"
    "Maybe"   -> "Data.Maybe"
    "Either"  -> "Data.Either"
    "()"      -> "GHC.Unit"
    _         -> "Unknown"

undot xs = 
    let ( pre, post ) = span (/= '.') xs
    in  pre : if null post then [] else undot $ tail post

redot xs = concat $ intersperse "-" xs -- for haddock-0.7 need dash

----------------------------------------------------------------------

class Source a where
      source :: a -> Maybe FilePath

instance Source a where
      source x = Nothing

drift :: FilePath -> Maybe FilePath
drift f = Just $ f ++ ".drift"

source_link :: Source a 
     => a 
     -> Output
source_link x = case source x of
    Nothing -> Text "no source location found"
    Just s  -> 
        let archive = "http://141.57.11.163/cgi-bin/cvsweb/tool/src/"
	in  Beside ( Text "source location" ) ( Link $ archive ++ s ) 


      
