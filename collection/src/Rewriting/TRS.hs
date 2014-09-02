{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# language DatatypeContexts #-}
{-# language FlexibleInstances #-}
{-# language DeriveDataTypeable #-}

module Rewriting.TRS 
( module Rewriting.TRS
, module Autolib.Symbol
, module Autolib.TES.Term
, module Autolib.TES.Position
, module Autolib.TES.Rule
, module Autolib.TES.Identifier
) where

import qualified Rewriting.TRS.Raw as Raw

import Autolib.Symbol
import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.TES.Rule
import Autolib.TES.Identifier
import qualified Autolib.TES

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Multilingual

import Data.Typeable
import qualified Data.Map as M
import qualified Data.Set as S

data ( Symbol c, Symbol v ) => TRS v c = 
     TRS { variables :: [ v ]
         , rules :: [ Rule ( Term v c ) ]
         }
    deriving ( Eq, Ord, Typeable )

instance ( Symbol c, Symbol v ) => ToDoc ( TRS v c ) where
    toDoc trs = toDoc 
              $ Raw.TRS 
              { Raw.variables = variables trs
              , Raw.rules     = rules     trs
              }

instance ( Symbol c ) => Reader ( TRS c c ) where
    reader = do
        trs <- reader
        patch trs

-- | upon reading, the parser does not know what is a variable
-- so the system has to be patched
patch :: ( Symbol c ) 
      => Raw.TRS c c 
      -> Parser ( TRS c c )
patch trs = do
    let handle t @ ( Node f xs ) =
           if f `elem` Raw.variables trs 
              then if null xs 
                   then return $ Var f
                   else fail 
                        $ show
                        $ multitext [(DE, "Variable darf keine Argumente haben:" )
			  	    ,(UK, "variable cannot have arguments")
				    ]
                               <+> toDoc t
              else do
                   ys <- mapM handle xs
                   let ar = length xs
                   return $ Node ( set_arity ar f ) ys
    rs <- sequence $ do
        rule <- Raw.rules trs
        return $ do
            ll <- handle $ lhs rule
            rr <- handle $ rhs rule
            return $ rule { lhs = ll, rhs = rr }
    let wrong = M.fromListWith S.union $ do
            r <- rs ; t <- [lhs r, rhs r]
            s @ ( Node f args ) <- subterms t
            let ar = length args
            guard $ arity f /= ar
            return (f, S.singleton s)
    forM_ (M.toList wrong) $ \ (f, ss) -> 
        forM (S.toList ss) $ \ s -> fail $ unlines
            [ unwords [ "symbol", show f, "of arity", show (arity f) ]
            , "occurs with different arity"
            , unwords [ "at root of subterm", show s ]
            ]
    
    return $ TRS { variables = Raw.variables trs
                 , rules = rs 
                 }   

example :: TRS Identifier Identifier
example = read "TRS { variables = [x, y] , rules = [ f(x,y) -> f(y,x) ] }"


pack :: ( Symbol c, Symbol v ) 
     => TRS v c -> Autolib.TES.RS c ( Term v c )
pack trs = Autolib.TES.from_strict_rules False 
         $ do r <- rules trs ; return ( lhs r, rhs r )
