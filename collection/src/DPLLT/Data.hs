{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language GeneralizedNewtypeDeriving #-}

module DPLLT.Data where

import qualified Fourier_Motzkin.Type as FM

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable
import Control.Applicative ((<$>))

newtype Variable = Variable String deriving (Eq, Ord)

instance ToDoc Variable where toDoc (Variable v) = text v
instance Reader Variable where reader = Variable <$> my_identifier

data Atom = Atom ( FM.Atom Variable )
          | Boolean Variable
    deriving (Eq, Ord, Typeable )

instance ToDoc Atom where
    toDoc a = case a of
        Atom a -> toDoc a ; Boolean b -> toDoc b
instance Reader Atom where
    reader = ( Atom <$> reader ) <|> ( Boolean <$> reader )

data Literal =  Literal { polarity :: Bool, atom :: Atom  }
    deriving (Eq, Ord, Typeable )

positive = polarity
negative = not . polarity
opposite l = l { polarity = not $ polarity l }

instance ToDoc Literal where
    toDoc l = ( case polarity l of True -> empty ; False -> text "!") 
          <+> toDoc (atom l)
instance Reader Literal where 
    reader = do
        p <- option True $ do my_symbol "!" ; return False
        a <- reader
        return $ Literal { polarity = p, atom = a }

instance Show Literal where show = render . toDoc

type Clause = [ Literal ]
type CNF = [ Clause ]

