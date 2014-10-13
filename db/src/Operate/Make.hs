{-# LANGUAGE PatternSignatures #-}

module Operate.Make where

import Inter.Types
import Data.Typeable
import Data.Dynamic
import Data.Maybe



import Autolib.Reader
import Autolib.ToDoc


get_boiler :: [ Make ] 
	   -> [ Dynamic ]
	   -> [ Variant ]
get_boiler makers configs = do
    dyn <- configs
    maker <- makers
    case maker of
        Make p doc ( fun :: conf -> Var p i b ) veri ex -> do
	    it <- maybeToList ( fromDynamic dyn :: Maybe conf )
	    return $ Variant $ fun it

-- | für den tutor: präsentiert liste der bekannten maker-typen
present :: [ Make ] -> Doc
present makers = vcat $ do
    maker <- makers
    case maker of
        Make p doc fun veri ex -> return $ text $ show 
	       $ hsep [ text doc, comma, toDoc $ typeOf fun ]

{-
cpresent :: [ Make ] -> [ Dynamic ] -> Doc
cpresent makers configs = hcat $ do
    dyn <- configs
    maker <- makers
    case maker of
        Make doc ( fun :: conf -> Var p i b ) ex -> do
	    it <- maybeToList ( fromDynamic dyn :: Maybe conf )
	    return $ text $ showXml it
-}


		   
    
    
