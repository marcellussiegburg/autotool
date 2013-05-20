{-# language DeriveDataTypeable #-}

module Resolution.Execute where

import Resolution.Data
import Resolution.Action

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Set
import Autolib.Size

import Data.Typeable
import Data.Maybe (maybeToList)

data State = State [ Clause ]
    deriving Typeable

instance Size State where size ( State cs ) = length cs

instance ToDoc State where
    toDoc ( State cs ) = vcat $ do
        (k, c) <- zip [ 0 :: Int .. ] cs
        return $ toDoc k <+> text ":" <+> toDoc c

extend :: State -> Clause -> State
extend ( State cs ) c = State ( cs ++ [c] )

execute :: State -> Action -> Reporter State
execute st act = do
    inform $ text "aktuelle Klauseln" </> toDoc st
    inform $ text "nächster Befehl" </> toDoc act
    l <- pick st $ left act 
    r <- pick st $ right act 

    c <- resolve (literal act) l r 
    inform $ nest 4 $ text "neue Klausel" </> 
           ( toDoc ( size st ) <+> text ":" <+> toDoc c )
    return $ extend st c

resolves :: Clause -> Clause -> [ Clause ]
resolves l r = do
    p <- literals l
    maybeToList $ result $ resolve p l r

resolve :: Literal -> Clause -> Clause 
        -> Reporter Clause
resolve p l r = do
    ll <- remove l p
    rr <- remove r $ turn p
    return $ merge ll rr

merge :: Clause -> Clause -> Clause
merge ( Clause xs ) ( Clause ys ) = Clause ( union xs ys )

remove :: Clause -> Literal -> Reporter Clause
remove c @ ( Clause s ) l = do
    inform $ text "entferne Literal" <+> toDoc l <+> text "aus Klausel" <+> toDoc c
    if elementOf l s
       then do
           let d = Clause $ minusSet s $ unitSet l
           inform $ text "ergibt" <+> toDoc d
           return d
       else reject $ text "Fehler: kommt nicht vor"
    
pick :: State -> Int -> Reporter Clause
pick ( State cs ) i = do
    when (i < 0) $ reject $ text "Index" <+> toDoc i <+> text "ist zu klein"
    when (i >= length cs) $ reject $ text "Index" <+> toDoc i <+> text "ist zu klein"
    return $ cs !! i
