{-# language DeriveDataTypeable #-}

module Flow.Common.Data where

-- import Flow.Program
import Flow.Expression
import Flow.Conditions
import Flow.Actions

import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

import Data.Set ( Set )
import qualified Data.Set as S

import Control.Applicative ( (<*>) )

data Statement
    = Atomic Identifier
    | Skip
    | Block [ Statement ]
    | Branch Expression Statement ( Maybe Statement )
    | While Expression Statement
    | Goto Identifier
    | Break (Maybe Identifier)
    | Continue (Maybe Identifier)
    | Label Identifier Statement
    deriving ( Eq, Ord, Typeable )

example :: Statement
example = read "foo : while (f) { o; if (c) continue foo; o; if (c) break ; }"

substatements s = s : case s of
        Block ss -> ss >>= substatements
        Branch _ l Nothing -> substatements l 
        Branch _ l (Just r) -> substatements l ++ substatements r
        While _ s -> substatements s
        Label _ s -> substatements s
        _ -> []




instance Conditions Statement where
    conditions s = case s of
        Block ss -> conditions ss
        Branch t s1 ms2 -> 
            S.unions [ conditions t
                     , conditions s1
                     , case ms2 of 
                         Nothing -> S.empty
                         Just s2 -> conditions s2
                     ]
        While t s -> S.union ( conditions t ) 
                           ( conditions s )
        Label i s -> conditions s
        _ -> S.empty

instance Actions Statement where
    actions s = case s of
        Atomic a -> S.fromList [ a ]
        Block ss -> actions ss
        Branch t s1 ms2 -> 
            S.unions [ actions s1
                     , case ms2 of 
                         Nothing -> S.empty
                         Just s2 -> actions s2
                     ]
        While t s -> actions s 
        Label i s -> actions s
        _ -> S.empty


instance Size Statement where
    size st = case st of
	Block sts -> sum $ map size sts
	Branch _ yes Nothing -> 1 + size yes
	Branch _ yes ( Just no) -> 1 + size yes + size no
	While _ st -> 1 + size st
        Label _ st -> size st
        _ -> 1

instance ToDoc Statement where
    toDoc s = case s of
        Atomic action -> toDoc action <> semi
        Skip -> text "skip" <> semi
	Block  stmts  -> braces $ vcat $ map toDoc stmts
        -- note: no need to make a dangling-else-aware printer here.
        -- it is enough if the parser does not remove blocks.
	Branch c yes mno -> 
            vcat [ text "if" <+> parens ( toDoc c )
	         , nest 4 $ toDoc yes
		 , case mno of
			Nothing -> empty
			Just no -> vcat
		            [ text "else"
			    , nest 4 $ toDoc no
			    ]
		 ]
	While c body -> 
	    vcat [ text "while" <+> parens ( toDoc c )
		 , nest 4 $ toDoc body
		 ]
        Goto i ->  text "goto" <+> toDoc i <> semi
        Break Nothing -> text "break" <> semi
        Break (Just i) -> text "break" <+> toDoc i <> semi
        Continue Nothing -> text "continue" <> semi
        Continue (Just i) -> text "continue" <+> toDoc i <> semi
        Label i st -> toDoc i <+> text ":" <+> toDoc st 


instance Show Statement where show = render . toDoc

instance Reader Statement where
    reader = skip <|> block <|> branch <|> while <|> control 
       <|> try ( do i <- reader ; my_symbol ":" ; s <- reader ; return $ Label i s )
       <|> atomic 

control = do my_reserved "goto" ; t <- reader ; my_semi ; return $ Goto t
    <|> do my_reserved "break" ; t <- optionMaybe reader ; my_semi ; return $ Break t
    <|> do my_reserved "continue" ; t <- optionMaybe reader ; my_semi ; return $ Continue t

block = my_braces $ do 
    xs <- many reader
    return $ Block xs 

skip = do
    my_reserved "skip"
    my_semi
    return Skip

atomic = do
    at <- reader
    my_semi
    return $ Atomic at

branch = do
    my_reserved "if"
    c <- my_parens reader
    yes <- reader
    mno <- ( do my_reserved "else" ; no <- reader ; return $ Just no )
       <|> return Nothing
    return $ Branch c yes mno
    
while = do
    my_reserved "while"
    c <- my_parens reader
    body <- reader
    return $ While c body


