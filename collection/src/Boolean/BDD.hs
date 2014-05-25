module Boolean.BDD where

import Boolean.Op 
import Expression.Op

import qualified OBDD as O
import Control.Applicative ( (<$>), (<*>) )

import Autolib.TES
import Autolib.Reporter
import Autolib.ToDoc

evaluate :: (Ord v, ToDoc v) => Term v (Op Bool) -> Reporter ( O.OBDD v )
evaluate t = case t of
    Var v -> return $ O.unit v True
    Node f args -> case (name f, args) of
        ( "false", [] ) -> return $ O.constant False
        ( "true", [] ) -> return $ O.constant True
        ( "not", [x] ) -> O.not <$> ( evaluate x )
        ( "&&", [x,y] ) -> O.binary (&&) <$> (evaluate x) <*> (evaluate y)
        ( "||", [x,y] ) -> O.binary (||) <$> (evaluate x) <*> (evaluate y)
        ( "->", [x,y] ) -> O.binary (<=) <$> (evaluate x) <*> (evaluate y)
        ( "<->", [x,y] ) -> O.binary (==) <$> (evaluate x) <*> (evaluate y)
        ( "!=", [x,y] ) -> O.binary (/=) <$> (evaluate x) <*> (evaluate y)
        _ -> reject $ text "unknown operator" <+> toDoc f 
                    <+> text "or wrong number of arguments" <+> parens (toDoc (length args))
                    </> toDoc t

