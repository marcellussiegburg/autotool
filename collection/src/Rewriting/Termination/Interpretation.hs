{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DatatypeContexts #-}
{-# language DatatypeContexts #-}
{-# language TemplateHaskell #-}
{-# language DeriveDataTypeable #-}
{-# language ScopedTypeVariables #-}
{-# language DoAndIfThenElse #-}

module Rewriting.Termination.Interpretation where

import Rewriting.Termination.Semiring
import qualified Rewriting.Termination.Domains as D
import Rewriting.Termination.Multilinear 
import qualified Rewriting.Termination.Polynomial as P
import qualified Polynomial.Type as P
import Rewriting.Termination.Matrix (contents, dim)

import Rewriting.TRS

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import Control.Lens
import Control.Monad ( when, forM )
import Control.Applicative ( (<$>) )
import Data.Typeable
import Data.List ( transpose )

import Autolib.FiniteMap
import qualified Data.Map as M
import qualified Data.Set as S
import Autolib.Size

data Domain = Natural | Arctic | Tropical | Fuzzy 
    deriving (Eq, Typeable)

derives [makeReader, makeToDoc] [''Domain]

type Inter c d = M.Map c d

data (Symbol c, Ord c) => Interpretation c 
    = Matrix_Interpretation_Natural (Inter c (Multilinear D.Natural))
    | Matrix_Interpretation_Arctic (Inter c (Multilinear D.Arctic))
    | Matrix_Interpretation_Tropical (Inter c (Multilinear D.Tropical))
    | Matrix_Interpretation_Fuzzy (Inter c (Multilinear D.Fuzzy))
    | Polynomial_Interpretation (Inter c (P.Poly P.X))
    deriving (Eq, Typeable)


instance Symbol c => Size (Interpretation c) where
    size i = case i of
        Matrix_Interpretation_Natural m -> nonzeroes m
        Matrix_Interpretation_Arctic m -> nonzeroes m
        Matrix_Interpretation_Tropical m -> nonzeroes m
        Matrix_Interpretation_Fuzzy m -> nonzeroes m
        Polynomial_Interpretation m -> sum $ do
            (c,p) <- M.toList m ; return $ P.nterms p
        
nonzeroes m = sum $ do 
    (k,v) <- M.toList m 
    a <- absolute v : coefficients v
    x <- concat $ contents a
    return $ if is_zero x then 0 else 1

derives [makeToDoc] [''Interpretation]

instance (Ord c, Symbol c, Reader c) => Reader (Interpretation c) where
    reader = 
            do my_reserved "Matrix_Interpretation_Natural" 
               Matrix_Interpretation_Natural <$> reader
        <|> do my_reserved "Matrix_Interpretation_Arctic" 
               Matrix_Interpretation_Arctic <$> reader
        <|> do my_reserved "Matrix_Interpretation_Tropical" 
               Matrix_Interpretation_Tropical <$> reader
        <|> do my_reserved "Matrix_Interpretation_Fuzzy" 
               Matrix_Interpretation_Fuzzy <$> reader
        <|> do my_reserved "Polynomial_Interpretation"
               Polynomial_Interpretation <$> reader

data Comparison = Greater | Greater_Equal | Other
    deriving (Eq, Typeable )

derives [makeReader, makeToDoc] [''Comparison]

-- | applied to a term where variables are renamed to [1,2..from]
inter :: (Symbol c, Ord c, Semiring d, ToDoc d)
      => Inter c (Multilinear d) -> Int -> Int 
      -> Term Int c -> Reporter (Multilinear d)
inter int from dim t = explained t $ case t of
    Var to -> return $ projection from to dim
    Node f args -> case M.lookup f int of
        Nothing -> reject $ vcat 
            [ text "missing interpretation for symbol" <+> toDoc f
            ]
        Just fun -> do
            let syn = length args
                sem = length (coefficients fun)
            when ( syn /= sem ) $ reject $ vcat 
                    [ text "arity mismatch for symbol" <+> toDoc f
                    , text "arity of symbol is" <+> toDoc syn
                    , text "arity of interpretation of symbol is" <+> toDoc sem
                    ]
            gs <- forM args $ inter int from dim
            return $ substitute fun gs

-- | applied to a term where variables are renamed to [1,2..from]
inter_poly :: (Symbol c, Ord c)
      => Inter c (P.Poly P.X) 
      -> Term Int c -> Reporter (P.Poly P.X)
inter_poly int t = explained t $ case t of
    Var to -> return $ P.variable $ P.X to
    Node f args -> case M.lookup f int of
        Nothing -> reject $ vcat 
            [ text "missing interpretation for symbol" <+> toDoc f
            ]
        Just fun -> do
            let syn = length args
            void $ sequence $ do 
                (c,m) <- P.terms fun ; (P.X i,e) <- P.factors m 
                return $ when ( i < 1 || i > syn ) $ reject $ vcat 
                    [ text "interpretation of symbol" <+> toDoc f
                    , text "uses non-existing argument" <+> toDoc (P.X i)
                    , toDoc fun
                    ]
            gs <- forM args $ inter_poly int 
            return $ P.substitute fun gs

explained t action = do
    inform $ text "compute interpretation of" <+> toDoc t
    i <- nested 4 action
    inform $ vcat 
        [ text "interpretation of" <+> toDoc t <+> text "is"
        , toDoc i
        ]
    return i

check_monotone i = case i of
    Matrix_Interpretation_Natural i -> must_be_monotone i
    Matrix_Interpretation_Arctic i -> must_be_monotone i
    Matrix_Interpretation_Tropical i -> must_be_monotone i
    Matrix_Interpretation_Fuzzy i -> must_be_monotone i
    Polynomial_Interpretation i -> must_be_monotone_poly i

must_be_monotone (int :: Inter c (Multilinear d)) = forM_ (M.toList int) $ \ (f, m) -> do
    inform $ vcat [ text "check monotonicity for"
                  , text "symbol" <+> toDoc f
                  , text "interpreted by" <+> toDoc m
                  ]
    forM (coefficients m) $ \ c ->
        when (not $ positive c) $ reject $ vcat
             [ text "is not monotone since coefficient" 
             , toDoc c
             , text "is not positive"
             ]
    when (not (strict_addition ( undefined :: d))) $ do
        when (not (null $ coefficients m) && not (is_zero $ absolute m)) $ reject $ vcat
                [ text "interpretation is not monotone"
                , text "since semiring addition is not monotone"
                , text "and absolute part is non-zero"
                , text "and function has at least one argument"
                ]
        when (length ( coefficients m ) > 1) $ reject $ vcat
                [ text "interpretation is not monotone"
                , text "since semiring addition is not monotone"
                , text "and function has more than one argument"
                ] 

must_be_monotone_poly (int :: Inter c (P.Poly P.X)) = forM_ (M.toList int) $ \ (f, p) -> do
    inform $ vcat [ text "check monotonicity for"
                  , text "symbol" <+> toDoc f
                  , text "interpreted by" <+> toDoc p
                  ]
    P.must_be_monotone (arity f) p

compute_order i = case i of
    Matrix_Interpretation_Natural i -> order i
    Matrix_Interpretation_Arctic i -> order i
    Matrix_Interpretation_Tropical i -> order i
    Matrix_Interpretation_Fuzzy i -> order i
    Polynomial_Interpretation i -> order_poly i

order_poly :: (Symbol c, Ord v )
      => Inter c (P.Poly P.X) -> Int 
      -> Rule (Term v c) -> Reporter Comparison
order_poly (int :: Inter c (P.Poly P.X)) dim u = do
    let l = lhs u ; r = rhs u
        vs = S.union (vars l) (vars r)
        m = M.fromList $ zip ( S.toList vs) [1..]
        from = M.size m
        rename = vmap (m M.!) 
    ml <- inter_poly int $ rename l
    mr <- inter_poly int $ rename r
    if P.weakly_greater ml mr
    then if P.strictly_greater ml mr
         then return Greater 
         else return Greater_Equal
    else return Other

order :: (Symbol c, Ord v, Semiring d, ToDoc d )
      => Inter c (Multilinear d) -> Int 
      -> Rule (Term v c) -> Reporter Comparison
order (int :: Inter c (Multilinear d)) dim u = do
    let l = lhs u ; r = rhs u
        vs = S.union (vars l) (vars r)
        m = M.fromList $ zip ( S.toList vs) [1..]
        from = M.size m
        rename = vmap (m M.!) 
    ml <- inter int from dim $ rename l
    mr <- inter int from dim $ rename r
    if and $ zipWith weakly_greater 
        (absolute ml : coefficients ml) 
        (absolute mr : coefficients mr)
    then if ( if strict_addition (undefined :: d) 
              then strictly_greater (absolute ml) (absolute mr)
              else and $ zipWith strictly_greater 
                  (absolute ml : coefficients ml) 
                  (absolute mr : coefficients mr)
            ) 
         then return Greater 
         else return Greater_Equal
    else return Other

check_dimension sig dim i = case i of
    Matrix_Interpretation_Natural i -> check_arity_dimension sig dim i
    Matrix_Interpretation_Arctic i -> check_arity_dimension sig dim i
    Matrix_Interpretation_Tropical i -> check_arity_dimension sig dim i
    Matrix_Interpretation_Fuzzy i -> check_arity_dimension sig dim i
    Polynomial_Interpretation {} -> return ()

check_arity_dimension sig dim i = do
    check_arities sig i
    must_be_dimension dim i

must_be_dimension d m = forM_ (M.toList m) $ \ (k,v) -> do
    let check msg want m = when (want /= dim m) $ reject $ vcat 
            [ text "interpretation of symbol" <+> toDoc k
            , text msg <+> toDoc m
            , text "must have dimension" <+> toDoc want
            ]
    check "absolute part" (d,1) $ absolute v
    forM_ (coefficients v) $ check "coefficient" (d,d)
    
check_arities sig m = forM_ (S.toList sig) $ \ k -> 
    case M.lookup k m of
        Nothing -> reject $ text "symbol" <+> toDoc k <+> text "is missing from interpretation"
        Just v -> do
            let ar = length $ coefficients v
            when (arity k /= ar ) $ reject $ vcat 
               [ text "symbol" <+> toDoc k
               , text "has arity" <+> toDoc (arity k)
               , text "but its interpretation has arity" <+> toDoc ar
               ]

    

