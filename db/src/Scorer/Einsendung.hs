module Scorer.Einsendung 

( Einsendung (..), size, Scorer.Einsendung.okay
, Obfuscated (..)
, SE (..)
, slurp, slurp_deco -- datei-inhalt verarbeiten
)

where

--   $Id$

{- so sehen die dinger aus: (3: VNR, 11: ANr)

Fri Nov 28 18:33:49 CET 2003 ( 2425 ) cgi-318 ( 318 ) 3-11 : OK # Size: 7 

-- oder auch (siehe http://nfa.imn.htwk-leipzig.de/bugzilla/show_bug.cgi?id=365)

Fri Nov 14 13:43:49 CET 2014 ( 19549 ) cgi- (  ) 212-2199 : NO 
Fri Nov 14 13:44:10 CET 2014 ( 19557 ) cgi- (  ) 212-2199 : OK # Size: 3 

-}

import Scorer.Util hiding ( size )

import Autolib.FiniteMap
import Control.Monad ( guard )
import Data.Maybe ( isJust )

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Applicative ((<$>))
import Data.List (intersperse)

-- | das ist die information zu jeweils einer studentischen einsendung
data Einsendung = Einsendung
          { msize     :: ! (Maybe Int)
	  , date     :: ! [Int]
	  , time     :: ! String -- ^ original time entry
	  , matrikel :: ! (Obfuscated MNr) -- ^ Datenschutz
	  , auf	     :: ! ANr
	  , vor      :: ! VNr
	  , pid	     :: ! String
          , visible  :: ! Bool -- tutor submissions should be invisible
	  }	deriving (Eq,Ord)

size e = case msize e of
    Nothing -> error "size"
    Just s  -> s

okay :: Einsendung -> Bool
okay = isJust . msize

data Obfuscated a = Obfuscated 
        { internal :: ! a
        , external :: ! String
        } deriving ( Eq, Ord, Show )

nobfuscate :: MNr -> Obfuscated MNr
nobfuscate mnr = Obfuscated { internal = mnr, external = toString mnr }

obfuscate :: MNr -> Obfuscated MNr
obfuscate mnr = Obfuscated 
              { internal = mnr
              , external = do
                    let cs = toString mnr
                    ( k, c, s ) <- zip3 ( reverse $ take ( length cs ) [ 0 .. ] )
                                        cs $ repeat '*'
                    return $ if 0 == k `mod` 3 then s else c
              }

instance ToString ( Obfuscated a ) where
    toString = external

data SE = SE !SNr !Einsendung

instance Show SE where 
    show ( SE s i ) = unwords 
        [ spaci 10 $ show $ abs $ size i
	, spaci 12 $ toString s
	,    (nulli 2 $ date i !! 2) ++ "."
		  ++ (nulli 2 $ (date i) !! 1) ++ "."
		  ++ (nulli 4 $ (date i) !! 0) 
	,    (nulli 2 $ (date i) !! 3) ++ ":"
		  ++ (nulli 2 $ (date i) !! 4) ++ ":"
		  ++ (nulli 2 $ (date i) !! 5)
		]
    

instance Show Einsendung where
    show i = unwords 
        [ spaci 10 $ show $ abs $ size i
	, spaci 12 $ toString $ matrikel i
	,    (nulli 2 $ date i !! 2) ++ "."
		  ++ (nulli 2 $ (date i) !! 1) ++ "."
		  ++ (nulli 4 $ (date i) !! 0) 
	,    (nulli 2 $ (date i) !! 3) ++ ":"
		  ++ (nulli 2 $ (date i) !! 4) ++ ":"
		  ++ (nulli 2 $ (date i) !! 5)
		]

spaci :: Int -> String -> String
spaci n = stretch n 

nulli :: Show a => Int -> a -> String
nulli n = stretchWith '0' n . show

-- | alle lesbaren Zeilen
slurp = slurp_deco True

slurp_deco :: Bool -> String -> [ Einsendung ]
slurp_deco deco cs = do
    z <- lines cs
    case parse (entry deco) "<>" z of
        Right e -> return e
        Left err -> fail "no parse"

{-
Fri Nov 28 18:33:49 CET 2003 ( 2425 ) cgi-318 ( 318 ) 3-11 : OK # Size: 7 
Fri Nov 14 13:43:49 CET 2014 ( 19549 ) cgi- (  ) 212-2199 : NO 
Fri Nov 14 13:44:10 CET 2014 ( 19557 ) cgi- (  ) 212-2199 : OK # Size: 3 
-}

entry :: Bool -> Parser Einsendung
entry deco = do
    let l = P.makeTokenParser emptyDef
    weekday <- P.identifier l
    month <- P.identifier l
    date <- nat l
    h <- nat l ; P.reservedOp l ":"
    m <- nat l ; P.reservedOp l ":"
    s <- nat l 
    tz <- P.identifier l
    year <- nat l
    p <- P.parens l $ nat l    
    string "cgi-" ; matrikelnr
    mnr <- P.parens l $ matrikelnr
    v <- nat l ; string "-" 
    a <- nat l ; P.reservedOp l ":"
    res <- do P.reserved l "NO" ; return Nothing
       <|> do P.reserved l "OK" ; P.reservedOp l "#"
              P.reserved l "Size" ; P.reservedOp l ":"
              Just <$> nat l
    return $ Einsendung
	      {	time = concat
                     $ intersperse ":" 
                     $ map show [ h,m,s]
              , date = [ year, monthNum month, date
                       , h, m, s ]
	      , msize     = res 
	      , matrikel = ( if deco then obfuscate else nobfuscate ) mnr
	      , auf	 = fromCGI $ show a
	      , vor      = fromCGI $ show v
	      , pid      = show p
	      , visible  = False
              }

nat l = fromInteger <$> P.natural l

matrikelnr = do
    s <- option "0" $ many1 (digit <|> char ',')
    spaces
    return $ fromCGI s

-- instance Read Einsendung where 
--    readsPrec p cs = do
read_deco deco cs = do
        let ( line, rest ) = span (/= '\n') cs

	let
            field n	 = head . drop (n-1)
            mySub x | x == ':'  = ' '
            		  | otherwise = x
            wl      = words line
            line'   = dropWhile (/=")") wl
            date'   = takeWhile (/="(") wl
            aufg    = field 6 line'
            ( v , '-' : a ) = span (/= '-') aufg
	    ok	    = field 8 line'

	-- guard $ ok == "OK"

	let e = Einsendung
	      {	time = unwords $ take 6 wl
              , date = [ read     $ field 6 date'            -- Jahr
                       , monthNum $ field 2 date'            -- Monat
                       , read     $ field 3 date'            -- Tag
                       ]
                       ++                                    -- St:Mi:Se
                       [ read x | x <- words $ map mySub (field 4 date') ]
	      , msize     = if ok == "OK"
                            then Just $ read $ field 11 line'
                            else Nothing
	      , matrikel = ( if deco then obfuscate else nobfuscate ) 
                         $ fromCGI $ field  4 line'
	      , auf	 = fromCGI a
	      , vor      = fromCGI v
	      , pid      = field 8 wl			-- process id
	      , visible  = False
              }
	return ( e, rest )

-------------------------------------------------------------------------------
-- | komisch, aber ich habs nirgendwo gefunden
monthFM :: FiniteMap String Int 
monthFM    = listToFM [ ("Jan", 1),("Feb", 2),("Mar", 3),("Apr", 4)
		      , ("May", 5),("Jun", 6),("Jul", 7),("Aug", 8)
		      , ("Sep", 9),("Oct",10),("Nov",11),("Dec",12)
                      ]

-- | Umwandlung Monat-Kürzel -> Zahl, bei Fehler kommt 13 zurück
monthNum m = lookupWithDefaultFM monthFM 13 m
