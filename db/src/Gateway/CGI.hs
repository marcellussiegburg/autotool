{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances -fallow-undecidable-instances #-}

{-# language OverloadedStrings #-}

-- | abstraction to simple CGI input elements
-- the state is in the CGI parameters (the Env)

module Gateway.CGI 

( Form, Tag, execute, look, write, delete
, get_env, get_vars, look_var, look_env
, runForm
, io, embed, embed_for, wrap
, select_preferred_language
, get_preferred_language
, set_preferred_language
, gensym
, begin, click, end, end0, end_with_default, end_maybe
, cc
, click_choice
, click_choice_with_default
, embedded_click_choice_with_default
, click_choice0
, h1, h2, h3
, pre, plain, html, br, hr, par
, open, close, row, glued, table, btable, sorted_btable
, open_btable_with_sorter
, beside, above
, require, with, blank
, submit, radio, hidden, hidden0, farbe
, file
, select, textfield, password, textarea, checkbox, radiogroup
, selector, selector', editor
, selector_submit, selector_submit', selector_submit_click
, selectors
, submit_choice, tree_choice
, editor_submit, defaulted_textarea
, defaulted_textfield, defaulted_password
, defaulted_selector
)

where

import Gateway.Help

import qualified Local
import Gateway.Errmsg

import Autolib.ToDoc hiding ( beside, group )
import qualified Autolib.Output
import Autolib.Reader
import Autolib.FiniteMap
import Autolib.Timer
import Autolib.Util.Sort
import Autolib.Util.Zufall

import qualified Autolib.Reporter.IO.Type as R

-- import qualified Gateway.My_CGI as N 
import qualified Gateway.My_CGI2 as N 
import qualified Network.CGI

import Autolib.Multilingual ( Language (..), specialize )

-- import qualified Text.XHtml
-- import Text.XHtml (HTML (..), Html, (!), (+++), (<<), primHtml, renderHtml)
import qualified Gateway.Html as Html
import Gateway.Html (Html, toHtml, primHtml, HTML, (!), (<<), (+++) )

import qualified Autolib.Output.Html
import qualified Autolib.Multilingual.Html

import Prelude hiding ( catch )
import Control.Exception as CE
import Debug ( debug )
import Control.Monad 
import Control.Monad.Fix

import Database.HSQL.MySQL ( sqlExceptions )

import Data.Maybe
import Data.Tree
import System.Random
import qualified Data.Set as S
import Data.Dynamic
import Data.Typeable
import Data.List ( nub, isPrefixOf, intersperse )
import Data.Char (isDigit, toLower, toUpper)
import Data.String ( fromString )

instance ToDoc Html where toDoc x = text "Html" -- text . show

type Env = [(String, String)]
type Tag = String

-- | list of html elements
data Stack a = Stack { final :: [ a ] -> a
		   , contents :: [ a ]
		   }

make :: ( [a] -> a ) -> Stack a
make f = Stack { final = f, contents = [] }

push :: ETML a => a -> Stack Etml -> Stack Etml
push x s = s { contents = Etml x : contents s }

pop :: Stack a -> a
pop s = final s $ reverse $ contents s

drops :: Stack a -> Stack a
drops s = s { contents = drop 1 $ contents s }

-- | existential HTML
class ( Typeable a, ToDoc a, HTML a, Parts a ) => ETML a 
instance ( Typeable a, ToDoc a, HTML a, Parts a ) => ETML a 

data Etml = forall a . ETML a => Etml a 
    deriving Typeable
instance HTML Etml where
    toHtml (Etml x) = toHtml x
instance Show Etml where
    show ( Etml x ) = render $ toDoc x
instance ToDoc Etml where 
    toDoc ( Etml x ) 
        = toDoc x
        -- = text $ show x
        -- = error $ unlines [ "toDoc Etml" , show ( typeOf x ) , show x ]

-- | stack of list of html elements.
-- new elements are added in front of the top list
data State = 
     State { stack :: [ Stack Etml ]
           , vars :: Env
	   , env :: Env
	   , forms :: [ Int ] -- ^ (random) sequence of uniqe ids
           , group :: [ ( Tag, Dynamic ) ] -- ^ candidates for next mutex
	   , mutex :: Maybe Tag -- ^ tag of hidden element that keeps previous choice within current group 
           , preferred_language :: Language
	   }
     deriving Typeable

state0 :: Env -> Env -> IO State
state0 vs e = do
    return $ State { stack = [ ]
          , vars = vs                   
	  , env = cleanup e
	  , forms = [ 1 .. ] 
	  , group = [ ]
	  , mutex = Nothing
          , preferred_language = DE
	  }

-- | input components are numbered.
-- there are several, but only one is of type submit (others are hidden).
-- this function finds the submit component and clears everything 
-- that comes later.
-- This is necessary to prevent erroneous behaviour.
-- The implementation is a hack
-- that assumes that the submit component's name starts "C" (click)
-- and that components are in order.
--
-- addition:
-- it clears only those entries that start with "F"

cleanup :: Env -> Env
cleanup env = 
    let ( post, pre ) = span ( not . clicked ) $ reverse env
    in  reverse pre ++ filter ( not . chosen ) ( reverse post )

clicked ( name, val ) = isPrefixOf "C" name
chosen ( name, val ) = isPrefixOf "F" name

data Form m a = Form ( State -> m ( State , Maybe a ) )
--    deriving Typeable

instance ( Monad m ) => Functor (Form m) where
    fmap g (Form f) = Form $ \ s -> do
	 ( s', x ) <- f s
	 return ( s', fmap g x )


instance ( Monad m ) => Monad ( Form m ) where
    return x = Form $ \ s -> do
	 return ( s , return x )
    Form f >>= g = Form $ \ s0 -> do
         ( s1, mx ) <- f s0
         case mx of
	      Nothing -> return ( s1, Nothing )
              Just x -> do
                  let Form f' = g x
	          ( s2, x2 ) <- f' s1
                  return ( s2 , x2 )
    fail msg = mzero

{-
instance MonadFix m => MonadFix (Form m) where
    mfix comp = Form $ \ s0 -> mdo
        ( s, mx ) <- case mx of
            Just x -> let Form f = comp x in f s0
	    Nothing -> return ( s0, Nothing )
	return ( s, mx )
-}

-- | always return
-- wrap :: Monad m => Form m a -> Form m (Maybe a)
wrap ( Form f ) = Form $ \ s0 -> do
    ( s1, mx ) <- ( f s0 ) `CE.catch` \ ( e :: CE.SomeException ) -> do
        debug $ "caught: " ++ show e
        return ( s0, Nothing )
    let top = unwind $ stack s1
        st = push top $ make glue
    return ( s1 { stack = [ st ] } , Just mx )

instance ( Monad m ) => MonadPlus ( Form m )  where
    mzero = Form $ \ s -> return ( s , mzero )
    mplus = error "instance MonadPlus (Form m) has no mplus"

runForm :: Form IO a -> IO (Maybe a)
runForm (Form f) = do
    s0 <- state0 [] []
    (s1 , ma) <- f s0
    return ma

execute :: String -> Form IO a -> IO ()
-- close is not here, it is done by unwind in execute0
execute cgi f = execute0 cgi $ do { open glue ; f } 

execute0 :: String -> Form IO a -> IO ()
execute0 cgi0 (Form f) = N.wrapper $ \ vs is -> do

    let query = concat $ intersperse "&" $ do
                 kv @ (k,v) <- is
                 guard $ not $ clicked kv
                 guard $ not $ chosen kv
                 return $  Network.CGI.urlEncode k 
                        ++ "=" 
                        ++ Network.CGI.urlEncode v
        cgi = cgi0 ++ ( if null query then "" else '?' : query )
                   ++ "#hotspot"
    timed 15 (return $ clock cgi) ( do 

        s0 <- state0 vs is
        let info = Html.pre 
                 << ( render $ toDoc $ env s0 ) -- environment
        let handler msg = do
                  let Form f = do
			 plain $ "got exception" ++ msg
                         mzero
                  f s0
{-
	    sql_handler action = 
		  Control.Exception.catchJust sqlExceptions action
	               ( \ err -> handler $ " (sql) " ++ show  err ) 
            dyn_handler action = 
		  Control.Exception.catchJust dynExceptions action
	               ( \ err -> handler $ " (dynamic) " ++ show  err ) 
-}
            stat_handler action = 
		  CE.catch action
	               ( \ (err :: CE.SomeException) -> handler $ "of value " ++ show err )
        (s', _) <- stat_handler 
--	         $ dyn_handler 
--	         $ sql_handler
		 $ f s0
	-- need to unwind because computation might have been interrupted
	let h = toHtml $ unwind $ stack s'
        return $ page cgi h -- $ if Local.debug then info +++ h else h
           -- ( Html.font h ! [ Html.face "Helvetica" ] )
      )
    `CE.catch` \ (err :: CE.SomeException) -> 
                 return $ Html.pre << primHtml ( show err )

clock :: String -> Html
clock cgi = page cgi ( Html.pre << primHtml timer_expired_message )
                                              -- "Timer expired" )

timer_expired_message :: String 
timer_expired_message = 
    unlines 
     [ "Zeitüberschreitung!"  
     , "Entweder das System ist überlastet oder Ihre Einsendung hat zu sehr umfangreichen Berechnungen geführt."
     , "Alle Aufgaben werden so gestellt, dass die Bearbeitung innerhalb kurzer Zeit möglich ist."
     , "Sie sollten also entweder Ihre Einsendung nochmals überarbeiten oder zu einem späteren Zeitpunkt erneut einsenden."
     , "Vielen Dank für Ihr Verständnis."
     ]

unwind :: [ Stack Etml ] -> Etml
unwind ( st : sts ) = 
    let p = pop st
    in  case sts of 
	   []         -> p
	   st' : rest -> unwind ( push p st' : rest)

sty :: Html
sty = ( Html.style 
         << ( "pre { margin-top: 0em; margin-bottom: 0em; }" 
         +++ "blockquote { margin-top: 0em; margin-bottom: 0em; }" 
          :: Html
         )
      ) ! [ Html.thetype "text/css" ]

page :: String -> Html -> Html
page cgi h = ( Html.html << ) $
     Html.header << ( Html.meta ! [Html.charset "UTF-8"] +++ sty +++ Html.thetitle << cgi )
      +++ Html.body ( Html.form h 
			 ! [ Html.action $ fromString cgi 
			   , Html.method "POST" 
			   , Html.strAttr "enctype" "multipart/form-data"
			   ]
		         )

------------------------------------------------------------

-- | run an (IO) action
io ::  Monad m
       => m a
       -> Form m a
io action = Form $ \ s -> do
    x <- action
    return ( s , return x )

-- | run computation (and IO side effects)
-- and embed result as Html
embed :: R.Reporter a 
      -> Form IO ( Maybe a )
embed r = do
    lang <- get_preferred_language 
    embed_for lang r

embed_for lang r = do
    ( ma, out ) <- io $ R.run r
    let h :: Autolib.Multilingual.Html.Html
        h = Autolib.Output.render out 
    html $ specialize lang h
    return ma

-- | new html stack level
open :: Monad m => ( [ Etml ] -> Etml ) ->  Form m ()
open fun = Form $ \ s -> 
    return ( s { stack = make fun : stack s } , return () )

-- | push onto topmost stack
html :: ( Monad m, ETML a )
     => a
     -> Form m ()
html h = Form $ \ s -> do
    let (xs : xss) = case null (stack s) of True -> [ make glue ]
					    _ -> stack s
    return  ( s { stack = push h xs : xss } , return () )

-- | apply function to topmost stack,
-- push result onto next stack (which becomes topmost)
close :: Monad m => Form m ()
close = Form $ \ s -> do
    let xs : (ys : yss) = stack s
    return ( s { stack = push ( pop xs ) ys : yss } , return () )

-- | get and remove topmost item from topmost stack (must be nonempty)
fetch :: Monad m => Form m Etml
fetch = Form $ \ s -> do
    let xs : yss = stack s
    let x = head $ contents xs
    return ( s { stack = drops xs : yss } , return x )

-- | return unique identifier string (for next input element tag)
-- and add to current mutex group
gensym :: Monad m => Form m String
gensym = gensympref "F"

gensympref f = Form $ \ s -> do
    let x : xs = forms s
    let sym = f ++ show x
    return ( s { forms = xs } 
	   , return sym
	   )

--------------------------------------------------------------------

select_preferred_language :: Monad m => Form m ()
select_preferred_language = do
    lang <- click_choice_with_default 0 
        "preferred language" [ ("english", UK), ("deutsch", DE) ]
    set_preferred_language lang

set_preferred_language :: Monad m => Language -> Form m ()
set_preferred_language l = Form $ \ s ->
    return (s { preferred_language = l }, return () )

get_preferred_language :: Monad m => Form m Language
get_preferred_language = Form $ \  s -> 
    return (s, return $ preferred_language s )

-- | clear mutex group
clear_mutex :: Monad m => Form m ()
clear_mutex = Form $ \ s -> do
    return ( s { group = [] } , return () )

-- | add to mutex group
add_to_mutex :: Monad m 
	     => (Tag, Dynamic)
             -> Form m ()
add_to_mutex (tag, d) = Form $ \ s -> do
    return ( s { group = (tag, d) : group s } 
	   , return () 
	   )

-- | clear mutex group, keeping top symbol as group header
get_mutex_group :: Monad m => Form m  [ (Tag, Dynamic) ]
get_mutex_group = Form $ \ s -> do
    return ( s 
	   , return $ group s 
	   )

-- | start new mutex group
-- that is, throw away previous group
-- in the group, only use click
-- use end to ask for the result
begin :: Monad m => Form m ()
begin = do
    clear_mutex

----------------------------------------------------------------------

click :: ( Typeable a, Monad m )
      => ( String, a )
      -> Form m ()
click ( name, a ) = do
    tag <- gensympref "C"
    submit0 tag name -- do not evaluate
    add_to_mutex ( tag, toDyn a )

cc ::  ( Typeable a, Monad m )
	     => Form m ( String, a )
	     -> String 
             -> [ (String, a) ]
	     -> Form m a
cc = cc_internal True

embedded_cc ending = cc_internal False ending undefined

cc_internal rows ending name opts =  do
    when rows $ do open row ; plain name ; open glue
    begin -- mutex
    mapM_ click $ do (elt, a) <- opts ; return ( elt, (elt, a) )
    ( tag, a ) <- ending -- mutex    
    when rows $ do close ; plain tag ; close -- row
    return a

-- | keine default, d. h. ohne Auswahl ist hier Schluß
click_choice_orig :: ( Typeable a, Monad m )
	     => String 
             -> [ (String, a) ]
	     -> Form m a
click_choice_orig  = cc end 

-- | ohne Typeable restriction
click_choice :: ( Monad m )
                   => String
                   -> [ (String, a ) ]
                   -> Form m a
click_choice name opts = do
    i <- click_choice_orig name $ do 
             (( n, o ), i) <- zip opts [0 ..] ; return ( n, i )
    return $ snd $ opts !! i 

-- | default wird sofort gewählt
click_choice_with_default_orig :: ( Typeable a, Monad m )
	     => Int
             -> String 
             -> [ (String, a) ]
	     -> Form m a
click_choice_with_default_orig i name opts = 
    cc (end_with_default ( opts !! i)) name opts

embedded_click_choice_with_default_orig i opts = 
    embedded_cc (end_with_default ( opts !! i)) opts

click_choice_with_default :: ( Monad m )
	     => Int
             -> String 
             -> [ (String, a) ]
	     -> Form m a
click_choice_with_default def name opts = do
    i <- click_choice_with_default_orig def name $ do
           (( n, o ), i) <- zip opts [0 ..] ; return ( n, i )
    return $ snd $ opts !! i

embedded_click_choice_with_default :: ( Monad m )
	     => Int
             -> [ (String, a) ]
	     -> Form m a
embedded_click_choice_with_default def opts = do
    i <- embedded_click_choice_with_default_orig def $ do
           (( n, o ), i) <- zip opts [0 ..] ; return ( n, i )
    return $ snd $ opts !! i


click_choice0 :: ( Typeable a, Monad m )
	     => String 
             -> [ (String, a) ]
	     -> Form m a
click_choice0 = cc end0

----------------------------------------------------------------------

-- | end without reading cache
end0 :: ( Typeable a, Monad m ) => Form m a
end0 = do
    elts <- get_mutex_group
    mea <- ender elts
    Just ( elt, a ) <- return mea
    fromdyn a

ender elts = do
    -- collect actual clicks
    clicked <- sequence $ do 
        ( elt, a ) <- elts
	return $ do 
	    f <- look elt
	    return [ ( elt, a ) | isJust f ]
    return $ listToMaybe $ concat clicked
    
-- | end mutex group, stop if no input and not default
end :: ( Typeable a, Monad m ) => Form m a
end = do
    Just x <- end_maybe
    return x

end_with_default :: ( Typeable a, Monad m ) => a -> Form m a
end_with_default d = do
    mx <- end_maybe
    case mx of
	 Just x  -> return x
	 Nothing -> return d

-- | end mutex group, return Nothing if no input and no default
end_maybe :: ( Typeable a, Monad m ) 
	  => Form m (Maybe a)
end_maybe = do
    elts <- get_mutex_group
    clicked <- ender elts
    tag <- gensym
    case clicked of
        Just ( elt, a ) -> do
	    -- new click
	    blank -- clear future
            hidden0 tag elt -- note for next round
	    x <- fromdyn a
	    return $ Just x
        Nothing -> do
	    -- no new click
            mprev <- look tag
	    case mprev of
	        Just prev -> do
		    -- cache entry exists, continue
		    hidden0 tag prev
		    -- and lookup value
                    Just a <- return $ lookup prev elts
		    x <- fromdyn a
		    return $ Just x
		Nothing -> do
                    -- cache entry does not exist
		    return $ Nothing

----------------------------------------------------------------------

tree_choice ::  ( Typeable a, Monad m )
            => Maybe String -- ^ previously selected
            -> Tree ( Either String ( String, a ) )
            -> Form m a
tree_choice mdef t = do
    let ps = paths $ fmap stringify t
        mpath = do
            def <- mdef
            lookup def ps
        path = case mpath of Just p -> p ; Nothing -> []
    path_tree_choice path t

stringify :: Either String ( String, a ) -> String
stringify n = case n of
            Left name -> name
            Right ( name, x ) -> name

paths :: Tree a -> [ (a, [Int]) ]
paths t = ( rootLabel t, [] ) : do 
    ( k, c ) <- zip [ 0 .. ] $ subForest t
    ( x, p ) <- paths c
    return ( x, k : p )

path_tree_choice :: ( Typeable a, Monad m )
            => [ Int ] -- ^ describes default path
            -> Tree ( Either String ( String, a ) )
            -> Form m a
path_tree_choice path top = do
    let tag = stringify $ rootLabel top
        cs = do
            t <- subForest top
            return $ case rootLabel t of
                Left name -> ( name, Left t )
                Right ( name, x ) -> ( name, Right x )
    c <- case path of
             [] -> click_choice tag cs
             p : ps -> click_choice_with_default p tag cs 
    case c of
        Left sub -> path_tree_choice ( drop 1 path ) sub
        Right x    -> return x

----------------------------------------------------------------------

fromdyn :: forall m a
         . ( Typeable a, Monad m )
        => Dynamic -> Form m a
fromdyn a = f where
    ( f :: Form m a ) = case fromDynamic a of
        Just x  -> return x
	Nothing -> do
            plain $  "error in fromDynamic for type " 
		  ++ show (typeOf (undefined :: a))

	    mzero

	    
--------------------------------------------------------------------------

glue :: ETML a => [ a ] -> Etml
glue = Etml . Html.concatHtml . map toHtml

h1 :: Monad m => String -> Form m ()
h1 w = html $ Html.h1 << w

h2 :: Monad m => String -> Form m ()
h2 w = html $ Html.h2 << w

h3 :: Monad m => String -> Form m ()
h3 w = html $ Html.h3 << w

pre :: Monad m => String -> Form m ()
pre w = html $ Html.p << Html.pre << w

plain :: Monad m => String -> Form m ()
plain = html . primHtml

br :: Monad m => Form m ()
br = html Html.br

hr :: Monad m => Form m ()
hr = html Html.hr

par :: Monad m => Form m ()
par = html $ Html.p << ( "" :: Html )


------------------------------------------------------------

-- | collect elements of a row
-- so they can be accessed when making the table
data Row a = Row [ a ]
    deriving ( Typeable )

instance ETML a => ToDoc ( Row a ) where
    toDoc ( Row xs ) 
        -- = error $ unlines [ "toDoc Row" , show ( typeOf xs ) ]  
	= text "Row .."

instance ETML a => HTML ( Row a ) where
    toHtml r @ ( Row xs )
      -- = error $ unlines [ "toHtml" , show ( typeOf r )  ]
      = toHtml $ glue $ map ( Html.td << ) $ map toHtml xs

class Parts a  where 
    parts :: a -> [ Etml ]

instance ETML a => Parts a where
    parts x =
       error $ unlines [ "Parts" , show ( typeOf x )  ]

instance  ( Parts a, ETML a ) => Parts ( Row a ) where 
    parts ( Row xs ) = map Etml xs
instance Parts Etml where 
    parts ( Etml x ) = parts x

data Glued a = Glued [a] deriving ( Typeable )
instance ETML a => ToDoc ( Glued a ) where
    toDoc ( Glued xs ) = toDoc ( "Glued .." :: String )
instance ETML a => HTML ( Glued a ) where
    toHtml g @ ( Glued xs ) 
      =  toHtml $ glue $ map toHtml xs

glued :: ( ToDoc a, ETML a ) => [ a ] -> Etml
glued xs = Etml $ Glued xs

-- | close for table row: put TD around each element
row :: ( ToDoc a, ETML a ) => [ a ] -> Etml
row hs = Etml $ Row hs

-- | close for table: put TR around each element, and TABLE around all
table :: ETML a => [ a ] -> Etml
table rows =  Etml $ Html.table 
	     $ toHtml 
	     << glue ( map ( Html.tr << ) rows )

-- | close for table: put TR around each element, and TABLE around all
btable :: ETML a => [ a ] -> Etml
btable rows = Etml 
            $ Html.table ! [ Html.border "1" ] 
	    << ( glue ( map ( Html.tr << ) rows ) )

-- | close for sorted table: put TR around each element, and TABLE around all
-- TODO: sortierbar nach beliebiger spalte
sorted_btable :: ( ) --  Show a, HTML a ) 
	      => Etml -- heading
	      -> Int -- sort by this column
	      -> [ Etml ] -- rows (get elems by @parts@)
	      -> Etml
sorted_btable heading i rows  = 
    let entries = do
	    row <- rows
	    return $ do
	        it <- parts row
		return $ toHtml it
    in  Etml
             $ Html.table ! [ Html.border "1" ] 
	    << glue ( map ( Html.tr << ) 
		    $ toHtml heading 
		    : map ( toHtml . Row ) 
			  ( sortBy ( tonum . Html.renderHtml . (!! i) ) entries )
		    )

-- | sorting hack:
-- if it is a number, compare as such (i. e. length-lex)
-- else it is a string
tonum :: String -> Either String Integer
tonum cs = 
    let w = map toLower $ unwords $ words cs
    in case reads w of
          [ (i, "") ] -> Right i
	  _ -> Left w

-- | needs @close@ later on
open_btable_with_sorter headings = do
    open row
    sorter <- embedded_click_choice_with_default 0 
	   $ zip headings [ 0.. ]
    close -- row
    heading <- fetch
    open $ sorted_btable heading sorter
    return ()


------------------------------------------------------------

-- | use to transform  Just Nothing (no input) to Nothing (failure)
require :: Monad m => Form m (Maybe a) -> Form m a
require (Form f) = Form $ \ s -> do
    (s', x) <- f s
    return (s', join x)

-- | supply default value and continue
with :: Monad m => a -> Form m (Maybe a) -> Form m a
with def f = do x <- f ; return $ fromMaybe def x

-- | blank the environment
blank :: Monad m => Form m ()
blank = Form $ \ s -> return $
     ( s { env = [] }, return () )

----------------------------------------------------------------------

look_var :: Monad m => String -> Form m ( Maybe String )
look_var w = Form $ \ s -> do
    let v = "HTTP_" ++ map toUpper w
    return ( s , return $ lookup v $ vars s )

get_vars :: Monad m => Form m  [(String,String)] 
get_vars = Form $ \  s -> return ( s, return $ vars s )

-- | for compatibility
look :: Monad m => String -> Form m ( Maybe String )
look = look_env

look_env :: Monad m => String -> Form m ( Maybe String )
look_env w = Form $ \ s -> return ( s , return $ lookup w $ env s )

get_env :: Monad m => Form m  [(String,String)] 
get_env = Form $ \  s -> return ( s, return $ env s )

write :: Monad m => String -> String -> Form m ()
write name value = Form $ \ s -> 
    return ( s { env = (name, value) : env s }
	   , return ()
	   )

delete :: Monad m => String -> Form m ()
delete name =  Form $ \ s -> 
    return ( s { env = filter ( \ it -> name /= fst it ) $ env s }
	   , return ()
	   )

type Bool_Widget m = String -> Form m Bool
type Bool_Widget0 m = Tag -> String -> Form m Bool

-- | basic elements
widget0 :: Monad m 
       => ( Tag -> String -> Html )
       -> Bool_Widget0 m
widget0 w tag lab = do
    prev <- look tag
    html $ w tag lab
    return $ isJust prev

widget :: Monad m 
       => ( Tag -> String -> Html )
       -> Bool_Widget m
widget w lab = do
    tag <- gensym
    widget0 w tag lab

widgetpref :: Monad m 
       => String
       -> ( Tag -> String -> Html )
       -> Bool_Widget m
widgetpref f  w lab = do
    tag <- gensympref f
    widget0 w tag lab

submit :: Monad m => Bool_Widget m
submit   = widgetpref "C" Html.submit

submit0 :: Monad m => Tag -> Bool_Widget m
submit0   = widget0 Html.submit

radio :: Monad m => Bool_Widget m
radio    = widget $ Html.radio     


checkbox :: Monad m => Bool -> Bool_Widget m
checkbox def = widget $ \ tag lab -> 
    Html.checkbox tag lab 
        ! [ Html.strAttr "checked" "checked" | def ]

-- | text input elements
-- come in two variants:
-- with epxlicit tagging (dangerous)
-- and with automatic tagging (recommended, names are unique)
type Textual m = String -> Form m ( Maybe String )
type Textual0 m = Tag -> String -> Form m ( Maybe String )

textual0 :: Monad m
       => ( Tag -> String -> Html )
       -> Textual0 m
textual0 element tag def = do
    prev <- look tag
    let cont = fromMaybe def prev
    html $ element tag cont
    return prev

textual :: Monad m
       => ( Tag -> String -> Html )
       -> Textual m
textual element def = do
    tag <- gensym
    textual0 element tag def

hidden0 :: Monad m => Textual0 m
hidden0 = textual0 $ \ tag cont -> 
    Html.hidden tag cont

hidden :: Monad m => Textual m
hidden = textual $ \ tag cont -> 
    Html.hidden tag cont

textfield :: Monad m => Textual m
textfield = textual $ \ tag cont -> 
    Html.textfield tag ! [ Html.value cont ] 

password :: Monad m => Textual m
password = textual $ \ tag cont ->
    Html.password tag ! [ Html.value cont ] 

textarea :: Monad m => Textual m
textarea = textual $ \ tag cont -> 
    let ls   = lines cont
    in  Html.textarea ( primHtml cont )
	  ! [ Html.name tag 
	    , Html.thestyle "background: lightblue"
	    , Html.cols $ show $ 2 + maximum ( 60 : map length ls )
	    , Html.rows $ show $ 2 + length ls 
	    ] 

-- | attributes see <http://de.selfhtml.org/html/formulare/datei_upload.htm>
file :: Monad m => Textual m
file = textual $ \ tag cont ->
    Html.input 
            ! [ Html.name    tag
	      , Html.thetype "file"
	      , Html.intAttr "maxlength" 10000
	      , Html.strAttr "accept"    "text/*"
	      ]

select :: Monad m
       => [ String ] -- ^ choices
       -> Textual m
select ws def = do
    tag <- gensym
    prev <- look tag
    let cont = fromMaybe def prev
        opts = do
	    w <- ws   
            return $ Html.option (Html.primHtml w) 
		   ! [ Html.selected | w == cont ]
    html $ Html.select ! [ Html.name tag ] << opts
    return prev

radiogroup :: Monad m
           => String 
           -> [ ( String, a ) ]
           -> Form m ( Maybe a )
radiogroup def pairs = do
    open glue
    let ws = map fst pairs
    tag <- gensym
    prev <- look tag
    let cont = fromMaybe def prev
    sequence_ $ do
	w <- ws
        return $ html $ glue
	    [ Html.radio tag w 
                 ! [ Html.strAttr "checked" "checked" | w == cont ] 
	    , Html.primHtml w
	    ]
    close
    return $ do w <- prev ; lookup w pairs

--------------------------------------------------------------------------

selector :: ( Monad m )
       => Int -- ^ index of default
       -> [ ( String, a ) ] -- ^ choices
       -> Form m ( Maybe a )
selector def pairs = 
    selector' ( fst $ pairs !! def ) pairs

-- | first one is default
selector_submit' title opts = do
    open row
    plain title
    mv <- selector 0 opts
    tag <- gensym
    submit "submit"
    Just v <- return mv
    close -- row
    return v

selectors title doptss = do
    open row
    plain title
    open glued -- CHECK
    vs <- sequence $ do
        ( def, opts ) <- doptss
        let sel = case def of
	        Nothing -> 0
		Just d  -> d
        return $ do
            mv <- selector sel opts
            return $ case mv of
	        Just v -> v
	        Nothing -> snd $ opts !! sel
    close -- sub
    close -- row        
    return vs

selector' :: ( Monad m )
       => String -- ^ default
       -> [ ( String, a ) ] -- ^ choices
       -> Form m ( Maybe a )
selector' def pairs = do
    let opts = map fst pairs
        vals = map snd pairs
    mw <- select opts def
    return $ do w <- mw ; lookup w pairs

-----------------------------------------------------------------------

editor :: ( Help a, Reader a, ToDoc a, Monad m )
       => a -- ^ default
       -> Form m ( Maybe a )
editor = editor' True

editor' :: ( Help a , Reader a, ToDoc a, Monad m )
       => Bool -- mzero in case of syntax error?
       -> a -- ^ default
       -> Form m ( Maybe a )
editor' break def = do
    tag <- gensym
    open table
    open row
    mcs <- textarea ( render $ toDoc def )
    close
    open row
    lang <- get_preferred_language
    let helper :: Html.Html
        helper = specialize lang
               $ Autolib.Output.render 
               $ Autolib.Output.Beside 
                 ( Autolib.Output.Doc
		 $ multitext [ (UK, "an expression of type")
			     , (DE, "ein Ausdruck vom Typ")
			     ] )
                 ( help def )
    html helper
    close
    x <- case mcs of
	 Nothing -> return Nothing
         Just cs -> 
	     case parse (parse_complete reader) tag cs of
	         Left  e -> do
                     open row
                     h3 "parse error"
                     close
                     open row
		     pre $ render $ errmsg 80 e cs
                     close
                     if break then mzero else return Nothing
		 Right x -> do
                     -- nicht mit Selbstverständlichkeiten angeben ...
		     -- h3 "parse success"
		     -- pre $ render $ toDoc x
		     return $ Just x
    close
    return x


--------------------------------------------------------------------------

-- TODO: move to separate module (chop Control.CGI into pieces as well)

defaulted_selector name def opts = do
    open row
    plain name
    ms <- selector def opts
    close
    return $ fromMaybe ( snd $ opts !! def ) ms

defaulted_textfield name def = do
    open row
    plain name
    ms <- textfield def
    close -- row
    return $ fromMaybe def ms

defaulted_password name def = do
    open row
    plain name
    ms <- password def
    close -- row
    return $ fromMaybe def ms

defaulted_textarea name def = do
    open row
    plain name
    ms <- textarea def
    close -- row
    return $ fromMaybe def ms

-- | show a few submit buttons
-- remember last choice
-- if no choice (initially), stop
submit_choice opts = do
    memo <- gensym
    let 
        fm = listToFM opts
    mprev <- look memo
    cclicks <- sequence $ do
        ( name, val ) <- opts
	return $ do
	    click <- submit name
	    return [ name | click ]
    let clicks = concat cclicks
	prevs = maybeToList mprev
    name <- case clicks ++ prevs of
        new : _ -> do
            hidden0 memo new
            return new
        _ -> mzero
    Just val <- return $ lookupFM fm name
    return val


selector_submit title def opts = do
    tag <- gensym
    open row
    plain title
    mopt <- selector def opts
    sopt <- submit   "submit"
    close -- row
    case mopt of
	 Nothing -> case opts of
              [( name, opt) ] -> return opt
	      _       -> mzero
	 Just opt -> do
	      when sopt blank
	      return opt



-- | if default is Nothing, then stop here, else continue with default
selector_submit_click :: Monad m
	         => String 
		 -> Maybe String -- ^ possible default
		 -> [(String, a) ] 
		 -> Form m (a, Bool)
selector_submit_click title def opts = do
    open row
    plain title
    mopt <- selector' (fromMaybe "XX" def) opts
    sopt <- submit    "submit"
    close
    case mopt of
	 Nothing -> case def of
	      Nothing -> mzero
	      Just d -> case lookup d opts of
		   Nothing -> mzero
                   Just opt -> return ( opt, False )
	 Just opt -> do
	      when sopt blank
	      return ( opt, sopt )


editor_submit :: ( Help a , ToDoc a, Reader a, Monad m )
	      => String -- ^ title
	      -> a -- ^ default
	      -> Form m a
editor_submit title ex = do
    open row
    plain title
    mconf <- editor' False ex
    sconf <- submit "submit"
    close -- row
    case mconf of
	 Nothing -> do
	     mzero
	     return ex
	 Just conf -> do
             when sconf blank
	     return conf

------------------------------------------------------------------

farbe :: Monad m =>  String -> String -> Form m ()
farbe cs h = html 
	 $ Html.font ( Html.primHtml h )
	 ! [ Html.color cs ]
 
beside l r = do
    open table
    open row
    l
    r
    close
    close

above t b = do
    open table
    open row ; t ; close
    open row ; b ; close
    close
