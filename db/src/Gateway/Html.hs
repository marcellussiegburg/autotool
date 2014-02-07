{-# language NoMonomorphismRestriction #-}
{-# language OverloadedStrings #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module Gateway.Html 

( Html
, primHtml, toHtml
, (!), (<<), (+++)
, HTML (..)
, module Text.Blaze.Html5 
, module Text.Blaze.Html5.Attributes
, module Text.Blaze.Html4.Transitional
, module Text.Blaze.Html4.Transitional.Attributes
, color, thetype, thestyle, thetitle
, strAttr, intAttr
, password, submit, checkbox, radio
, textfield, textarea, hidden
, value, name, rows, cols, selected
, style, anchor, input, header
, concatHtml, renderHtml
)

where

import Text.Blaze.Html ( Html )
import Text.Blaze.Html5 hiding ((!), toHtml, style, input, header)
import Text.Blaze.Html5.Attributes
       hiding ( cite,form,style,summary,title,span,label, hidden, value, name,rows,cols,selected)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html4.Transitional ( font )
import Text.Blaze.Html4.Transitional.Attributes ( border, bgcolor )
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Internal as I
import Data.String
import Control.Monad ( sequence_ )
import qualified Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.String


infixr 2 +++  -- combining Html
infixr 7 <<   -- nesting Html
infixl 8 !    -- adding optional arguments

class HTML a where toHtml :: a -> Html

instance HTML String where toHtml = H.toHtml
instance HTML Html where toHtml = Prelude.id
instance HTML [Html] where toHtml = mapM_ toHtml

concatHtml xs = sequence_ xs

instance Show Html where show = renderHtml

renderHtml = Text.Blaze.Html.Renderer.String.renderHtml

(<<) :: HTML a => (Html -> t) -> a -> t
e << d = e $ toHtml d

-- (!) :: Html -> [H.Attribute] -> Html
d ! attrs = foldl (H.!) d attrs
d +++ e = d >> e
primHtml = toHtml
color s = bgcolor ( fromString s )
thetype s = type_ ( fromString s )
thestyle s = A.style ( fromString s )
strAttr k v = H.customAttribute (fromString k)  $ I.stringValue $      v 
intAttr k v = H.customAttribute (fromString k)  $ I.stringValue $ show v

hidden n v = 
    input H.! type_ "hidden" H.! name n H.! A.id (fromString n) H.! value v
submit n v = 
    input H.! type_ "submit" H.! name n H.! A.id (fromString n) H.! value v
checkbox n v = 
    input H.! type_ "checkbox" H.! name n H.! A.id (fromString n) H.! value v
radio n v = input H.! type_ "radio" 
textfield n = input H.! type_ "text" H.! name n H.! A.id (fromString n)
password n = input H.! type_ "password"
-- textarea = input H.! type_ "textarea"
thetitle = title
value v = A.value $ fromString v
name v = A.name $ fromString v
cols v = A.cols $ fromString v
rows v = A.rows $ fromString v
selected = A.selected "selected"

input = I.customLeaf "input" True
style = I.customParent "style"
anchor = a
header = Text.Blaze.Html5.head
