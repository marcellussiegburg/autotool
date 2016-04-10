module Import
    ( module Import
    ) where

import Prelude as Import hiding (head, init, last, lines, readFile, sequence, tail, writeFile, foldl, foldr, mapM)
import Yesod as Import
import Yesod.Auth as Import (AuthId, maybeAuthId, requireAuthId)
import Yesod.Form.Bootstrap3 as Import (BootstrapFormLayout (..), BootstrapGridOptions (..), BootstrapSubmit (BootstrapSubmit), bfs, bootstrapSubmit, renderBootstrap3, withAutofocus)
import Yesod.Form.Jquery as Import (jqueryDayField, def)
import Text.Blaze.Html as Import (ToMarkup, preEscapedToHtml)
import Text.Shakespeare.Text as Import (toText)

import Control.Applicative as Import (pure, (<$>), (<*>), (<*), (*>))
import Control.Monad as Import (guard, join, liftM, mplus)
import Control.Monad.Trans.Maybe as Import (MaybeT (MaybeT), runMaybeT)
import Data.Either.Combinators as Import (leftToMaybe, rightToMaybe)
import Data.Foldable as Import (foldl, foldr)
import Data.Function as Import (on)
import Data.List as Import (deleteFirstsBy)
import Data.Maybe as Import (fromMaybe, isJust, listToMaybe, maybeToList)
import Data.Text as Import (Text, lines, pack, unpack)
import Data.Text.Lazy.Builder as Import (toLazyText)
import Data.Time as Import (Day, TimeOfDay, UTCTime (UTCTime), fromGregorian, midnight, timeOfDayToTime, timeToTimeOfDay, utctDay, utctDayTime)
import Data.Traversable as Import (mapM, sequence)
import Data.Tree as Import (Forest, Tree (Node), rootLabel, subForest)

import Foundation as Import 
import Model as Import
import Settings as Import
import Settings.Development as Import
import Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import Data.Monoid as Import (Monoid (mappend, mempty, mconcat), (<>))
#else
import Data.Monoid as Import (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

bfsFormControl :: RenderMessage master msg => msg -> FieldSettings master
bfsFormControl msg = (bfs msg) {fsAttrs = [("class", "form-control")]}
