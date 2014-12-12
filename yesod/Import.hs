module Import
    ( module Import
    ) where

import Prelude as Import hiding (head, init, last, readFile, tail, writeFile)
import Yesod as Import
import Yesod.Form.Bootstrap3 as Import (BootstrapFormLayout (..), BootstrapGridOptions (..), BootstrapSubmit (BootstrapSubmit), bfs, bootstrapSubmit, renderBootstrap3, withAutofocus)
import Yesod.Form.Jquery as Import (jqueryDayField, def)
import Text.Shakespeare.Text as Import (toText)

import Control.Applicative as Import (pure, (<$>), (<*>), (<*), (*>))
import Data.Text as Import (Text, pack, unpack)
import Data.Text.Lazy.Builder as Import (toLazyText)
import Data.Time as Import (Day, TimeOfDay, UTCTime (UTCTime), fromGregorian, midnight, timeOfDayToTime)

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
