module Yesod.Form.Fields.TreeValueField (treeValueField) where

import Control.Monad (Monad)
import Data.Bool (Bool (False), (||))
import Data.Foldable (foldl)
import Data.Either (Either (Right), either)
import Data.Eq ((==))
import Data.Function (id)
import Data.List (null)
import Data.Text (Text)
import Data.Tree (Forest, Tree, subForest, rootLabel)
import System.IO (IO)
import Text.Shakespeare.I18N (RenderMessage)
import Yesod.Core (HandlerSite)
import Yesod.Core.Handler (newIdent)
import Yesod.Core.Widget (WidgetT, whamlet)
import Yesod.Form.Fields (FormMessage)
import Yesod.Form.Functions (parseHelper)
import Yesod.Form.Types (Field (Field, fieldParse, fieldView, fieldEnctype), Enctype (UrlEncoded))

treeValueField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Forest Text -> Field m Text
treeValueField forest = Field {
  fieldParse = parseHelper Right,
  fieldView = \idAttr nameAttr otherAttrs val _ -> do
    [whamlet|
    #{either id id val}
    <div .tree .well ##{idAttr}>
      <ul>
        $forall tree <- forest
          ^{subforest tree nameAttr otherAttrs (either id id val)}
    |],
 fieldEnctype = UrlEncoded
}

subforest :: Tree Text -> Text -> [(Text, Text)] -> Text -> WidgetT site IO ()
subforest tree name' otherAttrs val = do
  inputId <- newIdent
  let containsVal = \b t -> b || (t == val)
  [whamlet|
  $if null (subForest tree)
    <li>
      $if val == rootLabel tree
        <input type="radio" name=#{name'} value=#{rootLabel tree} ##{inputId} *{otherAttrs} checked="">
      $else
        <input type="radio" name=#{name'} value=#{rootLabel tree} ##{inputId} *{otherAttrs}>
      <label for=#{inputId} >
        <i .glyphicon .glyphicon-leaf>
        &nbsp;#{rootLabel tree}
  $else
    <li>
      $if foldl containsVal False tree
        <input type="checkbox" id=#{inputId} checked="">
      $else
        <input type="checkbox" id=#{inputId}>
      <label for=#{inputId}>
        <i .glyphicon .glyphicon-folder-open>
        &nbsp;#{rootLabel tree}
      <ul>
        $forall subforest' <- subForest tree
          ^{subforest subforest' name' otherAttrs val}
  |]
