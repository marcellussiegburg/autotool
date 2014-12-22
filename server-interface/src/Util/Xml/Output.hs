-- | Utility functions for dealing with the conversion of Output to Xml

module Util.Xml.Output (
    outputToXmlString,
    stringToXmlString,
    xmlStringToOutput,
    outputToXOutput
) where

import qualified Util.Xml.OutputDTD as X
import qualified Autolib.Output as O

import Util.Xml.Representation

-- import Text.PrettyPrint.HughesPJ hiding (style)
import qualified Autolib.Multilingual as M
import qualified Autolib.Multilingual.Html as H
-- import qualified Text.Blaze.Html.Renderer.String
-- import qualified Text.Blaze.Html.Renderer.Utf8

import qualified Autolib.Multilingual.Doc as D
import Data.String ( fromString )

-- import qualified Codec.Binary.Base64 as C
import qualified Data.ByteString.Base64 as BB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.FilePath
import Control.Applicative
import Data.Maybe
import Data.Text (unpack)

import Util.Png

outputToXOutput :: M.Language -> O.Output -> IO X.Output
outputToXOutput lang o = case o of
    O.Empty ->
        return $ X.OBeside $ X.Beside []
    O.Doc doc ->
        outputToXOutput lang $ O.Pre doc
    O.Pre doc ->
        return $ X.OPre $ X.Pre $ D.render_for lang doc
    O.String txt ->
        return $ X.OText $ X.Text txt
    O.Text txt ->
        return $ X.OText $ X.Text $ Data.Text.unpack txt
    O.Image file (O.Hidden contents) -> do
        let ext = drop 1 $ snd $ splitExtension file
        contents' <- contents
        let (w, h) = case ext of
                "png" -> (pngSize contents')
                _     -> (0, 0)
            img = BC.unpack $ BB.encode contents'
        return $ X.OImage $
            X.Image (X.Image_Attrs { X.imageType = ext,
                                     X.imageAlt = "<image>",
                                     X.imageUnit = "px",
                                     X.imageWidth = show w,
                                     X.imageHeight = show h })
                    img
    O.Link uri ->
        outputToXOutput lang (O.Named_Link uri uri)
    O.Named_Link txt uri ->
        return $ X.OLink $ X.Link (X.Link_Attrs { X.linkHref = uri }) txt
        
    O.HRef uri o1 -> do
        -- FIXME
        -- outputToXOutput $ O.Above ( O.Link uri ) o1
        outputToXOutput lang o1
        
    O.Above o1 o2 ->
        X.OAbove . X.Above <$> mapM (outputToXOutput lang) (aboves o1 ++ aboves o2)
    O.Beside o1 o2 ->
        X.OBeside . X.Beside <$> mapM (outputToXOutput lang) (besides o1 ++ besides o2)
    O.Itemize os ->
        X.OItemize . X.Itemize <$> mapM (outputToXOutput lang) os
    O.Nest o' ->
        X.OBeside . X.Beside <$> sequence [return nestSpacing, outputToXOutput lang o']
    O.Figure a b ->
        X.OFigure <$> (X.Figure <$> outputToXOutput lang a <*> outputToXOutput lang b)


xoutputToOutput :: X.Output -> O.Output
xoutputToOutput o = case o of
   X.OPre  (X.Pre  txt) -> O.Pre (D.text txt)
   X.OText (X.Text txt) -> O.String txt
   X.OImage (X.Image _ img) ->
       O.Image (mkData img) (O.Hidden $ return $ BB.decodeLenient $ fromString img)
   X.OLink (X.Link (X.Link_Attrs { X.linkHref = uri }) txt) ->
       O.Named_Link txt uri
   X.OAbove (X.Above []) -> O.Empty
   X.OAbove (X.Above xs) -> foldl1 O.Above $ map xoutputToOutput xs
   X.OBeside (X.Beside []) -> O.Empty

   -- handle special shape that is produce by  outputToXOutput (Nest this)
   X.OBeside (X.Beside [ X.OSpace{}, this ]) -> O.Nest $ xoutputToOutput this

   X.OBeside (X.Beside xs) -> foldl1 O.Beside $ map xoutputToOutput xs
   X.OItemize (X.Itemize xs) -> O.Itemize $ map xoutputToOutput xs
   X.OSpace _ -> O.Empty -- FIXME
   X.OFigure (X.Figure a b) -> O.Figure (xoutputToOutput a) (xoutputToOutput b)


mkData = ("data:image/png;base64," ++)

wrapXOutput :: X.Output -> Document ()
wrapXOutput o = let [CElem e _] = toContents o in
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing)) [] Nothing [])
             emptyST e []

-- FIXME: this should go to Bytestring or Text instead
xmlToString :: Document () -> String
xmlToString = renderDocument_via_Doc 

outputToXmlString ::  M.Language -> O.Output -> IO String
outputToXmlString lang = fmap (xmlToString . wrapXOutput) . outputToXOutput lang

xmlStringToOutput :: String -> O.Output
xmlStringToOutput = xoutputToOutput . either error id . readXml

stringToXmlString :: String -> String
stringToXmlString = xmlToString . wrapXOutput . X.OText . X.Text

-- helpers for outputToXOutput
nestSpacing :: X.Output
nestSpacing = X.OSpace $ X.Space {
    X.spaceWidth = "4",
    X.spaceHeight = "0",
    X.spaceUnit = "em" }

besides :: O.Output -> [O.Output]
besides (O.Beside a b) = besides a ++ besides b
besides a = [a]

aboves :: O.Output -> [O.Output]
aboves (O.Above a b) = aboves a ++ aboves b
aboves a = [a]
