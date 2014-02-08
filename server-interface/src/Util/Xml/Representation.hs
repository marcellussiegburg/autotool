module Util.Xml.Representation 

( Document (Document)
, Content (CElem)
, Prolog (Prolog)
, XMLDecl (XMLDecl)
, emptyST, readXml, toContents
, renderDocument_via_BS
, renderDocument_via_Doc
)

where

import Text.XML.HaXml hiding (o, txt)
import Text.XML.HaXml.XmlContent

import qualified Text.XML.HaXml.ByteStringPP 
import qualified Data.ByteString.Lazy.Char8

import qualified Text.XML.HaXml.Pretty
import qualified Text.PrettyPrint as P

renderDocument_via_BS 
    = Data.ByteString.Lazy.Char8.unpack
    . Text.XML.HaXml.ByteStringPP.document 

renderDocument_via_Doc
    = P.renderStyle (P.Style { P.mode=P.LeftMode })
    . Text.XML.HaXml.Pretty.document
