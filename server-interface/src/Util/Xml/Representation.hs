module Util.Xml.Representation 

( Document (Document)
, Content (CElem)
, Prolog (Prolog)
, XMLDecl (XMLDecl)
, emptyST, document, readXml, toContents
, renderDocument
)

where

import Text.XML.HaXml hiding (o, txt)
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.XmlContent

import qualified Text.XML.HaXml.ByteStringPP 
import qualified Data.ByteString.Lazy.Char8

renderDocument 
    = Data.ByteString.Lazy.Char8.unpack
    . Text.XML.HaXml.ByteStringPP.document 

