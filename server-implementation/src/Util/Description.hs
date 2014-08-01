-- utility function related to dealing with descriptions

module Util.Description (
    fromDoc,
    fromToDoc,
    fromOutput,
    fromReport,
    help
) where

import Types.Description
import Util.Xml.Output

import qualified Autolib.Output as AO
import qualified Autolib.ToDoc as AT
import qualified Gateway.Help as GH

import Autolib.Reporter.IO.Type
-- import Autolib.Reporter

import Autolib.Multilingual as M

import Data.Typeable

fromToDoc :: AT.ToDoc a => M.Language -> a -> IO Description
fromToDoc lang = fromDoc lang . AT.toDoc

fromDoc :: M.Language -> AT.Doc -> IO Description
fromDoc lang = fromOutput lang . AO.Doc

fromOutput :: M.Language -> AO.Output -> IO Description
fromOutput lang = fmap DString . outputToXmlString lang

fromReport :: M.Language -> Reporter a -> IO Description
fromReport lang rep = 
    do k <- kommentar rep ; fromOutput lang k
    -- fromOutput ( kommentar rep )

help :: (Data.Typeable.Typeable a) 
     => M.Language -> a -> IO Description
help lang = fromOutput lang . GH.help
