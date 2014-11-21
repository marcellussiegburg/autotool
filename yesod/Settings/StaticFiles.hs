module Settings.StaticFiles where

import Prelude (IO)
import Yesod.Static
import qualified Yesod.Static as Static
import Settings (staticDir)
import Settings.Development
import Language.Haskell.TH (Q, Exp, Name)
import Data.Default (def)

-- | use this to create your static file serving site
staticSite :: IO Static.Static
staticSite = if development then Static.staticDevel staticDir
                            else Static.static      staticDir

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(staticFiles Settings.staticDir)

combineSettings :: CombineSettings
combineSettings = def

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets' development combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts' development combineSettings
