-- | module needed to realize this plan:
-- http://nfa.imn.htwk-leipzig.de/bugzilla/show_bug.cgi?id=382#c3

-- see also https://wiki.haskell.org/GHC/As_a_library
-- http://haddock.stackage.org/nightly-2015-12-10/ghc-7.10.2/Packages.html

import GHC
import GHC.Paths ( libdir )
import DynFlags
import Packages
import Module 

import Text.PrettyPrint.Leijen

main = do
    dflags <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        return dflags
    ps <- readPackageConfigs dflags
    putDoc $ vcat
      [ text "module Package_Translator where"
      , text "import qualified Data.Map.Strict as M"
      , text "package_translator = M.fromList" <+> pretty (map info ps)
      ]

-- | argh, instance Pretty String is broken, have to work around:
newtype S = S String
instance Pretty S where pretty (S s) = dquotes $ string s

info p = ( S $ packageKeyString $ packageKey p
         , S $ packageNameString p
         ) 
