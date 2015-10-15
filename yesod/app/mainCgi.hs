import Prelude              (IO, (>>=))
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)
import Network.Wai.Handler.CGI (run)

main :: IO ()
main = do
  args <- fromArgs parseExtra
  (app, log) <- makeApplication args
  run app
