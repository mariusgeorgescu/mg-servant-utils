module MGServantUtils.Handlers where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.Char8 as BL8
import Servant

-- | Generic exception handler for Servant handlers
-- Converts any exception to a 400 Bad Request response
handleException :: (Exception e, Show e) => e -> Servant.Handler a
handleException e = do
  liftIO $ putStrLn $ "Exception: \n" <> show e
  throwError err400 {errBody = BL8.pack (show e)}

