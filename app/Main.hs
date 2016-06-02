module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.String.Conv (toS)
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (lookupEnv)

import AppLib.GoogleCseApi (googleCse, googleCseBase)
import AppLib.SearchFormatting (formatResult)
import AppLib.SlackBot (runSlackBot, botResponder)


main :: IO ()
main = do
  slackApiToken    <- requiredEnv "SLACK_API_TOKEN"
  googleCseContext <- requiredEnv "GOOGLE_CSE_CONTEXT"
  googleApiToken   <- requiredEnv "GOOGLE_API_TOKEN"
  http             <- newManager tlsManagerSettings

  let searchEngine :: MonadIO io => Text -> io Text
      searchEngine query = liftIO $ do
        result <- runExceptT $
          googleCse (Just googleApiToken) (Just googleCseContext) (Just query) http googleCseBase
        return $ case result of
          Left err      -> toS $ "Search failed: " ++ show err
          Right success -> formatResult success

  runSlackBot slackApiToken (botResponder searchEngine)


-- | Looks up an environment variable with the given name or errors out.
requiredEnv :: String -> IO Text
requiredEnv env = maybe (error $ "Required environment variable not found: " ++ env) toS
              <$> lookupEnv env
