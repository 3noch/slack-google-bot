module SlackBot where

import Control.Monad.IO.Class (MonadIO)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import Web.Slack (Event(Message), SlackBot, SlackConfig(..), runBot)
import Web.Slack.Message (sendMessage)

type SearchEngine = forall io. MonadIO io => Text -> io Text


queryBot :: SearchEngine -> SlackBot ()
queryBot searchEngine (Message cid _ msg _ _ _) = case T.stripPrefix "google" msg of
    Just query -> do
      result <- searchEngine query
      sendMessage cid result
    _ -> return ()

queryBot _ _ = return ()

runSlackBot :: Text -> SearchEngine -> IO ()
runSlackBot apiToken searchEngine =
  runBot (SlackConfig $ toS apiToken) (queryBot searchEngine) ()
