module AppLib.SlackBot where

import Control.Monad.IO.Class (MonadIO)
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import Web.Slack (Event(Message), SlackBot, SlackConfig(..), runBot)
import Web.Slack.Message (sendMessage)

-- | A function that runs in any monad on IO and returns a search result based on a query.
type SearchEngine = forall io. MonadIO io => Text -> io Text

-- | A function that takes a slack message and optionally responds to it.
type Responder = forall io. MonadIO io => Text -> io (Maybe Text)


-- | The official responder which looks for search requests and posts search results.
botResponder :: SearchEngine -> Responder
botResponder searchEngine msg = case T.stripPrefix "google" msg of
    Just query -> Just <$> searchEngine (T.strip query)
    Nothing    -> return Nothing


-- | The Slack bot engine for our app.
queryBot :: Responder -> SlackBot ()
queryBot responder (Message cid _ msg _ _ _) = do
    maybeResult <- responder msg
    case maybeResult of
      Just result -> sendMessage cid result
      Nothing -> return ()
queryBot _ _ = return ()


-- | Connects to Slack and runs the Slack bot.
runSlackBot :: Text      -- ^Slack API token
            -> Responder -- ^Message responding logic
            -> IO ()
runSlackBot apiToken responder =
  runBot (SlackConfig $ toS apiToken) (queryBot responder) ()
