module AppLib.SearchFormatting where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import AppLib.GoogleCseApi (ResultItem(..), QueryResult(..))

-- | Embolden text in Slack's primitive syntax.
bold :: Text -> Text
bold x = "*" <> x <> "*"

-- | Format a single result item into Slack-friendly text.
formatResultItem :: ResultItem -> Text
formatResultItem ResultItem{..} = T.unlines [bold title, snippet, link]

-- | Format a whole series of results from Google into Slack-friendly text.
formatResult :: QueryResult -> Text
formatResult QueryResult{..} = T.intercalate "\n" (formatResultItem <$> take 3 items)
