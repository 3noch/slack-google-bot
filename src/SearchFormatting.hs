module SearchFormatting where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import GoogleCseApi (ResultItem(..), QueryResult(..))

bold :: Text -> Text
bold x = "*" <> x <> "*"

formatResultItem :: ResultItem -> Text
formatResultItem ResultItem{..} = T.unlines [bold title, snippet, link]

formatResult :: QueryResult -> Text
formatResult QueryResult{..} = T.intercalate "\n" (formatResultItem <$> take 3 items)
