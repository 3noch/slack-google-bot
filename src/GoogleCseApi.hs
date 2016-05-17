module GoogleCseApi where

import Data.Aeson (FromJSON)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Network.HTTP.Client (Manager)
import Servant.API ((:>), QueryParam, Get, JSON)
import Servant.Client (client, BaseUrl(..), ClientM, Scheme(Https))

type GoogleCseApi = "customsearch" :> "v1"
                    :> QueryParam "key" Text
                    :> QueryParam "cx" Text
                    :> QueryParam "q" Text
                    :> Get '[JSON] QueryResult



data QueryResult = QueryResult
  { items :: [ResultItem]
  } deriving (Eq, Generic, Show)

data ResultItem = ResultItem
  { title   :: Text
  , link    :: Text
  , snippet :: Text
  } deriving (Eq, Generic, Show)


instance FromJSON QueryResult
instance FromJSON ResultItem


googleCseBase :: BaseUrl
googleCseBase = BaseUrl Https "www.googleapis.com" 443 "/"

googleCse :: Maybe Text -> Maybe Text -> Maybe Text -> Manager -> BaseUrl -> ClientM QueryResult
googleCse = client (Proxy :: Proxy GoogleCseApi)
