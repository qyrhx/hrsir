module RSS where

import Network.HTTP.Req
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Types (Feed)

type RssFeedList = [RssFeed]
data RssFeed = RssFeed
  { url :: String
  , elems :: [Article]
  } deriving (Show, Eq)
data Article = Article
  { title :: String
  , content :: String
  , read :: Bool
  } deriving (Show, Eq)

-- | Fetch an RSS feed from the given URL and return the raw XML.
-- domain: for example "site.com"
-- path: for example "en/rss"
-- together "site.com/en/rss"
getRssFeed :: Text -> Text -> IO LBS.ByteString
getRssFeed domain path = runReq defaultHttpConfig $ do
  r <- req
       GET -- method
       (http domain /: path) -- safe by construction URL
       NoReqBody
       lbsResponse -- specify how to interpret response
       mempty -- query params, headers, explicit port number, etc.
  pure $ responseBody r


parseFeed :: LBS.ByteString -> Maybe Feed
parseFeed = parseFeedSource
