module RSS where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import Text.Feed.Import (parseFeedSource)
import qualified Text.Feed.Query as Q
import Text.Feed.Types (Feed, Item)
import Types

-- | Fetch an RSS feed from the given URL and return the raw XML.
-- domain: for example "site.com"
-- path: for example "en/rss"
-- together "site.com/en/rss"
getRssFeed :: Text -> Text -> IO LBS.ByteString
getRssFeed domain path = runReq defaultHttpConfig $ do
  r <-
    req
      GET -- method
      (http domain /: path) -- safe by construction URL
      NoReqBody
      lbsResponse -- specify how to interpret response
      mempty -- query params, headers, explicit port number, etc.
  pure $ responseBody r

parseFeed :: LBS.ByteString -> Maybe Feed
parseFeed = parseFeedSource

-- | Splits a URL into (domain, path)
splitUrl :: Text -> (Text, Text)
splitUrl urlStr = do
  let removedPrefixUrl =
        fromMaybe urlStr $
          T.stripPrefix "https://" urlStr <|> T.stripPrefix "http://" urlStr
  let (dom, path) = T.break (== '/') removedPrefixUrl
  (dom, T.tail path)

getAllArticles :: RssFeedList -> [Article]
getAllArticles = concatMap rssFeedArticles

fetchAllFeeds :: Config -> IO RssFeedList
fetchAllFeeds conf = mapM (getFeed . T.pack) $ _feedUrls conf
  where
    getFeed :: Text -> IO RssFeed
    getFeed url = do
      let (domain, path) = splitUrl url
      f <- getRssFeed domain path
      let f' = fromJust $ parseFeed f
      pure
        RssFeed
          { rssFeedUrl = url,
            rssFeedArticles = articlesFromFeed f'
          }

articlesFromFeed :: Feed -> [Article]
articlesFromFeed f = map makeArticle $ Q.feedItems f
  where
    makeArticle :: Item -> Article
    makeArticle i =
      Article
        { articleTitle = fromMaybe "~NO TITLE~" $ Q.getItemTitle i,
          articleUrl = fromMaybe "~NO URL~" $ Q.getItemLink i,
          articleContent = fromMaybe "~NO DESC~" $ Q.getItemDescription i,
          articleRead = False
        }
