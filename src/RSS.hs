{-# LANGUAGE OverloadedStrings #-}

module RSS where

import Config
import Data.Text (Text)
import Data.Maybe (fromJust, fromMaybe)
import Text.Feed.Types (Feed, Item)
import Text.Feed.Import (parseFeedSource)
import Control.Applicative ((<|>))
import Network.HTTP.Req
import qualified Data.Text as T
import qualified Text.Feed.Query as Q
import qualified Data.ByteString.Lazy as LBS

type RssFeedList = [RssFeed]
data RssFeed = RssFeed
  { rssFeedUrl :: Text
  , rssFeedArticles :: [Article]
  } deriving (Show, Eq)
data Article = Article
  { articleTitle :: Text
  , articleUrl :: Text
  , articleContent :: Text
  , articleRead :: Bool
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

-- | Splits a URL into (domain, path)
splitUrl :: Text -> (Text, Text)
splitUrl urlStr = do
  let removedPrefixUrl = fromMaybe urlStr $
        T.stripPrefix "https://" urlStr <|> T.stripPrefix "http://" urlStr
  let (dom, path) = T.break (== '/') removedPrefixUrl
  (dom, T.tail path)

getAllFeeds :: Config -> IO RssFeedList
getAllFeeds conf = mapM getFeed $ feedsUrls conf
  where
    getFeed :: Text -> IO RssFeed
    getFeed url = do
      let (domain, path) = splitUrl url
      f <- getRssFeed domain path
      let f' = fromJust $ parseFeed f
      pure RssFeed {
        rssFeedUrl = url
        , rssFeedArticles = articlesFromFeed f'
        }

articlesFromFeed :: Feed -> [Article]
articlesFromFeed f = map makeArticle $ Q.feedItems f where
  makeArticle :: Item -> Article
  makeArticle i = Article {
    articleTitle = fromMaybe "~NO TITLE~" $ Q.getItemTitle i
    , articleUrl = fromMaybe "~NO URL~" $ Q.getItemLink i
    , articleContent = fromMaybe "~NO DESC~" $ Q.getItemDescription i
    , articleRead = False
    }
