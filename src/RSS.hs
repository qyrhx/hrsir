{-# LANGUAGE DataKinds #-}

module RSS
  ( articlesFromFeed,
    fetchAllFeeds,
    fetchFeed,
    getAllArticles,
    getRssFeed,
    parseFeed,
  )
where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import Text.Feed.Import (parseFeedSource)
import qualified Text.Feed.Query as Q
import Text.Feed.Types (Feed, Item)
import qualified Text.URI as URI
import Types

fetchFeed :: Text -> IO RssFeed
fetchFeed url = do
  rawXml <- fetchUrlByteString url
  case parseFeed rawXml of
    Just f ->
      pure
        RssFeed
          { rssFeedUrl = url,
            rssFeedArticles = articlesFromFeed f
          }
    Nothing -> error $ "Failed to parse feed XML from: " <> T.unpack url

fetchUrlByteString :: Text -> IO LBS.ByteString
fetchUrlByteString urlStr = runReq defaultHttpConfig $ do
  uri <- URI.mkURI urlStr
  case useHttpsURI uri of
    Just (url, opts) -> responseBody <$> req GET url NoReqBody lbsResponse opts
    Nothing -> case useHttpURI uri of
      Just (url, opts) -> responseBody <$> req GET url NoReqBody lbsResponse opts
      Nothing -> error $ "Invalid or unsupported HTTP(S) URL: " <> T.unpack urlStr

getRssFeed :: Text -> Text -> IO LBS.ByteString
getRssFeed domain path = fetchUrlByteString ("https://" <> domain <> "/" <> path)

parseFeed :: LBS.ByteString -> Maybe Feed
parseFeed = parseFeedSource

getAllArticles :: RssFeedList -> [Article]
getAllArticles = concatMap rssFeedArticles

fetchAllFeeds :: Config -> IO RssFeedList
fetchAllFeeds conf = mapM (fetchFeed . T.pack) $ _feedUrls conf

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
