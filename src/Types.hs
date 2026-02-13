module Types
  ( Config(..)
  , FeedList
  , ArticleList
  , UIBoxes(..)
  , RssFeedList
  , RssFeed(..)
  , Article(..)
  , AppState(..)
  ) where

import Data.Text
import GHC.Generics (Generic)
import Brick.Widgets.List (List)
import Data.Aeson (FromJSON, ToJSON)

data Config = Config
  { urls :: [String]
  } deriving (Show, Generic)
instance FromJSON Config
instance ToJSON Config

type FeedList = List String RssFeed
type ArticleList = List String Article

data UIBoxes = FeedsBox | ArticlesBox | ReadArticleBox
  deriving (Eq, Show)

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

data AppState = AppState
  { _feeds :: FeedList
  , _articles :: ArticleList
  , _selectedArticle :: Article
  , _focusedBox :: UIBoxes
  } deriving (Generic)
