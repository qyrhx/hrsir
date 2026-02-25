module Types where

import Brick.Widgets.List (List)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro.TH (makeLenses)

data Config = Config
  { _feedUrls :: [String]
  }
  deriving (Show, Generic)

instance FromJSON Config

instance ToJSON Config

type FeedList = List String RssFeed

type ArticleList = List String Article

data UIBoxes = FeedsBox | ArticlesBox | ReadArticleBox
  deriving (Eq, Show)

type RssFeedList = [RssFeed]

data RssFeed = RssFeed
  { rssFeedUrl :: Text,
    rssFeedArticles :: [Article]
  }
  deriving (Show, Eq)

data Article = Article
  { articleTitle :: Text,
    articleUrl :: Text,
    articleContent :: Text,
    articleRead :: Bool
  }
  deriving (Show, Eq)

data CMDData = None | Message Text | Err Text | Input Text

data AppMode = Normal | Command

data AppState = AppState
  { _mode :: AppMode,
    _feeds :: FeedList,
    _articles :: ArticleList,
    _selectedArticle :: Article,
    _focusedBox :: UIBoxes,
    _cmd :: CMDData,
    _config :: Config
  }
  deriving (Generic)

makeLenses ''Config
makeLenses ''AppState
