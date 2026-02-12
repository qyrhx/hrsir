{-# LANGUAGE OverloadedStrings #-}
module Dummy where

import RSS

dummyArticles :: [Article]
dummyArticles =
  [ Article
    { articleTitle = "Haskell 9.10 Released"
    , articleContent = "Lots of performance improvements and new warnings."
    , articleRead = False
    , articleUrl = "https://site.org/article.html"
    }
  , Article
    { articleTitle = "'C++ is the best language of all time', declares a courageous man."
    , articleContent = "Says he loves to live on the edge"
    , articleRead = False
    , articleUrl = "https://site.org/article.html"
    }
  , Article
    { articleTitle = "Rust Dev Discovers Monads"
    , articleContent = "Claims it is 'just like Option but scary'."
    , articleRead = False
    , articleUrl = "https://site.org/article.html"
    }
  , Article
    { articleTitle = "Programmer Refactors For Fun, Regrets Everything"
    , articleContent = "Spent 6 hours, gained 0 features, but code is now 'clean'."
    , articleRead = True
    , articleUrl = "https://site.org/article.html"
    }
  ]

dummyFeed :: RssFeed
dummyFeed =
  RssFeed
    { rssFeedUrl = "https://example.com/feed.xml"
    , rssFeedArticles = dummyArticles
    }
