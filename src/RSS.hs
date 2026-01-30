{-# LANGUAGE OverloadedStrings #-}

module RSS (getRssFeed) where

import Network.HTTP.Req

-- getRssFeed :: Text -> IO String
getRssFeed site sitePath = runReq defaultHttpConfig $ do
  r <- req
       GET -- method
       (http site /: sitePath) -- safe by construction URL
       NoReqBody
       bsResponse -- specify how to interpret response
       mempty -- query params, headers, explicit port number, etc.
  let x = show $ responseBody r
  return x
