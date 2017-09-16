{-# LANGUAGE OverloadedStrings #-}
module Tweet where

import qualified Data.ByteString as B
import Data.ByteString.Base64
import qualified Data.Text as T
import Data.Text.Encoding

import System.IO

import Web.Authenticate.OAuth
import Network.HTTP.Conduit


postStr :: OAuth -> Credential -> B.ByteString -> IO ()
postStr myOAuth myCred tw = do
  req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
  let postReq = urlEncodedBody [("status", tw)] req
  m <- newManager tlsManagerSettings
  res <- do
    signedreq <- signOAuth myOAuth myCred postReq
    httpLbs signedreq m
  return ()

postWithMedia :: OAuth -> Credential -> B.ByteString -> B.ByteString -> IO ()
postWithMedia myOAuth myCred tw mediaId = do
  req <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
  let postReq = urlEncodedBody [ ("status", tw)
                               , ("media_ids", mediaId)
                               ] req
  m <- newManager tlsManagerSettings
  res <- do
    signedreq <- signOAuth myOAuth myCred postReq
    httpLbs signedreq m
  return ()
