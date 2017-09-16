{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.ByteString.Base64
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Maybe (fromMaybe)

import System.IO
import System.Environment (lookupEnv)

import Web.Authenticate.OAuth

import Tweet


main :: IO ()
main = do
  withFile "./secrets.txt" ReadMode $ \handle -> do
    keys <- hGetContents handle
    tw' <- fromMaybe (error " not set") <$> lookupEnv "WEIGHT"
    md <- fromMaybe (error " not set") <$> lookupEnv "MEDIA_UPLOAD_RES"
    let
      [o1, o2, a1, a2] = map (encodeUtf8 . T.pack) $ take 4 $ lines keys
      myOAuth :: OAuth
      myOAuth = newOAuth
        { oauthServerName     = "api.twitter.com"
        , oauthConsumerKey    = o1
        , oauthConsumerSecret = o2
        }
      myCred :: Credential
      myCred = newCredential accessToken accessTokenSecret
        where
          accessToken       = a1
          accessTokenSecret = a2
      tw = encodeUtf8 $ T.pack $ "#ok_diet\n\n" ++ tw' ++ " kg"
      mediaId = encodeUtf8 $ T.pack $ drop 12 $ take 30 md
    postWithMedia myOAuth myCred tw mediaId
    return ()
