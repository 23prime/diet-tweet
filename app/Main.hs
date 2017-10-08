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
  withFile "./mezase65kg.txt" ReadMode $ \handle -> do
    withFile "../csv-graph/weight.csv" ReadMode $ \csv -> do
      keys    <- hGetContents handle
      weights <- hGetContents csv
      weight  <- fromMaybe (error " not set") <$> lookupEnv "WEIGHT"
      md      <- fromMaybe (error " not set") <$> lookupEnv "MEDIA_UPLOAD_RES"
      let [oauthConsumerKey, oauthConsumerSecret, accessToken, accessTokenSecret]
            = map (encodeUtf8 . T.pack) $ take 4 $ lines keys
          myOAuth :: OAuth
          myOAuth = newOAuth
                    { oauthServerName     = "api.twitter.com"
                    , oauthConsumerKey    = oauthConsumerKey
                    , oauthConsumerSecret = oauthConsumerSecret
                    }
          myCred :: Credential
          myCred = newCredential accessToken accessTokenSecret
      let yesterdaysWeight = read $ killPoint $ drop 6 $ last $ init $ lines weights
                             :: Int
          from1stDay    = "初日比 " ++ fromOtherDay weight 749 ++ " kg"
          fromYesterday = "前日比 " ++ fromOtherDay weight yesterdaysWeight ++ " kg"
          today         = weight ++ " kg"
          tw = encodeUtf8 $ T.pack
               $ "#ok_diet\n\n" ++ today ++ "\n"
               ++ from1stDay ++ "\n"
               ++ fromYesterday ++ "\n"
          mediaId = encodeUtf8 $ T.pack $ drop 12 $ take 30 md
      postWithMedia myOAuth myCred tw mediaId
      return ()
    return()
  return()

-- 小数点をふっ飛ばす
killPoint :: String -> String
killPoint = filter (/= '.')

-- 小数点を戻す
addPoint :: String -> String
addPoint x = i ++ '.' : l
  where
    (i, l) = splitAt (length x - 1) x

-- 他の日との差を取る
fromOtherDay :: String -> Int -> String
fromOtherDay today otherDay
  | head dif == '-' = dif
  | otherwise       = '+' : dif
    -- 符号±の付加
  where
    (h : dif') = addPoint $ show $ (read $ killPoint today :: Int) - otherDay
    -- 数字フォーマットの整形
    dif
      | length dif' > 2 = h : dif'
      | h == '.'  = '0' : h : dif'
      | h == '-'  = h : '0' : dif'
      | otherwise = h : dif'
