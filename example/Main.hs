{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (ToJSON, Value)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Req (ReqBodyJson (ReqBodyJson), (=:))
import Slackell.Internal.HTTP ( get, post )
import System.Environment (lookupEnv)


data ChatPostMessagePayload = ChatPostMessagePayload {channel :: String, text :: String }
  deriving (Show, Generic)

instance ToJSON ChatPostMessagePayload

chid :: IO String
chid = lookupEnv "CHID" <&> fromMaybe ""

token :: IO String
token = lookupEnv "TOKEN" <&> fromMaybe ""

msg :: IO String
msg = lookupEnv "MSG" <&> fromMaybe ""

conversationsInfo :: IO Value
conversationsInfo = do
  t <- token
  c <- chid
  get t "conversations.info" (Just ("channel" =: c))

conversationsList :: IO Value
conversationsList = do
  t <- token
  get t "conversations.list" (Just ("types" =: T.pack "private_channel" <> "exclude_archived" =: True))

chatPostMessage :: IO Value
chatPostMessage = do
  t <- token
  m <- msg
  c <- chid
  let r = ReqBodyJson (ChatPostMessagePayload c m)
  post t "chat.postMessage" r Nothing

main :: IO ()
main = do
  print "=== conversions.list ==="
  conversationsList >>= print
  print "=== conversions.info ==="
  conversationsInfo >>= print
  print "=== chat.postMessage ==="
  chatPostMessage >>= print
