{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (ToJSON (toJSON), Value, object, (.=))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (UTCTime (UTCTime), addDays, fromGregorian, getCurrentTime, secondsToDiffTime, NominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Network.HTTP.Req (ReqBodyJson (ReqBodyJson), (=:))
import Slackell.Internal.HTTP (get, post)
import System.Environment (lookupEnv)

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

data ChatPostMessagePayload = ChatPostMessagePayload {chatPostMessageChannel :: String, text :: String}
  deriving (Show, Generic)

instance ToJSON ChatPostMessagePayload where
  toJSON (ChatPostMessagePayload{chatPostMessageChannel = ch}) =
    object ["channel" .= ch]

chatPostMessage :: IO Value
chatPostMessage = do
  t <- token
  m <- msg
  c <- chid
  let r = ReqBodyJson (ChatPostMessagePayload c m)
  post t "chat.postMessage" r Nothing

data ConversationsHistoryPayload = ConversationsHistoryPayload
  { conversationHistoryChannel :: String
  , conversationsHistoryOldest :: NominalDiffTime
  , conversationsHistoryLatest :: NominalDiffTime
  }
  deriving (Show, Generic)

instance ToJSON ConversationsHistoryPayload where
  toJSON (ConversationsHistoryPayload{conversationHistoryChannel = ch, conversationsHistoryLatest = l, conversationsHistoryOldest = o}) =
    object ["channel" .= ch, "oldest" .= o, "latest" .= l]

conversationsHistory :: IO Value
conversationsHistory = do
  t <- token
  ch <- chid
  let od = fromGregorian 2025 1 10
      ld = addDays 3 od
      o = utcTimeToPOSIXSeconds (UTCTime od (secondsToDiffTime 0))
      l = utcTimeToPOSIXSeconds (UTCTime ld (secondsToDiffTime 0))
      r = ReqBodyJson (ConversationsHistoryPayload ch o l)
  post t "conversations.history" r Nothing

main :: IO ()
main = do
  -- print "=== conversions.list ==="
  -- conversationsList >>= print
  -- print "=== conversions.info ==="
  -- conversationsInfo >>= print
  -- print "=== chat.postMessage ==="
  -- chatPostMessage >>= print
  print "=== conversations.history ==="
  conversationsHistory >>= print
