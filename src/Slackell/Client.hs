module Slackell.Client (
  Client (..),
  client
) where

newtype Client = Client
  { clientToken :: String
  }
  deriving (Show, Eq)

client :: String -> Client
client = Client
