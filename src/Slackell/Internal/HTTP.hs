{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Slackell.Internal.HTTP (
  get,
  post,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network.HTTP.Req (
  GET (GET),
  HttpBody,
  HttpBodyAllowed,
  HttpMethod (AllowsBody),
  JsonResponse,
  MonadHttp,
  NoReqBody (NoReqBody),
  Option,
  POST (POST),
  ProvidesBody,
  Req,
  Scheme (Https),
  Url,
  defaultHttpConfig,
  header,
  https,
  jsonResponse,
  req,
  responseBody,
  runReq,
  (/:),
 )

perform :: (FromJSON a, MonadIO m) => Req (JsonResponse a) -> m a
perform r = runReq defaultHttpConfig $ responseBody <$> r

get :: (FromJSON a, MonadIO m) => String -> String -> Maybe (Option 'Https) -> m a
get auth endpoint opt = perform $ getReq auth endpoint opt

post :: (FromJSON a, MonadIO m, HttpBody body) => String -> String -> body -> Maybe (Option 'Https) -> m a
post auth endpoint body opt = perform $ postReq auth endpoint body opt

authHeader :: String -> Option scheme
authHeader a = header "Authorization" ("Bearer " <> B.pack a)

buildApiUrl :: String -> Url Https
buildApiUrl endpoint = https "slack.com" /: "api" /: T.pack endpoint
request :: (FromJSON a, MonadHttp m, HttpBody body, HttpMethod method, HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) => String -> String -> method -> body -> Maybe (Option Https) -> m (JsonResponse a)
request auth endpoint method body opt = req method (buildApiUrl endpoint) body jsonResponse (authHeader auth <> fromMaybe mempty opt)

getReq :: (FromJSON a, MonadHttp m) => String -> String -> Maybe (Option Https) -> m (JsonResponse a)
getReq auth endpoint = request auth endpoint GET NoReqBody

postReq :: (FromJSON a, MonadHttp m, HttpBody body) => String -> String -> body -> Maybe (Option Https) -> m (JsonResponse a)
postReq auth endpoint = request auth endpoint POST
