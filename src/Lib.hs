{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Lib where

import qualified Data.Aeson as Aeson
import Data.Text
import GHC.Generics (Generic)
import qualified Zero.Server as Server

data Person = Person {name :: String, age :: Int}
  deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

myHandler :: Server.Request -> Server.Response
myHandler req =
  Server.stringResponse result
  where
    body =
      Server.requestBody req
    result =
      case Server.decodeJson body of
        Left err -> "Failed to decode request body as a Person. It must be something else"
        Right p -> "Yay! We have a person named: " <> (name p)

run :: IO ()
run =
  Server.startServer
    [ Server.simpleHandler Server.POST "/person" myHandler
    ]
