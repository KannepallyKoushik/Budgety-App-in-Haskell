{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Lib where

-- import GHC.Generics (Generic)
-- import qualified Data.Aeson as Aeson
-- import qualified Zero.Server as Server

-- helloHandler :: Server.Handler
-- helloHandler =
--   Server.simpleHandler Server.GET "/hello" helloCallback

-- data Item = Item {model :: String, quantity :: Int}
--   deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

-- data Cart
--   = Cart [Item]
--   deriving (Eq, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

-- helloCallback :: Server.Request -> Server.Response
-- helloCallback req =
--   Server.stringResponse "Hello World"

-- initialState :: Cart
-- initialState = Cart []

-- addToCartHandler :: Server.Request -> Server.Response
-- addToCartHandler req = Server.jsonResponse (Server.requestBody req)

import qualified Data.Aeson as Aeson
import Data.Text
import GHC.Generics (Generic)
import qualified Zero.Server as Server

data Item = Item {model :: String, quantity :: Int}
  deriving (Eq, Show, Generic)

instance FromJSON Item where
  parseJSON (Object v) = Item <$> v .: "model" <*> v .: "quantity"
  parseJSON _ = empty

instance ToJSON Item where
  toJSON (Item model quantity) = object ["model" .= model, "quantity" .= quantity]

data Cart
  = Cart [Item]
  deriving (Eq, Show, Generic)

addToCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
addToCartHandler (Cart cart) req =
  case Server.decodeJson body of
    Right item ->
      (appendToCart item, Server.stringResponse "ok")
    Left err ->
      (Cart cart, Server.failureResponse $ "Got an error! " ++ show err)
  where
    body =
      Server.requestBody req

    appendToCart item =
      Cart (cart ++ [item]) -- append item at the end of the list

currentCartHandler :: Cart -> Server.Request -> (Cart, Server.Response)
currentCartHandler state _ =
  (state, response)
  where
    response =
      Server.jsonResponse state

initialState :: Cart
initialState = Cart []

run :: IO ()
run =
  Server.startServer
    [ Server.handlersWithState
        initialState
        [ Server.statefulHandler Server.POST "/cart" addToCartHandler,
          Server.statefulHandler Server.GET "/cart" currentCartHandler
        ]
    ]
