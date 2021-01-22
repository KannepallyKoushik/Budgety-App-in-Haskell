{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

module Lib where

import qualified Data.Aeson as Aeson
import Data.Text
import GHC.Generics (Generic)
import qualified Zero.Server as Server

data CashFlow = CashFlow {mode :: String, description :: String, value :: Int}
  deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

data Account = Account [CashFlow]
  deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

data Budget = Budget {budget :: Int, statements :: Account}

-- initialBudget :: Budget
-- initialBudget = Budget ["{\"budget\":\0,\"statements\": []}"]

addCashFlow :: Account -> Server.Request -> (Account, Server.Response)
addCashFlow (Account a) req =
  case Server.decodeJson body of
    Right item ->
      (appendToAccount item, Server.stringResponse "successfull")
    Left err ->
      (Account a, Server.failureResponse $ "Got an error! " ++ show err)
  where
    body = Server.requestBody req
    appendToAccount item =
      Account (a ++ [item])

handleStatement :: Account -> Server.Request -> (Account, Server.Response)
handleStatement state _ =
  (state, response)
  where
    response =
      Server.jsonResponse state

initialState :: Account
initialState = Account []

run :: IO ()
run =
  Server.startServer
    [ Server.handlersWithState
        initialState
        [ Server.statefulHandler Server.POST "/cashflow" addCashFlow,
          Server.statefulHandler Server.GET "/statement" handleStatement
        ]
    ]
