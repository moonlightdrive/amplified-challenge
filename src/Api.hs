{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Api (app) where

import Data.Aeson
import GHC.Generics
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

import Dialpad


type DialpadAPI = QueryParam "input" String :> Get '[JSON] [String]

errNoInput, errNotPhoneSeq :: ServantErr
errNoInput = ServantErr 400 "Query string 'input' not found" "" []
errNotPhoneSeq = ServantErr 400 "Query string 'input' must be a sequence of digits" "" []

phone :: Maybe String -> Handler [String]
phone Nothing = throwError errNoInput
phone (Just input) =
  let possibleWords = telephoneWords input
  in case possibleWords of
    Nothing -> throwError errNotPhoneSeq
    Just ws -> return ws

app :: Application
app = serve (Proxy :: Proxy DialpadAPI) server

server :: Server DialpadAPI
server = phone
