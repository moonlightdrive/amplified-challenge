{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Data.Aeson
import GHC.Generics
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

import Dialpad


type DialpadAPI = "phone" :> QueryParam "input" String :> Get '[JSON] [String]

phone :: Maybe String -> Handler [String]
phone Nothing = undefined
phone (Just input) =
  let possibleWords = digitsFromString input >>= return . telephoneWords
  in case possibleWords of
    Nothing -> undefined
    Just ws -> return ws


app :: Application
app = serve (Proxy :: Proxy DialpadAPI) server

server :: Server DialpadAPI
server = phone

main :: IO ()
main = run 3000 app
