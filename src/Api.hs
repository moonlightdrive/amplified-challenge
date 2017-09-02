{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Api
-- Description : The RESTful API.
--
-- Provides a RESTful API for GETting all possible
-- representations
-- using the number to letter mapping provided in the "Dialpad" module.
--
-- = Making Requests
-- GET requests must be made to @/@ with query parameter
-- __input__ as an array of integers.
--
-- == Format of the __input__ parameter
-- An array containing the numbers 2 and 5 must be formatted as
-- @input[]=2&input=5@. For Example:
--
-- >>> curl -v -X GET localhost:3000/?input[]=2&input[]=5
-- * Rebuilt URL to: localhost:3000/?input[]=2&input[]=5
-- *   Trying 127.0.0.1...
-- * Connected to localhost (127.0.0.1) port 3000 (#0)
-- > GET /?input=25 HTTP/1.1
-- > Host: localhost:3000
-- > User-Agent: curl/7.50.1
-- > Accept: */*
-- > 
-- < HTTP/1.1 200 OK
-- < Transfer-Encoding: chunked
-- < Date: Sat, 02 Sep 2017 04:39:50 GMT
-- < Server: Warp/3.2.13
-- < Content-Type: application/json;charset=utf-8
-- < 
-- * Connection #0 to host localhost left intact
-- ["aj","ak","al","bj","bk","bl","cj","ck","cl"]
--
-- = Types of Responses
-- [@200 OK@] Returns the result of 'Dialpad.telephoneWords' in the response body as JSON
-- [@400 Bad Request@] Occurs when the __input__ parameter is malformed,
-- i.e. not a valid sequence of phone digits as defined by the "Dialpad" module
module Api (app) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

import Dialpad


type DialpadAPI = QueryParams "input" Int :> Get '[JSON] [String]

errNotPhoneSeq :: ServantErr
errNotPhoneSeq = ServantErr 400 "Query string 'input' must be a sequence of digits" "" []

phone :: [Int] -> Handler [String]
phone input = if isValidSequence input
              then return . telephoneWords $ input
              else throwError errNotPhoneSeq

-- | Used by the executable in 'Main.main' to serve requests
app :: Application
app = serve (Proxy :: Proxy DialpadAPI) server

server :: Server DialpadAPI
server = phone
