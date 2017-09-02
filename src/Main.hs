-- |
-- Module: Main
-- Description: The main executable
--
-- A REST API that, when given a sequence of phone digits
-- returns all possible words (including the gibberish ones!)
-- that are represented by the mapping
module Main (main) where

import Network.Wai.Handler.Warp(run)

import Api(app)

main :: IO ()
main = run 3000 Api.app
