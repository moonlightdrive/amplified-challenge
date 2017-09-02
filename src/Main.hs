-- |
-- Module: Main
-- Description: The main executable.
--
-- Invokes a REST API that, when given a sequence of phone digits,
-- returns all possible words (including the gibberish ones!)
-- that are represented by the mapping
--
-- Requests must be made via port 3000 eg
-- @localhost:3000@
--
-- For more details about Requests and Responses, see "Api"
module Main(main) where
import Network.Wai.Handler.Warp(run)

import Api

main :: IO ()
main = run 3000 Api.app
