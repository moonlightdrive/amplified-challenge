import Network.Wai.Handler.Warp(run)

import Api(app)

main :: IO ()
main = run 3000 Api.app
