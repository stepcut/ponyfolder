{-# LANGUAGE OverloadedStrings #-}
module Main where

import Happstack.Server
import Server
import Data.Text (Text)
import System.Log.Logger

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    runPonyServer simpleRoutes routes
        where
          simpleRoutes :: ServerPart Response
          simpleRoutes =
              msum [ dir "favicon.ico" $ serveFile (asContentType "image/x-icon") "./static/favicon.ico"
                   , dir "robots.txt" $ serveFile (asContentType "text/plain") "./static/robots.txt"
                   , nullDir >> ok (toResponse ("Hello World" :: Text))
                   ]

          routes :: Sitemap -> PonyServerPart Response
          routes Home = ok $ toResponse ("Welcome Home!" :: Text)


