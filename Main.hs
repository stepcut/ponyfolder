{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Main where

import Happstack.Server
import Server
import System.Log.Logger
import Text.Hamlet
import Sitemap
import Web.Routes

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    runPonyServer simpleRoutes routes
        where
          simpleRoutes :: ServerPart Response
          simpleRoutes =
              msum [ dir "favicon.ico" $ serveFile (asContentType "image/x-icon") "./static/favicon.ico"
                   , dir "robots.txt" $ serveFile (asContentType "text/plain") "./static/robots.txt"
                   , nullDir >> (ok $ toResponse $ homeTemplate renderUrl)
                   ]

          routes :: Sitemap -> PonyServerPart Response
          routes Home =
              do showFn <- askRouteFn
                 let showFn' u = showFn u []
                 ok $ toResponse $ homeTemplate showFn'

homeTemplate = [hamlet|
!!!
<html>
    <head>
        <title>Ponyfolder
    <body>
        <h1>Hello World
|]

