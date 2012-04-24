{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Main where

import Happstack.Server
import Server
import System.Log.Logger
import Text.Hamlet
import Sitemap

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    runPonyServer $ msum [
            dir "favicon.ico" $ serveFile (asContentType "image/x-icon") "./static/favicon.ico"
            , dir "robots.txt" $ serveFile (asContentType "text/plain") "./static/robots.txt"
            , ok $ toResponse $ homeTemplate renderUrl
        ]

homeTemplate = [hamlet|
!!!
<html>
    <head>
        <title>Ponyfolder
    <body>
        <h1>Hello World
|]
