{-# LANGUAGE OverloadedStrings #-}
module Main where

import Happstack.Server
import Server
import Data.Text (Text)
import System.Log.Logger

main = do
    updateGlobalLogger rootLoggerName $ setLevel DEBUG
    runPonyServer $ ok $ toResponse ("Hello World" :: Text)
