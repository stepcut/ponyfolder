{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}
module Sitemap where

import Data.Data (Data, Typeable)

data Sitemap
    = Home
    deriving (Eq, Ord, Read, Show, Data, Typeable)

renderUrl :: Sitemap -> String
renderUrl Home = "/"
