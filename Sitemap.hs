{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable #-}
module Sitemap where

import Data.Data (Data, Typeable)
import Web.Routes.TH

data Sitemap
    = Home
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(derivePathInfo ''Sitemap)

renderUrl :: Sitemap -> String
renderUrl Home = "/"
