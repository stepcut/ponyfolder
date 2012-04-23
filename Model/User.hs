{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}
module Model.User where

import Data.Aeson
import Data.Data (Data, Typeable)
import Server
import Data.Text (Text, unpack)
import Data.Map as M (Map, empty)
import Data.Fleece
import Data.Aeson
import Data.Default
import Control.Applicative
import Database.MongoDB hiding (unpack)
import Data.Time

data User = User {
        userEmail :: UserId,
        userName :: Text,
        userPublic :: Bool,
        userNsfw :: Bool,
        userCreated :: UTCTime,
        userMeta :: Map Text Text
    } deriving (Show, Read, Eq, Ord, Data, Typeable)

mkUser email t = User email "" False False t M.empty

userCollection = "users"

instance ToJSON User where
    toJSON User{..} = object [
                             "_id" .= userEmail,
                             "display name" .= userName,
                             "public" .= userPublic,
                             "allow nsfw" .= userNsfw,
                             "created at" .= userCreated,
                             "meta" .= userMeta
                             ]

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "_id" <*> v .: "display name" <*> v .: "public" <*> v .: "allow nsfw" <*> v .: "created at" <*> v .: "meta"
    parseJSON _ = mzero

--userFind :: UserId -> Server User
userFind email = msum [
    doc <- runDB . findOne $ thisUser email
    case mdoc of
        Nothing -> do
            now <- liftIO getCurrentTime
            let user = mkUser email now
            runDB $ insert_ userCollection $ asDocument user
            return user
        Just u -> cast $ val u

--currentUser :: Server User
currentUser = userFind =<< authenticated

thisUser :: Select a => UserId -> a
thisUser email = select ["_id" =: unpack email] userCollection
